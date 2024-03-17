# Utility convert a binary file into an intcode object file

# Output file format:
#
# <name>_section_count:
#       db  <count>                                         # Number of sections
#
# <name>_section_0_address:
#       db  <size0>                                         # Section 0 load address
# <name>_section_0_start:
#       db  <size0>                                         # Section 0 starting index
# <name>_section_0_size:
#       db  <size0>                                         # Section 0 size in bytes
# <name>_section_1_address:
#       db  <size0>                                         # Section 1 load address
# <name>_section_1_start:
#       db  <size0>                                         # Section 1 starting index
# <name>_section_1_size:
#       db  <size1>                                         # Section 1 size in bytes
# ...
# <name>_section_<count-1>_address:
#       db  <sizeX>                                         # Section <count-1> load address
# <name>_section_<count-1>_start:
#       db  <sizeX>                                         # Section <count-1> starting index
# <name>_section_<count-1>_size:
#       db  <sizeX>                                         # Section <count-1> size in bytes
#
# <name>_section_0_data:
#       db  <byte_0>, <byte_1>, ... <byte_size-1>           # Section 0 data, <size0> bytes
# <name>_section_1_data:
#       db  <byte_0>, <byte_1>, ... <byte_size-1>           # Section 1 data, <size0> bytes
# ...
# <name>_section_<count-1>_data:
#       db  <byte_0>, <byte_1>, ... <byte_size-1>           # Section <count-1> data, <sizeX> bytes

# Input format:
#
# Since Intcode has no way of detecting end of file or inspecting the file system, this utility needs to receive
# the binary name and the file size as its input. To make that easy, the utility expects to receive this in
# the format that is conveniently generated by the standard wc utility ('wc -c <path>').
#
# The Makefile rule to run bin2obj could look like this:
# define run-bin2obj
# 	wc -c $< | cat - $< | $(ICVM) ./bin/bin2obj.input > $@ || ( cat $@ ; false )
# endef

.IMPORT print_num
.IMPORT print_str
.IMPORT is_alphanum
.IMPORT __heap_start

.SYMBOL MAX_NAME_LENGTH                 30
.SYMBOL MAX_ZERO_COUNT                  64

.SYMBOL SECTION_ADDRESS                 0
.SYMBOL SECTION_START                   1
.SYMBOL SECTION_SIZE                    2
.SYMBOL SECTION_RECORD_SIZE             3

##########
# entry point
    arb stack
    call main

    hlt

##########
    ds  50, 0
stack:

##########
main:
.FRAME
    call parse_wc
    call load_data
    call detect_sections
    call output_object

    ret 0
.ENDFRAME

##########
parse_wc:
.FRAME
    # The format is:
    # - zero or more spaces
    # - one or more decimal digits, binary size
    # - one or more spaces
    # - unix path, possibly including directories
    # - line end, which could be either 0x0a or 0x0d 0x0a

    # Functions below expect the next character to be already in [char]
    in  [char]

    # Read zero or more spaces
    call read_spaces

    # Read the size + one space after
    call read_size

    # Read zero or more spaces
    call read_spaces

    # Read binary name from the unix path
    call read_name

    ret 0
.ENDFRAME

##########
read_spaces:
.FRAME tmp
    arb -1

read_spaces_loop:
    eq  [char], ' ', [rb + tmp]
    jz  [rb + tmp], read_spaces_done

    in  [char]
    jz  0, read_spaces_loop

read_spaces_done:

    arb 1
    ret 0
.ENDFRAME

##########
read_size:
.FRAME tmp
    arb -1

    add 0, 0, [data_size]

read_size_loop:
    eq  [char], ' ', [rb + tmp]
    jnz [rb + tmp], read_size_done
    lt  [char], '0', [rb + tmp]
    jnz [rb + tmp], read_size_invalid
    lt  '9', [char], [rb + tmp]
    jnz [rb + tmp], read_size_invalid

    add [char], -'0', [char]
    mul [data_size], 10, [data_size]
    add [data_size], [char], [data_size]

    in  [char]
    jz  0, read_size_loop

read_size_done:
    arb 1
    ret 0

read_size_invalid:
    add read_size_invalid_message, 0, [rb - 1]
    arb -1
    call print_str

    hlt

read_size_invalid_message:
    db  "invalid file size", 10, 0
.ENDFRAME

##########
read_name:
.FRAME is_ext, tmp
    arb -2

    add [free_memory], 0, [name_addr]
    add 0, 0, [name_length]
    add 0, 0, [rb + is_ext]

    # We want the file name without extension:
    # Read in characters one by one into the name buffer
    # When we see a dot, we start ignoring the input until the end, or until a reset
    # When we see a non-alphanumeric character, we reset the name and start again

read_name_loop:
    # Handle 0xa as the end of input, ignore 0xd
    eq  [char], 0xa, [rb + tmp]
    jnz [rb + tmp], read_name_done
    eq  [char], 0xd, [rb + tmp]
    jnz [rb + tmp], read_name_loop_end

    # Handle '.' as start of an extension
    eq  [char], '.', [rb + tmp]
    jnz [rb + tmp], read_name_ext

    # Alphanumeric characters could be the name
    add [char], 0, [rb - 1]
    arb -1
    call is_alphanum
    jnz [rb - 3], read_name_alnum

    # Anything else means this wasn't the name after all, reset
    add [free_memory], 0, [name_addr]
    add 0, 0, [name_length]
    add 0, 0, [rb + is_ext]

    jz  0, read_name_loop_end

read_name_ext:
    add 1, 0, [rb + is_ext]
    jz  0, read_name_loop_end

read_name_alnum:
    # Ignore everythig after a dot
    jnz [rb + is_ext], read_name_loop_end

    # Save the character
    add [name_addr], [name_length], [ip + 3]
    add [char], 0, [0]
    add [name_length], 1, [name_length]

read_name_loop_end:
    in  [char]
    jz  0, read_name_loop

read_name_done:
    jz  [name_length], read_name_invalid

    lt  MAX_NAME_LENGTH, [name_length], [rb + tmp]
    jnz [rb + tmp], read_name_too_long

    # Zero terminate the string
    add [name_addr], [name_length], [ip + 3]
    add 0, 0, [0]

    # Mark the buffer as used memory
    add [free_memory], [name_length], [free_memory]
    add [free_memory], 1, [free_memory]

    arb 2
    ret 0

read_name_invalid:
    add read_name_invalid_message, 0, [rb - 1]
    arb -1
    call print_str

    hlt

read_name_too_long:
    add read_name_too_long_message, 0, [rb - 1]
    arb -1
    call print_str

    hlt

read_name_invalid_message:
    db  "invalid binary name", 10, 0
read_name_too_long_message:
    db  "binary name too long", 10, 0
.ENDFRAME

##########
load_data:
.FRAME index, tmp
    arb -2

    add [free_memory], 0, [data_addr]
    add 0, 0, [rb + index]

load_data_loop:
    eq  [rb + index], [data_size], [rb + tmp]
    jnz [rb + tmp], load_data_done

    in  [rb + tmp]
    add [data_addr], [rb + index], [ip + 3]
    add [rb + tmp], 0, [0]

    add [rb + index], 1, [rb + index]
    jz  0, load_data_loop

load_data_done:
    # Mark the buffer as used memory
    add [free_memory], [data_size], [free_memory]

    arb 2
    ret 0
.ENDFRAME

##########
detect_sections:
.FRAME data_index, byte_index, section_size, zero_count, byte, tmp
    arb -6

    add [free_memory], 0, [section_addr]

    add 0, 0, [section_count]
    add 0, 0, [rb + section_size]
    add 0, 0, [rb + zero_count]

    # Loop through the data, detect large blocks of zeros and skip them
    add 0, 0, [rb + data_index]
    add 0, 0, [rb + byte_index]

detect_sections_loop:
    eq  [rb + data_index], [data_size], [rb + tmp]
    jnz [rb + tmp], detect_sections_finish_last_section

    # Read next byte
    add [data_addr], [rb + data_index], [ip + 1]
    add [0], 0, [rb + byte]

    # Detect zeros
    jnz [rb + byte], detect_sections_nonzero

    # Count zeros, don't output them yet
    add [rb + zero_count], 1, [rb + zero_count]
    jz  0, detect_sections_loop_end

detect_sections_nonzero:
    # Was this a long run of zeros?
    lt  MAX_ZERO_COUNT, [rb + zero_count], [rb + tmp]
    jnz [rb + tmp], detect_sections_finish_section

    # No, do we have a current section we can continue?
    jz  [section_count], detect_sections_start_section

    # Yes, continue the section
    jz  0, detect_sections_continue_section

detect_sections_finish_section:
    # Finish current section if any
    jz  [section_count], detect_sections_start_section

    # section_addr[section_count - 1].SECTION_SIZE = [rb + section_size]
    add [section_count], -1, [rb + tmp]
    mul [rb + tmp], SECTION_RECORD_SIZE, [rb + tmp]
    add [rb + tmp], [section_addr], [rb + tmp]
    add [rb + tmp], SECTION_SIZE, [ip + 3]
    add [rb + section_size], 0, [0]

    add [rb + byte_index], [rb + section_size], [rb + byte_index]

detect_sections_start_section:
    # Start a new section
    # section_addr[section_count].SECTION_ADDRESS = [rb + data_index]
    # section_addr[section_count].SECTION_START = [rb + byte_index]
    mul [section_count], SECTION_RECORD_SIZE, [rb + tmp]
    add [rb + tmp], [section_addr], [rb + tmp]
    add [rb + tmp], SECTION_ADDRESS, [ip + 3]
    add [rb + data_index], 0, [0]
    add [rb + tmp], SECTION_START, [ip + 3]
    add [rb + byte_index], 0, [0]

    add [section_count], 1, [section_count]
    add 0, 0, [rb + section_size]
    add 0, 0, [rb + zero_count]

detect_sections_continue_section:
    # Was there any run of zeros at all?
    jz  [rb + zero_count], detect_sections_after_zeros

    # Yes, output the zero run as regular data, it wasn't long enough
    add [rb + section_size], [rb + zero_count], [rb + section_size]
    add 0, 0, [rb + zero_count]

detect_sections_after_zeros:
    # Current byte belongs to this section
    add [rb + section_size], 1, [rb + section_size]

detect_sections_loop_end:
    # Process next byte
    add [rb + data_index], 1, [rb + data_index]
    jz  0, detect_sections_loop

detect_sections_finish_last_section:
    # Is there a section to finish?
    jz  [section_count], detect_sections_done

    # Finish last section if by writing section size
    add [section_count], -1, [rb + tmp]
    mul [rb + tmp], SECTION_RECORD_SIZE, [rb + tmp]
    add [rb + tmp], [section_addr], [rb + tmp]
    add [rb + tmp], SECTION_SIZE, [ip + 3]
    add [rb + section_size], 0, [0]

detect_sections_done:
    # Mark section data as used memory
    mul [section_count], SECTION_RECORD_SIZE, [rb + tmp]
    add [free_memory], [rb + tmp], [free_memory]

    arb 6
    ret 0
.ENDFRAME

##########
output_object:
.FRAME
    # Output start of the object file
    add output_object_start, 0, [rb - 1]
    arb -1
    call print_str

    call output_header
    call output_data

    # Output the middle part of the object file
    add output_object_middle, 0, [rb - 1]
    arb -1
    call print_str

    call output_exports

    ret 0

output_object_start:
    db ".C", 10, 0
output_object_middle:
    db  10, ".R", 10, ".I", 10, ".E", 10, 0
.ENDFRAME

##########
output_header:
.FRAME section_index, section_address, section_start, section_size, tmp
    arb -5

    # Output section count
    add [section_count], 0, [rb - 1]
    arb -1
    call print_num

    # Loop all sections
    add 0, 0, [rb + section_index]

output_header_sections_loop:
    eq  [rb + section_index], [section_count], [rb + tmp]
    jnz [rb + tmp], output_header_sections_done

    # Load current section data to local variables
    mul [rb + section_index], SECTION_RECORD_SIZE, [rb + tmp]
    add [rb + tmp], [section_addr], [rb + tmp]

    add [rb + tmp], SECTION_ADDRESS, [ip + 1]
    add [0], 0, [rb + section_address]
    add [rb + tmp], SECTION_START, [ip + 1]
    add [0], 0, [rb + section_start]
    add [rb + tmp], SECTION_SIZE, [ip + 1]
    add [0], 0, [rb + section_size]

    # Output current section address, start index and size
    out ','
    add [rb + section_address], 0, [rb - 1]
    arb -1
    call print_num

    out ','
    add [rb + section_start], 0, [rb - 1]
    arb -1
    call print_num

    out ','
    add [rb + section_size], 0, [rb - 1]
    arb -1
    call print_num

    # Next section
    add [rb + section_index], 1, [rb + section_index]
    jz  0, output_header_sections_loop

output_header_sections_done:
    arb 5
    ret 0
.ENDFRAME

##########
output_data:
.FRAME section_index, section_address, section_size, data_index, data_limit, tmp
    arb -6

    # Loop all sections
    add 0, 0, [rb + section_index]

output_data_sections_loop:
    eq  [rb + section_index], [section_count], [rb + tmp]
    jnz [rb + tmp], output_data_sections_done

    # Load current section data to local variables
    mul [rb + section_index], SECTION_RECORD_SIZE, [rb + tmp]
    add [rb + tmp], [section_addr], [rb + tmp]

    add [rb + tmp], SECTION_ADDRESS, [ip + 1]
    add [0], 0, [rb + section_address]
    add [rb + tmp], SECTION_SIZE, [ip + 1]
    add [0], 0, [rb + section_size]

    # Calculate which data belongs to current section
    add [rb + section_address], 0, [rb + data_index]
    add [rb + section_address], [rb + section_size], [rb + data_limit]

output_data_bytes_loop:
    eq  [rb + data_index], [rb + data_limit], [rb + tmp]
    jnz [rb + tmp], output_data_bytes_done

    # Output next byte
    out ','
    add [data_addr], [rb + data_index], [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_num

    add [rb + data_index], 1, [rb + data_index]
    jz  0, output_data_bytes_loop

output_data_bytes_done:
    # Next section
    add [rb + section_index], 1, [rb + section_index]
    jz  0, output_data_sections_loop

output_data_sections_done:
    arb 6
    ret 0
.ENDFRAME

##########
output_exports:
.FRAME
    # Section count
    add [name_addr], 0, [rb - 1]
    arb -1
    call print_str

    add output_exports_section_count, 0, [rb - 1]
    arb -1
    call print_str

    # Section count is always at index 0
    out '0'
    out 10

    # Section header
    add [name_addr], 0, [rb - 1]
    arb -1
    call print_str

    add output_exports_header, 0, [rb - 1]
    arb -1
    call print_str

    # Section header is always at index 1
    out '1'
    out 10

    # Section data
    add [name_addr], 0, [rb - 1]
    arb -1
    call print_str

    add output_exports_data, 0, [rb - 1]
    arb -1
    call print_str

    # Section data starts after the header
    mul [section_count], SECTION_RECORD_SIZE, [rb - 1]
    add [rb - 1], 1, [rb - 1]
    arb -1
    call print_num

    out 10

    ret 0

output_exports_section_count:
    db  "_section_count:", 0
output_exports_header:
    db  "_header:", 0
output_exports_data:
    db  "_data:", 0
.ENDFRAME

##########
# globals
char:
    db  0

free_memory:
    db  __heap_start

name_addr:
    db  0
name_length:
    db  0

data_addr:
    db  0
data_size:
    db  0

section_addr:
    db  0
section_count:
    db  0

.EOF
