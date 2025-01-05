# Utility convert a binary file into an intcode object file

# Output file format:
#
# <name>_image:
#
# <name>_size:
#       db  <size>                                          # Image size
# <name>_count:
#       db  <count>                                         # Number of sections
#
# <name>_header:
#       db  <address0>                                      # Section 0 load address
#       db  <start0>                                        # Section 0 starting index
#       db  <size0>                                         # Section 0 size in bytes
#       db  <address1>                                      # Section 1 load address
#       db  <start1>                                        # Section 1 starting index
#       db  <size1>                                         # Section 1 size in bytes
# ...
#       db  <addressX>                                      # Section <count-1> load address
#       db  <startX>                                        # Section <count-1> starting index
#       db  <sizeX>                                         # Section <count-1> size in bytes
#
# <name>_data:
#       db  <byte_0>, <byte_1>, ... <byte_size-1>           # Section 0 data, <size0> bytes
#       db  <byte_0>, <byte_1>, ... <byte_size-1>           # Section 1 data, <size1> bytes
# ...
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
    ds  1000, 0
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

.loop:
    eq  [char], ' ', [rb + tmp]
    jz  [rb + tmp], .done

    in  [char]
    jz  0, .loop

.done:

    arb 1
    ret 0
.ENDFRAME

##########
read_size:
.FRAME tmp
    arb -1

    add 0, 0, [data_size]

.loop:
    eq  [char], ' ', [rb + tmp]
    jnz [rb + tmp], .done
    lt  [char], '0', [rb + tmp]
    jnz [rb + tmp], .invalid
    lt  '9', [char], [rb + tmp]
    jnz [rb + tmp], .invalid

    add [char], -'0', [char]
    mul [data_size], 10, [data_size]
    add [data_size], [char], [data_size]

    in  [char]
    jz  0, .loop

.done:
    arb 1
    ret 0

.invalid:
    add .invalid_message, 0, [rb - 1]
    arb -1
    call print_str

    hlt

.invalid_message:
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

.loop:
    # Handle 0xa as the end of input, ignore 0xd
    eq  [char], 0xa, [rb + tmp]
    jnz [rb + tmp], .done
    eq  [char], 0xd, [rb + tmp]
    jnz [rb + tmp], .loop_end

    # Handle '.' as start of an extension
    eq  [char], '.', [rb + tmp]
    jnz [rb + tmp], .ext

    # Alphanumeric characters could be the name
    add [char], 0, [rb - 1]
    arb -1
    call is_alphanum
    jnz [rb - 3], .alnum

    # Anything else means this wasn't the name after all, reset
    add [free_memory], 0, [name_addr]
    add 0, 0, [name_length]
    add 0, 0, [rb + is_ext]

    jz  0, .loop_end

.ext:
    add 1, 0, [rb + is_ext]
    jz  0, .loop_end

.alnum:
    # Ignore everythig after a dot
    jnz [rb + is_ext], .loop_end

    # Save the character
    add [name_addr], [name_length], [ip + 3]
    add [char], 0, [0]
    add [name_length], 1, [name_length]

.loop_end:
    in  [char]
    jz  0, .loop

.done:
    jz  [name_length], .invalid

    lt  MAX_NAME_LENGTH, [name_length], [rb + tmp]
    jnz [rb + tmp], .too_long

    # Zero terminate the string
    add [name_addr], [name_length], [ip + 3]
    add 0, 0, [0]

    # Mark the buffer as used memory
    add [free_memory], [name_length], [free_memory]
    add [free_memory], 1, [free_memory]

    arb 2
    ret 0

.invalid:
    add .invalid_message, 0, [rb - 1]
    arb -1
    call print_str

    hlt

.too_long:
    add .too_long_message, 0, [rb - 1]
    arb -1
    call print_str

    hlt

.invalid_message:
    db  "invalid binary name", 10, 0
.too_long_message:
    db  "binary name too long", 10, 0
.ENDFRAME

##########
load_data:
.FRAME index, tmp
    arb -2

    add [free_memory], 0, [data_addr]
    add 0, 0, [rb + index]

.loop:
    eq  [rb + index], [data_size], [rb + tmp]
    jnz [rb + tmp], .done

    in  [rb + tmp]
    add [data_addr], [rb + index], [ip + 3]
    add [rb + tmp], 0, [0]

    add [rb + index], 1, [rb + index]
    jz  0, .loop

.done:
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

.loop:
    eq  [rb + data_index], [data_size], [rb + tmp]
    jnz [rb + tmp], .finish_last_section

    # Read next byte
    add [data_addr], [rb + data_index], [ip + 1]
    add [0], 0, [rb + byte]

    # Detect zeros
    jnz [rb + byte], .nonzero

    # Count zeros, don't output them yet
    add [rb + zero_count], 1, [rb + zero_count]
    jz  0, .loop_end

.nonzero:
    # Was this a long run of zeros?
    lt  MAX_ZERO_COUNT, [rb + zero_count], [rb + tmp]
    jnz [rb + tmp], .finish_section

    # No, do we have a current section we can continue?
    jz  [section_count], .start_section

    # Yes, continue the section
    jz  0, .continue_section

.finish_section:
    # Finish current section if any
    jz  [section_count], .start_section

    # section_addr[section_count - 1].SECTION_SIZE = [rb + section_size]
    add [section_count], -1, [rb + tmp]
    mul [rb + tmp], SECTION_RECORD_SIZE, [rb + tmp]
    add [rb + tmp], [section_addr], [rb + tmp]
    add [rb + tmp], SECTION_SIZE, [ip + 3]
    add [rb + section_size], 0, [0]

    add [rb + byte_index], [rb + section_size], [rb + byte_index]

.start_section:
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

.continue_section:
    # Was there any run of zeros at all?
    jz  [rb + zero_count], .after_zeros

    # Yes, output the zero run as regular data, it wasn't long enough
    add [rb + section_size], [rb + zero_count], [rb + section_size]
    add 0, 0, [rb + zero_count]

.after_zeros:
    # Current byte belongs to this section
    add [rb + section_size], 1, [rb + section_size]

.loop_end:
    # Process next byte
    add [rb + data_index], 1, [rb + data_index]
    jz  0, .loop

.finish_last_section:
    # Is there a section to finish?
    jz  [section_count], .done

    # Finish the last section by writing section size
    add [section_count], -1, [rb + tmp]
    mul [rb + tmp], SECTION_RECORD_SIZE, [rb + tmp]
    add [rb + tmp], [section_addr], [rb + tmp]
    add [rb + tmp], SECTION_SIZE, [ip + 3]
    add [rb + section_size], 0, [0]

.done:
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
    add .start, 0, [rb - 1]
    arb -1
    call print_str

    call output_header
    call output_data

    # Output the middle part of the object file
    add .middle, 0, [rb - 1]
    arb -1
    call print_str

    call output_exports

    ret 0

.start:
    db ".C", 10, 0
.middle:
    db  10, ".R", 10, ".I", 10, ".E", 10, 0
.ENDFRAME

##########
output_header:
.FRAME section_index, section_address, section_start, section_size, tmp
    arb -5

    # Output image size
    add [data_size], 0, [rb - 1]
    arb -1
    call print_num

    out ','

    # Output section count
    add [section_count], 0, [rb - 1]
    arb -1
    call print_num

    # Loop all sections
    add 0, 0, [rb + section_index]

.sections_loop:
    eq  [rb + section_index], [section_count], [rb + tmp]
    jnz [rb + tmp], .sections_done

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
    jz  0, .sections_loop

.sections_done:
    arb 5
    ret 0
.ENDFRAME

##########
output_data:
.FRAME section_index, section_address, section_size, data_index, data_limit, tmp
    arb -6

    # Loop all sections
    add 0, 0, [rb + section_index]

.sections_loop:
    eq  [rb + section_index], [section_count], [rb + tmp]
    jnz [rb + tmp], .sections_done

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

.bytes_loop:
    eq  [rb + data_index], [rb + data_limit], [rb + tmp]
    jnz [rb + tmp], .bytes_done

    # Output next byte
    out ','
    add [data_addr], [rb + data_index], [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_num

    add [rb + data_index], 1, [rb + data_index]
    jz  0, .bytes_loop

.bytes_done:
    # Next section
    add [rb + section_index], 1, [rb + section_index]
    jz  0, .sections_loop

.sections_done:
    arb 6
    ret 0
.ENDFRAME

##########
output_exports:
.FRAME
    # Image
    add [name_addr], 0, [rb - 1]
    arb -1
    call print_str

    add .image, 0, [rb - 1]
    arb -1
    call print_str

    # Image is always at index 0
    out '0'
    out 10

    # Image size
    add [name_addr], 0, [rb - 1]
    arb -1
    call print_str

    add .size, 0, [rb - 1]
    arb -1
    call print_str

    # Image size is always at index 0
    out '0'
    out 10

    # Section count
    add [name_addr], 0, [rb - 1]
    arb -1
    call print_str

    add .count, 0, [rb - 1]
    arb -1
    call print_str

    # Section count is always at index 1
    out '1'
    out 10

    # Section header
    add [name_addr], 0, [rb - 1]
    arb -1
    call print_str

    add .header, 0, [rb - 1]
    arb -1
    call print_str

    # Section header is always at index 2
    out '2'
    out 10

    # Section data
    add [name_addr], 0, [rb - 1]
    arb -1
    call print_str

    add .data, 0, [rb - 1]
    arb -1
    call print_str

    # Section data starts after the header
    mul [section_count], SECTION_RECORD_SIZE, [rb - 1]
    add [rb - 1], 2, [rb - 1]
    arb -1
    call print_num

    out 10

    ret 0

.image:
    db  "_image:", 0
.size:
    db  "_size:", 0
.count:
    db  "_count:", 0
.header:
    db  "_header:", 0
.data:
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
