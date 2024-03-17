# Utility convert a binary file into an intcode object file

# Output file format:
#
#       db  "name", 0                                       # Zero terminated binary name, variable length
# <name>_section_count:
#       db  <count>                                         # Number of sections
# <name>_section_0_size:
#       db  <size0>                                         # Section 0 size in bytes
# <name>_section_0_data:
#       db  <byte_0>, <byte_1>, ... <byte_size-1>           # Section 0 data, <size0> bytes
# <name>_section_1_size:
#       db  <size1>                                         # Section 1 size in bytes
# <name>_section_1_data:
#       db  <byte_0>, <byte_1>, ... <byte_size-1>           # Section 1 data, <size0> bytes
# ...
# <name>_section_<count-1>_size:
#       db  <sizeX>                                         # Section <count-1> size in bytes
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
.IMPORT print_str_as_mem
.IMPORT is_alphanum
.IMPORT __heap_start

.SYMBOL MAX_NAME_LENGTH 20

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
output_object:
.FRAME
    # Output start of the object file
    add output_object_start, 0, [rb - 1]
    arb -1
    call print_str

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
output_data:
.FRAME index, tmp
    arb -2

    # Binary name
    add [name_addr], 0, [rb - 1]
    arb -1
    call print_str_as_mem

    # Zero termination
    out ','
    out '0'

    out ','

    # Section count
    # TODO real count
    add 1, 0, [rb - 1]
    arb -1
    call print_num

    # TODO loop all sections

    out ','

    # Current section size
    # TODO not data_size
    add [data_size], 0, [rb - 1]
    arb -1
    call print_num

    # Current section data
    # TODO read from section, not all data
    add 0, 0, [rb + index]

output_object_loop:
    eq  [rb + index], [data_size], [rb + tmp]
    jnz [rb + tmp], output_object_done

    out ','

    # Output next byte
    add [data_addr], [rb + index], [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_num

    add [rb + index], 1, [rb + index]
    jz  0, output_object_loop

output_object_done:
    arb 2
    ret 0
.ENDFRAME

##########
output_exports:
.FRAME section, index, tmp
    arb -3

    # Symbols start after the name + zero termination
    add [name_length], 1, [rb + index]

    # Section count
    add [name_addr], 0, [rb - 1]
    arb -1
    call print_str

    add output_exports_section_count, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + index], 0, [rb - 1]
    arb -1
    call print_num

    add [rb + index], 1, [rb + index]

    out 10

    # TODO loop all sections
    add 0, 0, [rb + section]

    # Current section size
    add [name_addr], 0, [rb - 1]
    arb -1
    call print_str

    add output_exports_section, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + section], 0, [rb - 1]
    arb -1
    call print_num

    add output_exports_size, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + index], 0, [rb - 1]
    arb -1
    call print_num

    out 10
    add [rb + index], 1, [rb + index]

    # Current section data
    add [name_addr], 0, [rb - 1]
    arb -1
    call print_str

    add output_exports_section, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + section], 0, [rb - 1]
    arb -1
    call print_num

    add output_exports_data, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + index], 0, [rb - 1]
    arb -1
    call print_num

    out 10
    # TODO use section size, not data size
    add [rb + index], [data_size], [rb + index]

    arb 3
    ret 0

output_exports_section_count:
    db  "_section_count:", 0
output_exports_section:
    db  "_section_", 0
output_exports_size:
    db  "_size:", 0
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

.EOF
