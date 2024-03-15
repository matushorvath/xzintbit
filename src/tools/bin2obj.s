# Utility convert a binary file into an intcode object file

# File format:
# binary_length:
#       db  <size>                                          # File size in bytes
# binary_data:
#       db  <byte_0>, <byte_1>, ... <byte_size-1>           # File data, <size> bytes, each from 0 to 255

# Since Intcode has no way of detecting end of file, this utility expects the file size followed
# by a space to be passed through the standard input before the binary itself.

# The Makefile rule to run bin2obj could look like this:
# define run-bin2obj
# 	ls -n $< | awk '{ printf "%s ", $$5 }' | cat - $< | $(ICVM) ./bin/bin2obj.input > $@ || ( cat $@ ; false )
# endef

.IMPORT print_num
.IMPORT print_str

    arb stack

    add 0, 0, [size]

read_size_loop:
    # Read file size
    in  [digit]

    eq  [digit], ' ', [tmp]
    jnz [tmp], read_size_done
    lt  [digit], '0', [tmp]
    jnz [tmp], invalid_size
    lt  '9', [digit], [tmp]
    jnz [tmp], invalid_size

    add [digit], -48, [digit]           # 48 = '0'
    mul [size], 10, [size]
    add [size], [digit], [size]

    jz  0, read_size_loop

read_size_done:
    # Output .C header
    out '.'
    out 'C'
    out 10

    # Output file size
    add [size], 0, [rb - 1]
    arb -1
    call print_num

print_code_loop:
    # Output the code
    jz  [size], print_code_done
    add [size], -1, [size]

    out ','

    in  [rb - 1]
    arb -1
    call print_num

    jz  0, print_code_loop

print_code_done:
    # Output the rest of the object file, which is always the same
    add object_file_end, 0, [rb - 1]
    arb -1
    call print_str

    hlt

invalid_size:
    add invalid_size_error, 0, [rb - 1]
    arb -1
    call print_str

    hlt

tmp:
    db  0
digit:
    db  0
size:
    db  0

object_file_end:
    db  10, ".R", 10, ".I", 10, ".E", 10, "binary_length:0", 10, "binary_data:1", 10, 0
invalid_size_error:
    db  "invalid file size", 10, 0

    ds  50, 0
stack:

.EOF
