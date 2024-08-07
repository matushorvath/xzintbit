.EXPORT read_identifier
.EXPORT read_string
.EXPORT read_number

# from input.s
.IMPORT get_input
.IMPORT unget_input

# from heap.s
.IMPORT alloc_blocks
.IMPORT realloc_more_blocks
.IMPORT neg_chunk_header_size
.IMPORT block_size

# from string.s
.IMPORT char_to_digit
.IMPORT is_alphanum

.SYMBOL IDENTIFIER_DEFAULT_ALLOC_SIZE   8       # size in memory blocks, for alloc_blocks (8 * 8 - 2 = 62 bytes)
.SYMBOL STRING_DEFAULT_ALLOC_SIZE       8       # size in memory blocks, for alloc_blocks (8 * 8 - 2 = 62 bytes)
.SYMBOL REALLOC_INCREMENT               8       # how many additional blocks to realloc when needed

##########
read_identifier:
.FRAME buffer, index, char, blocks, space_left, tmp
    arb -6

    # we will store the identifier in dynamic memory that needs to be freed by caller
    add IDENTIFIER_DEFAULT_ALLOC_SIZE, 0, [rb + blocks]

    add [rb + blocks], 0, [rb - 1]
    arb -1
    call alloc_blocks
    add [rb - 3], 0, [rb + buffer]

    # how many more characters we can input before allocating more memory?
    mul [rb + blocks], [block_size], [rb + space_left]
    add [rb + space_left], [neg_chunk_header_size], [rb + space_left]
    add [rb + space_left], -1, [rb + space_left]            # space for zero termination

    add 0, 0, [rb + index]

.loop:
    # any space for characters left?
    jnz [rb + space_left], .have_space

    # no, need to allocate more
    add [rb + blocks], REALLOC_INCREMENT, [rb + blocks]

    add [rb + buffer], 0, [rb - 1]
    add [rb + blocks], 0, [rb - 2]
    arb -2
    call realloc_more_blocks
    add [rb - 4], 0, [rb + buffer]

    # there is now more space left
    mul REALLOC_INCREMENT, [block_size], [rb + space_left]

.have_space:
    call get_input
    add [rb - 2], 0, [rb + char]

    # when we find first non-alphanumeric character, we are done
    add [rb + char], 0, [rb - 1]
    arb -1
    call is_alphanum
    jz  [rb - 3], .done

    # store the character in buffer
    add [rb + buffer], [rb + index], [ip + 3]
    add [rb + char], 0, [0]

    # increase index and decrease space left
    add [rb + index], 1, [rb + index]
    add [rb + space_left], -1, [rb + space_left]

    jz  0, .loop

.done:
    # zero terminate
    add [rb + buffer], [rb + index], [ip + 3]
    add 0, 0, [0]

    # unget last char
    add [rb + char], 0, [rb - 1]
    arb -1
    call unget_input

    arb 6
    ret 0
.ENDFRAME

##########
read_string:
.FRAME buffer, index, char, blocks, space_left, tmp
    arb -6

    # we will store the string in dynamic memory that needs to be freed by caller
    add STRING_DEFAULT_ALLOC_SIZE, 0, [rb + blocks]

    add [rb + blocks], 0, [rb - 1]
    arb -1
    call alloc_blocks
    add [rb - 3], 0, [rb + buffer]

    # how many more characters we can input before allocating more memory?
    mul [rb + blocks], [block_size], [rb + space_left]
    add [rb + space_left], [neg_chunk_header_size], [rb + space_left]
    add [rb + space_left], -1, [rb + space_left]            # space for zero termination

    add 0, 0, [rb + index]

    # the opening quote was already processed by caller

.loop:
    # any space for characters left?
    jnz [rb + space_left], .have_space

    # no, need to allocate more
    add [rb + blocks], REALLOC_INCREMENT, [rb + blocks]

    add [rb + buffer], 0, [rb - 1]
    add [rb + blocks], 0, [rb - 2]
    arb -2
    call realloc_more_blocks
    add [rb - 4], 0, [rb + buffer]

    # there is now more space left
    mul REALLOC_INCREMENT, [block_size], [rb + space_left]

.have_space:
    call get_input
    add [rb - 2], 0, [rb + char]

    # when we find a quote character, we are done
    eq  [rb + char], '"', [rb + tmp]
    jnz [rb + tmp], .done

    # backslash is an escape character
    eq  [rb + char], '\', [rb + tmp]
    jz  [rb + tmp], .after_escape

    # read the escaped character
    call get_input
    add [rb - 2], 0, [rb + char]

.after_escape:
    # store the character in buffer
    add [rb + buffer], [rb + index], [ip + 3]
    add [rb + char], 0, [0]

    # increase index and decrease space left
    add [rb + index], 1, [rb + index]
    add [rb + space_left], -1, [rb + space_left]

    jz  0, .loop

.done:
    # zero terminate
    add [rb + buffer], [rb + index], [ip + 3]
    add 0, 0, [0]

    arb 6
    ret 0
.ENDFRAME

##########
read_number:
.FRAME byte, is_number, radix, char, digit, sign, tmp       # returns byte, is_number, radix
    arb -7

    add 0, 0, [rb + byte]
    add 1, 0, [rb + sign]
    add 0, 0, [rb + is_number]
    add 10, 0, [rb + radix]

    # get first character, process the minus sign if present
    call get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], '-', [rb + tmp]
    jz  [rb + tmp], .prefix

    add -1, 0, [rb + sign]

    call get_input
    add [rb - 2], 0, [rb + char]

.prefix:
    # detect a 0b, 0o or 0x prefix
    eq  [rb + char], '0', [rb + tmp]
    jz  [rb + tmp], .loop

    # starts with zero, is there a 0b, 0o or 0x?
    call get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 'b', [rb + tmp]
    jz  [rb + tmp], .not_binary
    add 2, 0, [rb + radix]

    call get_input
    add [rb - 2], 0, [rb + char]

    jz  0, .loop

.not_binary:
    eq  [rb + char], 'o', [rb + tmp]
    jz  [rb + tmp], .not_octal
    add 8, 0, [rb + radix]

    call get_input
    add [rb - 2], 0, [rb + char]

    jz  0, .loop

.not_octal:
    eq  [rb + char], 'x', [rb + tmp]
    jz  [rb + tmp], .not_hexadecimal
    add 16, 0, [rb + radix]

    call get_input
    add [rb - 2], 0, [rb + char]

    jz  0, .loop

.not_hexadecimal:
    # no prefix, but we have the zero, so we have at least one digit
    add 1, 0, [rb + is_number]

.loop:
    # convert current character to a digit
    add [rb + char], 0, [rb - 1]
    add [rb + radix], 0, [rb - 2]
    arb -2
    call char_to_digit
    add [rb - 4], 0, [rb + digit]

    # if it is not a digit, end
    eq  [rb + digit], -1, [rb + tmp]
    jnz [rb + tmp], .end

    # byte = byte * radix + digit
    mul [rb + byte], [rb + radix], [rb + byte]
    add [rb + byte], [rb + digit], [rb + byte]

    # we have at least one digit
    add 1, 0, [rb + is_number]

    # get next character
    call get_input
    add [rb - 2], 0, [rb + char]

    jz  0, .loop

.end:
    mul [rb + byte], [rb + sign], [rb + byte]

    # unget last char
    add [rb + char], 0, [rb - 1]
    arb -1
    call unget_input

    arb 7
    ret 0
.ENDFRAME

.EOF
