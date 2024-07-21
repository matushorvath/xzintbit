.EXPORT read_identifier
.EXPORT read_string
.EXPORT read_number

# from input.s
.IMPORT get_input
.IMPORT unget_input

# from heap.s
.IMPORT alloc

# from string.s
.IMPORT char_to_digit
.IMPORT is_alphanum

# from outside of this library
.IMPORT report_libxib_error

# TODO unlimited identifier length
.SYMBOL IDENTIFIER_LENGTH 45

# TODO unlimited string length
.SYMBOL STRING_LENGTH 49

##########
read_identifier:
.FRAME buffer, index, char, tmp
    arb -4

    # we will store the identifier in dynamic memory that needs to be freed by caller
    add IDENTIFIER_LENGTH, 1, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + buffer]

    add 0, 0, [rb + index]

read_identifier_loop:
    call get_input
    add [rb - 2], 0, [rb + char]

    # when we find first non-alphanumeric character, we are done
    add [rb + char], 0, [rb - 1]
    arb -1
    call is_alphanum
    jz  [rb - 3], read_identifier_done

    # store the character in buffer
    add [rb + buffer], [rb + index], [ip + 3]
    add [rb + char], 0, [0]

    # increase index and check for maximum identifier length
    add [rb + index], 1, [rb + index]
    lt  [rb + index], IDENTIFIER_LENGTH, [rb + tmp]
    jnz [rb + tmp], read_identifier_loop

    add err_max_identifier_length, 0, [rb]
    call report_libxib_error

read_identifier_done:
    # zero terminate
    add [rb + buffer], [rb + index], [ip + 3]
    add 0, 0, [0]

    # unget last char
    add [rb + char], 0, [rb - 1]
    arb -1
    call unget_input

    arb 4
    ret 0
.ENDFRAME

##########
read_string:
.FRAME buffer, index, char, tmp
    arb -4

    # we will store the string in dynamic memory that needs to be freed by caller
    add STRING_LENGTH, 1, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + buffer]

    add 0, 0, [rb + index]

    # the opening quote was already processed by caller

read_string_loop:
    call get_input
    add [rb - 2], 0, [rb + char]

    # when we find a quote character, we are done
    eq  [rb + char], '"', [rb + tmp]
    jnz [rb + tmp], read_string_done

    # backslash is an escape character
    eq  [rb + char], '\', [rb + tmp]
    jz  [rb + tmp], read_string_after_escape

    # read the escaped character
    call get_input
    add [rb - 2], 0, [rb + char]

read_string_after_escape:
    # store the character in buffer
    add [rb + buffer], [rb + index], [ip + 3]
    add [rb + char], 0, [0]

    # increase index and check for maximum string length
    add [rb + index], 1, [rb + index]
    lt  [rb + index], STRING_LENGTH, [rb + tmp]
    jnz [rb + tmp], read_string_loop

    add err_max_string_length, 0, [rb]
    call report_libxib_error

read_string_done:
    # zero terminate
    add [rb + buffer], [rb + index], [ip + 3]
    add 0, 0, [0]

    arb 4
    ret 0
.ENDFRAME

##########
read_number:
.FRAME byte, is_number, radix, char, digit, sign, tmp             # returns byte, is_number, radix
    arb -7

    add 0, 0, [rb + byte]
    add 1, 0, [rb + sign]
    add 0, 0, [rb + is_number]
    add 10, 0, [rb + radix]

    # get first character, process the minus sign if present
    call get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], '-', [rb + tmp]
    jz  [rb + tmp], read_number_prefix

    add -1, 0, [rb + sign]

    call get_input
    add [rb - 2], 0, [rb + char]

read_number_prefix:
    # detect a 0b, 0o or 0x prefix
    eq  [rb + char], '0', [rb + tmp]
    jz  [rb + tmp], read_number_loop

    # starts with zero, is there a 0b, 0o or 0x?
    call get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 'b', [rb + tmp]
    jz  [rb + tmp], read_number_not_binary
    add 2, 0, [rb + radix]

    call get_input
    add [rb - 2], 0, [rb + char]

    jz  0, read_number_loop

read_number_not_binary:
    eq  [rb + char], 'o', [rb + tmp]
    jz  [rb + tmp], read_number_not_octal
    add 8, 0, [rb + radix]

    call get_input
    add [rb - 2], 0, [rb + char]

    jz  0, read_number_loop

read_number_not_octal:
    eq  [rb + char], 'x', [rb + tmp]
    jz  [rb + tmp], read_number_not_hexadecimal
    add 16, 0, [rb + radix]

    call get_input
    add [rb - 2], 0, [rb + char]

    jz  0, read_number_loop

read_number_not_hexadecimal:
    # no prefix, but we have the zero, so we have at least one digit
    add 1, 0, [rb + is_number]

read_number_loop:
    # convert current character to a digit
    add [rb + char], 0, [rb - 1]
    add [rb + radix], 0, [rb - 2]
    arb -2
    call char_to_digit
    add [rb - 4], 0, [rb + digit]

    # if it is not a digit, end
    eq  [rb + digit], -1, [rb + tmp]
    jnz [rb + tmp], read_number_end

    # byte = byte * radix + digit
    mul [rb + byte], [rb + radix], [rb + byte]
    add [rb + byte], [rb + digit], [rb + byte]

    # we have at least one digit
    add 1, 0, [rb + is_number]

    # get next character
    call get_input
    add [rb - 2], 0, [rb + char]

    jz  0, read_number_loop

read_number_end:
    mul [rb + byte], [rb + sign], [rb + byte]

    # unget last char
    add [rb + char], 0, [rb - 1]
    arb -1
    call unget_input

    arb 7
    ret 0
.ENDFRAME

##########
# error messages

err_max_identifier_length:
    db  "Maximum identifier length exceeded", 0
err_max_string_length:
    db  "Maximum string length exceeded", 0

.EOF
