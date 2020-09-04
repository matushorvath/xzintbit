# TODO display line/column number instead of using just report_plain_error

.EXPORT read_identifier
.EXPORT read_string
.EXPORT read_number

# from error.s
.IMPORT report_plain_error

# from input.s
.IMPORT get_input
.IMPORT unget_input

# from heap.s
.IMPORT alloc

# from string.s
.IMPORT is_digit
.IMPORT is_alphanum

##########
read_identifier:
.FRAME buffer, index, char, tmp
    arb -4

    # we will store the identifier in dynamic memory that needs to be freed by caller
    add MEM_BLOCK_SIZE, 0, [rb - 1]
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
    call report_plain_error

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
    add MEM_BLOCK_SIZE, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + buffer]

    add 0, 0, [rb + index]

    # the opening quote was already processed by caller

read_string_loop:
    call get_input
    add [rb - 2], 0, [rb + char]

    # when we find a quote character, we are done
    # TODO escaping
    eq  [rb + char], '"', [rb + tmp]
    jnz [rb + tmp], read_string_done

    # store the character in buffer
    add [rb + buffer], [rb + index], [ip + 3]
    add [rb + char], 0, [0]

    # increase index and check for maximum string length
    add [rb + index], 1, [rb + index]
    lt  [rb + index], MEM_BLOCK_SIZE - 1, [rb + tmp]
    jnz [rb + tmp], read_string_loop

    add err_max_string_length, 0, [rb]
    call report_plain_error

read_string_done:
    # zero terminate
    add [rb + buffer], [rb + index], [ip + 3]
    add 0, 0, [0]

    arb 4
    ret 0
.ENDFRAME

##########
read_number:
.FRAME byte, digit, sign, tmp
    arb -4

    add 0, 0, [rb + byte]
    add 1, 0, [rb + sign]

    # get first character, process the minus sign if present
    call get_input
    add [rb - 2], 0, [rb + digit]

    eq  [rb + digit], '-', [rb + tmp]
    jz  [rb + tmp], read_number_loop

    add -1, 0, [rb + sign]

    call get_input
    add [rb - 2], 0, [rb + digit]

read_number_loop:
    # if it is not a digit, end
    add [rb + digit], 0, [rb - 1]
    arb -1
    call is_digit
    jz  [rb - 3], read_number_end

    # convert ASCII to a number
    add [rb + digit], -'0', [rb + digit]

    # byte = byte * 10 + digit
    mul [rb + byte], 10, [rb + byte]
    add [rb + byte], [rb + digit], [rb + byte]

    # get next character
    call get_input
    add [rb - 2], 0, [rb + digit]

    jz  0, read_number_loop

read_number_end:
    mul [rb + byte], [rb + sign], [rb + byte]

    # unget last char
    add [rb + digit], 0, [rb - 1]
    arb -1
    call unget_input

    arb 4
    ret 0
.ENDFRAME

##########
# globals

.SYMBOL IDENTIFIER_LENGTH 45

# allocation block size
.SYMBOL MEM_BLOCK_SIZE 50

##########
# error messages

err_max_identifier_length:
    db  "Maximum identifier length exceeded", 0
err_max_string_length:
    db  "Maximum string length exceeded", 0

.EOF
