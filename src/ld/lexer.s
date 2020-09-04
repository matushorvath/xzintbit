# TODO can this file be merged with as/lexer.s?

.EXPORT read_directive
.EXPORT read_identifier
.EXPORT read_number
.EXPORT get_input
.EXPORT unget_input
.EXPORT input_line_num
.EXPORT input_column_num

# from libxib/heap.s
.IMPORT alloc

# from libxib/string.s
.IMPORT is_digit
.IMPORT is_alphanum

# from util.s
.IMPORT report_error

##########
read_directive:
.FRAME error_message; char, tmp
    arb -2

    call get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], '.', [rb + tmp]
    jz  [rb + tmp], read_directive_error

    call get_input
    add [rb - 2], 0, [rb + char]

    call get_input
    add [rb - 2], 0, [rb + tmp]

    eq  [rb + tmp], 10, [rb + tmp]
    jz  [rb + tmp], read_directive_error

    # return [rb + char]

    arb 2
    ret 1

read_directive_error:
    add [rb + error_message], 0, [rb]
    call report_error
.ENDFRAME

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
    call report_error

read_identifier_done:
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

    # return:
    # [rb + byte] is the numbner
    # [rb + digit] is the next non-digit character

    arb 4
    ret 0
.ENDFRAME

##########
get_input:
.FRAME char, tmp
    arb -2

    # get input from the buffer if we have it
    add [input_buffer], 0, [rb + char]
    add 0, 0, [input_buffer]

    jnz [rb + char], get_input_have_char
    in  [rb + char]

get_input_have_char:
    # track line and column number
    eq  [rb + char], 10, [rb + tmp]
    jz  [rb + tmp], get_input_same_line

    # we have a new line
    add [input_line_num], 1, [input_line_num]
    add [input_column_num], 0, [input_prev_column_num]
    add 1, 0, [input_column_num]
    jz  0, get_input_done

get_input_same_line:
    # we are on the same line
    add [input_column_num], 1, [input_column_num]

get_input_done:
    arb 2
    ret 0
.ENDFRAME

##########
unget_input:
.FRAME char; tmp
    arb -1

    # "unget" an unused char so we get it again later
    add [rb + char], 0, [input_buffer]

    # track line and column number
    eq  [rb + char], 10, [rb + tmp]
    jz  [rb + tmp], unget_input_same_line

    # we moved back to previous line, use [input_prev_column_num] as column num
    add [input_line_num], -1, [input_line_num]
    add [input_prev_column_num], 0, [input_column_num]
    jz  0, unget_input_done

unget_input_same_line:
    add [input_column_num], -1, [input_column_num]

unget_input_done:
    arb 1
    ret 1
.ENDFRAME

##########
# globals

# line and column number of next input character
input_line_num:
    db  1
input_column_num:
    db  1

# column number before last input character
input_prev_column_num:
    db  0

# last input char buffer
input_buffer:
    db  0

.SYMBOL IDENTIFIER_LENGTH 45

##########
# error messages

err_max_identifier_length:
    db  "Maximum identifier length exceeded", 0

.EOF
