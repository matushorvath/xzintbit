# TODO:
# - remember line and column for .EXPORT (perhaps in a dummy fixup record)

# token types:
# 1 add; 9 arb; 8 eq; 99 hlt; 3 in; 5 jnz; 6 jz; 7 lt; 2 mul; 4 out
# C call; R ret; B db; S ds
# F .FRAME; D .ENDFRAME; Y .SYMBOL; E .EXPORT; I .IMPORT; N .EOF; O EOI
# $ EOL; P rb; I ip
# + - = , : ; [ ]
# n [0-9]+; i [a-zA-Z_][a-zA-Z0-9_]*; c '.'; s ".*"

.EXPORT get_token
.EXPORT token_type
.EXPORT token_value
.EXPORT token_line_num
.EXPORT token_column_num

# from libxib/input.s
.IMPORT get_input
.IMPORT unget_input
.IMPORT input_line_num
.IMPORT input_column_num

# from libxib/print.s
.IMPORT print_num
.IMPORT print_str

# from libxib/heap.s
.IMPORT alloc
.IMPORT free

# from libxib/string.s
.IMPORT is_digit
.IMPORT is_alpha
.IMPORT is_alphanum
.IMPORT strcmp

# from directive.s
.IMPORT detect_directive

# from keyword.s
.IMPORT detect_keyword

# from util.s
.IMPORT report_error

##########
get_token:
.FRAME tmp, char
    arb -2

get_token_loop:
    # remember position at the start of the token
    add [input_line_num], 0, [token_line_num]
    add [input_column_num], 0, [token_column_num]

    # get next input character
    call get_input
    add [rb - 2], 0, [rb + char]

    # skip whitespace
    eq  [rb + char], ' ', [rb + tmp]
    jnz [rb + tmp], get_token_loop
    eq  [rb + char], 7, [rb + tmp]
    jnz [rb + tmp], get_token_loop

    # comments
    eq  [rb + char], 35, [rb + tmp]
    jnz [rb + tmp], get_token_eat_comment

    # end of line
    eq  [rb + char], 10, [rb + tmp]
    jnz [rb + tmp], get_token_eol
    eq  [rb + char], 13, [rb + tmp]
    jnz [rb + tmp], get_token_eol

    # identifiers and keywords
    eq  [rb + char], '_', [rb + tmp]
    jnz [rb + tmp], get_token_identifier

    add [rb + char], 0, [rb - 1]
    arb -1
    call is_alpha
    jnz [rb - 3], get_token_identifier

    # directives
    eq  [rb + char], '.', [rb + tmp]
    jnz [rb + tmp], get_token_directive

    # symbols
    add [rb + char], 0, [rb - 1]
    arb -1
    call is_symbol
    jnz [rb - 3], get_token_symbol

    # string literals
    eq  [rb + char], '"', [rb + tmp]
    jnz [rb + tmp], get_token_string

    # character literals
    eq  [rb + char], ''', [rb + tmp]
    jnz [rb + tmp], get_token_char

    # number literals
    add [rb + char], 0, [rb - 1]
    arb -1
    call is_digit
    jnz [rb - 3], get_token_number

    add err_invalid_token, 0, [rb]
    call report_error

get_token_eat_comment:
    # eat everything until end of line

get_token_eat_comment_loop:
    call get_input
    add [rb - 2], 0, [rb + char]

    # the comment ends with an EOL, so we jump to EOL handling
    eq  [rb + char], 10, [rb + tmp]
    jnz [rb + tmp], get_token_eol
    eq  [rb + char], 13, [rb + tmp]
    jnz [rb + tmp], get_token_eol

    jz  0, get_token_eat_comment_loop

get_token_eol:
    # handle Windows-style 13,10 line ends
    eq  [rb + char], 13, [rb + tmp]
    jz  [rb + tmp], get_token_eol_unix

    call get_input
    eq  [rb - 2], 10, [rb + tmp]
    jnz [rb + tmp], get_token_eol_unix

    add err_expect_10_after_13, 0, [rb]
    call report_error

get_token_eol_unix:
    add '$', 0, [token_type]
    jz  0, get_token_done

get_token_identifier:
    # unget last char, read_identifier will get it again
    add [rb + char], 0, [rb - 1]
    arb -1
    call unget_input

    # return read identifier pointer in [token_value]
    # this memory needs to be freed by caller of get_token
    call read_identifier_or_keyword
    add [rb - 2], 0, [token_type]
    add [rb - 3], 0, [token_value]

    jz  0, get_token_done

get_token_directive:
    call read_directive
    add [rb - 2], 0, [token_type]
    jz  0, get_token_done

get_token_symbol:
    add [rb + char], 0, [token_type]
    jz  0, get_token_done

get_token_string:
    # return read string pointer in [token_value]
    # this memory needs to be freed by caller of get_token
    call read_string
    add [rb - 2], 0, [token_value]

    add 's', 0, [token_type]
    jz  0, get_token_done

get_token_char:
    # get one character and return it as token value
    call get_input
    add [rb - 2], 0, [token_value]

    # get closing quote
    call get_input
    eq  [rb - 2], ''', [rb + tmp]
    jnz [rb + tmp], get_token_char_done

    add err_expect_double_quote, 0, [rb]
    call report_error

get_token_char_done:
    add 'c', 0, [token_type]
    jz  0, get_token_done

get_token_number:
    # unget last char, read_number will get it again
    add [rb + char], 0, [rb - 1]
    arb -1
    call unget_input

    # return read number in [token_value]
    call read_number
    add [rb - 2], 0, [token_value]

    add 'n', 0, [token_type]
    jz  0, get_token_done

get_token_done:
    arb 2
    ret 0
.ENDFRAME

##########
is_symbol:
.FRAME char; tmp
    arb -1

    eq  [rb + char], '+', [rb + tmp]
    jnz [rb + tmp], is_symbol_end
    eq  [rb + char], '-', [rb + tmp]
    jnz [rb + tmp], is_symbol_end
    eq  [rb + char], '=', [rb + tmp]
    jnz [rb + tmp], is_symbol_end
    eq  [rb + char], ',', [rb + tmp]
    jnz [rb + tmp], is_symbol_end
    eq  [rb + char], ':', [rb + tmp]
    jnz [rb + tmp], is_symbol_end
    eq  [rb + char], ';', [rb + tmp]
    jnz [rb + tmp], is_symbol_end
    eq  [rb + char], '[', [rb + tmp]
    jnz [rb + tmp], is_symbol_end
    eq  [rb + char], ']', [rb + tmp]

is_symbol_end:
    arb 1
    ret 1
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
    call report_error

read_string_done:
    # zero terminate
    add [rb + buffer], [rb + index], [ip + 3]
    add 0, 0, [0]

    arb 4
    ret 0
.ENDFRAME

##########
read_identifier_or_keyword:
.FRAME token, buffer, tmp
    arb -3

    # read the identifier into a buffer
    call read_identifier
    add [rb - 2], 0, [rb + buffer]

    # check if the identifier is actually a keyword
    # reuse buffer [rb - 2] and length [rb - 3] as parameters
    arb -3
    call detect_keyword
    arb 1
    add [rb - 5], 0, [rb + token]

    # if it is a keyword, free the buffer, we don't need it
    eq  [rb + token], 'i', [rb + tmp]
    jnz [rb + tmp], read_identifier_or_keyword_skip_free

    add [rb + buffer], 0, [rb - 1]
    arb -1
    call free
    add 0, 0, [rb + buffer]

read_identifier_or_keyword_skip_free:
    arb 3
    ret 0
.ENDFRAME
##########

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

    # unget last char
    add [rb + char], 0, [rb - 1]
    arb -1
    call unget_input

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
read_directive:
.FRAME token, buffer, tmp
    arb -3

    # read an identifier (which is actually the directive without the initial dot) into a buffer
    call read_identifier
    add [rb - 2], 0, [rb + buffer]

    # check if the identifier is a valid directive
    # reuse buffer [rb - 2] and length [rb - 3] as parameters
    arb -3
    call detect_directive
    arb 1
    add [rb - 5], 0, [rb + token]

    # free the buffer, we don't need it anymore
    add [rb + buffer], 0, [rb - 1]
    arb -1
    call free
    add 0, 0, [rb + buffer]

    arb 3
    ret 0
.ENDFRAME

##########
dump_token:
.FRAME tmp
    arb -1

    # print token type
    out [token_type]

    # print token value if relevant
    eq  [token_type], 'n', [rb + tmp]
    jnz [rb + tmp], dump_token_print_n
    eq  [token_type], 'c', [rb + tmp]
    jnz [rb + tmp], dump_token_print_c
    eq  [token_type], 'i', [rb + tmp]
    jnz [rb + tmp], dump_token_print_i
    jz  0, dump_token_finish

dump_token_print_n:
    out ' '
    add [token_value], 0, [rb - 1]
    arb -1
    call print_num
    jz  0, dump_token_finish

dump_token_print_c:
    out ' '
    out [token_value]
    jz  0, dump_token_finish

dump_token_print_i:
    out ' '
    add [token_value], 0, [rb - 1]
    arb -1
    call print_str
    jz  0, dump_token_finish

dump_token_finish:
    out 10

    arb 1
    ret 0
.ENDFRAME

##########
# globals

# line and column number of current token
token_line_num:
    db  1
token_column_num:
    db  1

# lookahead token type
token_type:
    db  0

# lookahead token value, if any
token_value:
    db  0

.SYMBOL IDENTIFIER_LENGTH 45

##########
# error messages

err_invalid_token:
    db  "Invalid token", 0
err_expect_double_quote:
    db  "Expecting a double quote", 0
err_max_string_length:
    db  "Maximum string length exceeded", 0
err_max_identifier_length:
    db  "Maximum identifier length exceeded", 0
err_expect_10_after_13:
    db  "Expecting character 10 after character 13", 0

.EOF
