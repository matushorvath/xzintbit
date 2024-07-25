# TODO remember line and column for .EXPORT (perhaps in a dummy fixup record)

# token types:
# 1 add; 9 arb; 8 eq; 99 hlt; 3 in; 5 jnz; 6 jz; 7 lt; 2 mul; 4 out
# C call; R ret; B db; S ds
# F .FRAME; D .ENDFRAME; Y .SYMBOL; E .EXPORT; I .IMPORT; N .EOF; O EOI
# $ EOL; P rb; I ip
# + - = , : ; [ ]
# n [0-9]+; i [a-zA-Z_][a-zA-Z0-9_]*; d .[a-zA-Z_][a-zA-Z0-9_]*; c '.'; s ".*"

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

# from libxib/lexer.s
.IMPORT read_identifier
.IMPORT read_string
.IMPORT read_number

# from libxib/print.s
.IMPORT print_num
.IMPORT print_str

# from libxib/heap.s
.IMPORT free

# from libxib/string.s
.IMPORT is_digit
.IMPORT is_alpha

# from directive.s
.IMPORT detect_directive

# from error.s
.IMPORT report_error

# from keyword.s
.IMPORT detect_keyword

##########
get_token:
.FRAME tmp, char
    arb -2

.loop:
    # remember position at the start of the token
    add [input_line_num], 0, [token_line_num]
    add [input_column_num], 0, [token_column_num]

    # get next input character
    call get_input
    add [rb - 2], 0, [rb + char]

    # skip whitespace
    eq  [rb + char], ' ', [rb + tmp]
    jnz [rb + tmp], .loop
    eq  [rb + char], 7, [rb + tmp]
    jnz [rb + tmp], .loop

    # comments
    eq  [rb + char], 35, [rb + tmp]
    jnz [rb + tmp], .eat_comment

    # end of line
    eq  [rb + char], 10, [rb + tmp]
    jnz [rb + tmp], .eol
    eq  [rb + char], 13, [rb + tmp]
    jnz [rb + tmp], .eol

    # identifiers and keywords
    eq  [rb + char], '_', [rb + tmp]
    jnz [rb + tmp], .identifier

    add [rb + char], 0, [rb - 1]
    arb -1
    call is_alpha
    jnz [rb - 3], .identifier

    # directives
    eq  [rb + char], '.', [rb + tmp]
    jnz [rb + tmp], .directive

    # symbols
    add [rb + char], 0, [rb - 1]
    arb -1
    call is_symbol
    jnz [rb - 3], .symbol

    # string literals
    eq  [rb + char], '"', [rb + tmp]
    jnz [rb + tmp], .string

    # character literals
    eq  [rb + char], ''', [rb + tmp]
    jnz [rb + tmp], .char

    # number literals
    add [rb + char], 0, [rb - 1]
    arb -1
    call is_digit
    jnz [rb - 3], .number

    add err_invalid_token, 0, [rb]
    call report_error

.eat_comment:
    # eat everything until end of line

.eat_comment_loop:
    call get_input
    add [rb - 2], 0, [rb + char]

    # the comment ends with an EOL, so we jump to EOL handling
    eq  [rb + char], 10, [rb + tmp]
    jnz [rb + tmp], .eol
    eq  [rb + char], 13, [rb + tmp]
    jnz [rb + tmp], .eol

    jz  0, .eat_comment_loop

.eol:
    # handle Windows-style 13,10 line ends
    eq  [rb + char], 13, [rb + tmp]
    jz  [rb + tmp], .eol_unix

    call get_input
    eq  [rb - 2], 10, [rb + tmp]
    jnz [rb + tmp], .eol_unix

    add err_expect_10_after_13, 0, [rb]
    call report_error

.eol_unix:
    add '$', 0, [token_type]
    jz  0, .done

.identifier:
    # unget last char, read_identifier will get it again
    add [rb + char], 0, [rb - 1]
    arb -1
    call unget_input

    # return read identifier pointer in [token_value]
    # this memory needs to be freed by caller of get_token
    call read_identifier_or_keyword
    add [rb - 2], 0, [token_type]
    add [rb - 3], 0, [token_value]

    jz  0, .done

.directive:
    # return read identifier pointer in [token_value]
    # this memory needs to be freed by caller of get_token
    call read_dot_identifier_or_directive
    add [rb - 2], 0, [token_type]
    add [rb - 3], 0, [token_value]

    jz  0, .done

.symbol:
    add [rb + char], 0, [token_type]
    jz  0, .done

.string:
    # return read string pointer in [token_value]
    # this memory needs to be freed by caller of get_token
    call read_string
    add [rb - 2], 0, [token_value]

    add 's', 0, [token_type]
    jz  0, .done

.char:
    # get one character and return it as token value
    call get_input
    add [rb - 2], 0, [token_value]

    # get closing quote
    call get_input
    eq  [rb - 2], ''', [rb + tmp]
    jnz [rb + tmp], .char_done

    add err_expect_single_quote, 0, [rb]
    call report_error

.char_done:
    add 'c', 0, [token_type]
    jz  0, .done

.number:
    # unget last char, read_number will get it again
    add [rb + char], 0, [rb - 1]
    arb -1
    call unget_input

    # return read number in [token_value]
    call read_number
    add [rb - 2], 0, [token_value]

    # was there actually a number?
    jnz [rb - 3], .number_done

    add err_expect_number, 0, [rb]
    call report_error

.number_done:
    add 'n', 0, [token_type]
    jz  0, .done

.done:
    arb 2
    ret 0
.ENDFRAME

##########
is_symbol:
.FRAME char; tmp
    arb -1

    eq  [rb + char], '+', [rb + tmp]
    jnz [rb + tmp], .done
    eq  [rb + char], '-', [rb + tmp]
    jnz [rb + tmp], .done
    eq  [rb + char], '=', [rb + tmp]
    jnz [rb + tmp], .done
    eq  [rb + char], ',', [rb + tmp]
    jnz [rb + tmp], .done
    eq  [rb + char], ':', [rb + tmp]
    jnz [rb + tmp], .done
    eq  [rb + char], ';', [rb + tmp]
    jnz [rb + tmp], .done
    eq  [rb + char], '[', [rb + tmp]
    jnz [rb + tmp], .done
    eq  [rb + char], ']', [rb + tmp]

.done:
    arb 1
    ret 1
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
    jnz [rb + tmp], .skip_free

    add [rb + buffer], 0, [rb - 1]
    arb -1
    call free
    add 0, 0, [rb + buffer]

.skip_free:
    arb 3
    ret 0
.ENDFRAME

##########
read_dot_identifier_or_directive:
.FRAME token, buffer, tmp
    arb -3

    # read an identifier into a buffer
    call read_identifier
    add [rb - 2], 0, [rb + buffer]

    # check if the identifier is actually a directive
    # reuse buffer [rb - 2] and length [rb - 3] as parameters
    arb -3
    call detect_directive
    arb 1
    add [rb - 5], 0, [rb + token]

    # if it is a directive, free the buffer, we don't need it
    eq  [rb + token], 'd', [rb + tmp]
    jnz [rb + tmp], .skip_free

    add [rb + buffer], 0, [rb - 1]
    arb -1
    call free
    add 0, 0, [rb + buffer]

.skip_free:
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
    jnz [rb + tmp], .print_number
    eq  [token_type], 'c', [rb + tmp]
    jnz [rb + tmp], .print_char
    eq  [token_type], 'i', [rb + tmp]
    jnz [rb + tmp], .print_string
    eq  [token_type], 'd', [rb + tmp]
    jnz [rb + tmp], .print_string
    jz  0, .done

.print_number:
    out ' '
    add [token_value], 0, [rb - 1]
    arb -1
    call print_num
    jz  0, .done

.print_char:
    out ' '
    out [token_value]
    jz  0, .done

.print_string:
    out ' '
    add [token_value], 0, [rb - 1]
    arb -1
    call print_str
    jz  0, .done

.done:
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

##########
# error messages

err_invalid_token:
    db  "Invalid token", 0
err_expect_single_quote:
    db  "Expecting a single quote", 0
err_expect_10_after_13:
    db  "Expecting character 10 after character 13", 0
err_expect_number:
    db  "Expecting a number", 0

.EOF
