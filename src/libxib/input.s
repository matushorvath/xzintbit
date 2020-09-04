.EXPORT get_input
.EXPORT unget_input
.EXPORT peek_input
.EXPORT reset_input_location
.EXPORT input_line_num
.EXPORT input_column_num

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
peek_input:
.FRAME char
    arb -1

    # get input from the buffer if we have it
    add [input_buffer], 0, [rb + char]
    jnz [rb + char], get_input_done

    # get real input and store it in input buffer
    in  [rb + char]
    add [rb + char], 0, [input_buffer]

peek_input_done:
    arb 1
    ret 0
.ENDFRAME

##########
reset_input_location:
.FRAME
    arb -0

    # TODO maintain current file index, increment it here
    add 0, 0, [input_line_num]
    add 1, 0, [input_column_num]

    arb 0
    ret 0
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

.EOF
