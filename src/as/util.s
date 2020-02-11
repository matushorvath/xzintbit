# TODO
# - have a printf-like function to print more info about errors

.EXPORT report_error

# from libxib/error.s
.IMPORT report_error_at_location

# from lexer.s
.IMPORT token_line_num
.IMPORT token_column_num

##########
report_error:
.FRAME message;
    # we don't bother with updating the stack pointer, this function never returns
    add [rb + message], 0, [rb + 2]
    add [token_line_num], 0, [rb + 1]
    add [token_column_num], 0, [rb]
    call report_error_at_location
.ENDFRAME

.EOF
