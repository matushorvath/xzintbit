.EXPORT report_error

# from libxib/error.s
.IMPORT report_error_at_location

# from libxib/input.s
.IMPORT input_line_num
.IMPORT input_column_num

##########
report_error:
.FRAME message;
    # we don't bother with updating the stack pointer, this function never returns
    add [rb + message], 0, [rb + 2]
    add [input_line_num], 0, [rb + 1]
    add [input_column_num], 0, [rb]
    call report_error_at_location
.ENDFRAME

.EOF
