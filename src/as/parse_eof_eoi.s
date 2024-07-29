.EXPORT parse_dir_eof
.EXPORT parse_dir_eoi

# from libxib/input.s
.IMPORT reset_input_location

# from error.s
.IMPORT report_error

# from parse_frame.s
.IMPORT is_frame

##########
parse_dir_eof:
.FRAME
    # detect missing .ENDFRAME
    jz  [is_frame], .done

    add err_expect_endframe, 0, [rb + 0]
    call report_error

.done:
    ret 0
.ENDFRAME

##########
parse_dir_eoi:
.FRAME
    # detect missing .ENDFRAME
    jz  [is_frame], .done

    add err_expect_endframe, 0, [rb + 0]
    call report_error

.done:
    # end of included file, reset lexer position
    call reset_input_location

    ret 0
.ENDFRAME

##########
# error messages

err_expect_endframe:
    db  "Expecting .ENDFRAME", 0

.EOF
