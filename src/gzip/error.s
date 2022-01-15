.EXPORT report_libxib_error
.EXPORT report_error

# from libxib/print.s
.IMPORT print_str

# from libxib/error.s
.IMPORT halt_and_catch_fire

##########
report_libxib_error:
    # We also export this as report_libxib_error, to be used for error reporting from the libxib library

report_error:
.FRAME message;
    # we don't bother updating the stack pointer, this function never returns

    add report_error_msg_start, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + message], 0, [rb - 1]
    arb -1
    call print_str

    out 10

    call halt_and_catch_fire

report_error_msg_start:
    db "Error: ", 0
.ENDFRAME

.EOF
