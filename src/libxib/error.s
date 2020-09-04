.EXPORT report_error_at_location
.EXPORT report_error_with_symbol
.EXPORT halt_and_catch_fire

# User of this library should export a function called report_libxib_error
# that will be used to report errors from this library

# from print.s
.IMPORT print_num
.IMPORT print_str

##########
report_error_at_location:
.FRAME message, line_num, column_num;
    add report_error_at_location_msg_start, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + message], 0, [rb - 1]
    arb -1
    call print_str

    add report_error_at_location_msg_line, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + line_num], 0, [rb - 1]
    arb -1
    call print_num

    add report_error_at_location_msg_column, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + column_num], 0, [rb - 1]
    arb -1
    call print_num

    add report_error_at_location_msg_end, 0, [rb - 1]
    arb -1
    call print_str

    out 10

    call halt_and_catch_fire

report_error_at_location_msg_start:
    db "Error: ", 0
report_error_at_location_msg_line:
    db " (line ", 0
report_error_at_location_msg_column:
    db ", column ", 0
report_error_at_location_msg_end:
    db ")", 0
.ENDFRAME

##########
report_error_with_symbol:
.FRAME message, identifier;
    add report_error_with_symbol_msg_start, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + message], 0, [rb - 1]
    arb -1
    call print_str

    add report_error_with_symbol_msg_symbol, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + identifier], 0, [rb - 1]
    arb -1
    call print_str

    out 10

    call halt_and_catch_fire

report_error_with_symbol_msg_start:
    db "Error: ", 0
report_error_with_symbol_msg_symbol:
    db ": ", 0
.ENDFRAME

##########
halt_and_catch_fire:
.FRAME
    # instead of halting, we will read all inputs, which hopefully
    # will cause the intcode vm to crash, indicating a problem
    # this is the only way we can return a non-zero result from intcode

halt_and_catch_fire_loop:
    in  [0]
    jz  0, halt_and_catch_fire_loop
.ENDFRAME

.EOF
