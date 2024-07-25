# TODO have a printf-like function to print more info about errors

.EXPORT report_error
.EXPORT report_libxib_error
.EXPORT report_global_symbol_error
.EXPORT report_global_fixup_error

# from libxib/error.s
.IMPORT report_error_at_location

# from lexer.s
.IMPORT token_line_num
.IMPORT token_column_num

# from libxib/print.s
.IMPORT print_str

##########
report_libxib_error:
    # We also export this as report_libxib_error, to be used for error reporting from the libxib library

report_error:
.FRAME message;
    # we don't bother with updating the stack pointer, this function never returns
    add [rb + message], 0, [rb + 2]
    add [token_line_num], 0, [rb + 1]
    add [token_column_num], 0, [rb]
    call report_error_at_location
.ENDFRAME

##########
report_global_symbol_error:
.FRAME symbol, message;
    # error message with an identifier and a location taken from lexer

    add [rb + symbol], 0, [rb - 1]
    arb -1
    call print_symbol_identifier

    # we don't bother with updating the stack pointer, this function never returns
    add [rb + message], 0, [rb + 2]
    add [token_line_num], 0, [rb + 1]
    add [token_column_num], 0, [rb]
    call report_error_at_location
.ENDFRAME

##########
report_global_fixup_error:
.FRAME symbol, message; fixup, line_num, column_num
    arb -3

    # error message with an identifier and a location taken from the first symbol fixup

    add [rb + symbol], 0, [rb - 1]
    arb -1
    call print_symbol_identifier

    add 0, 0, [rb + line_num]
    add 0, 0, [rb + column_num]

    # get first fixup for this symbol
    add [rb + symbol], GLOBAL_FIXUPS_HEAD, [ip + 1]
    add [0], 0, [rb + fixup]
    jz  [rb + fixup], report_symbol_error_after_location

    # read fixup line num
    add [rb + fixup], FIXUP_LINE_NUM, [ip + 1]
    add [0], 0, [rb + line_num]

    # read fixup column num
    add [rb + fixup], FIXUP_COLUMN_NUM, [ip + 1]
    add [0], 0, [rb + column_num]

report_symbol_error_after_location:
    # we don't bother with updating the stack pointer, this function never returns
    add [rb + message], 0, [rb + 2]
    add [rb + line_num], 0, [rb + 1]
    add [rb + column_num], 0, [rb]
    call report_error_at_location
.ENDFRAME

##########
print_symbol_identifier:
.FRAME symbol; identifier
    arb -1

    out '('

    add [rb + symbol], GLOBAL_IDENTIFIER_PTR, [ip + 1]
    add [0], 0, [rb + identifier]

    jnz [rb + identifier], print_symbol_identifier_have_identifier
    add print_symbol_identifier_null, 0, [rb + identifier]

print_symbol_identifier_have_identifier:
    # print the identifier
    add [rb + identifier], 0, [rb - 1]
    arb -1
    call print_str

    out ')'
    out ' '

    arb 1
    ret 1

print_symbol_identifier_null:
    db  "null identifier", 0
.ENDFRAME

.EOF
