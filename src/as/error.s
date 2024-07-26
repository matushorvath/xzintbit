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
report_global_symbol_error: # TODO rename to report_symbol_error
.FRAME global, message;
    # error message with an identifier and a location taken from lexer

    add [rb + global], 0, [rb - 1]
    arb -1
    call print_symbol_identifier

    # we don't bother with updating the stack pointer, this function never returns
    add [rb + message], 0, [rb + 2]
    add [token_line_num], 0, [rb + 1]
    add [token_column_num], 0, [rb]
    call report_error_at_location
.ENDFRAME

##########
report_global_fixup_error: # TODO rename to report_symbol_fixup_error
.FRAME global, message; fixup, line_num, column_num
    arb -3

    # error message with an identifier and a location taken from the first symbol fixup

    add [rb + global], 0, [rb - 1]
    arb -1
    call print_symbol_identifier

    add 0, 0, [rb + line_num]
    add 0, 0, [rb + column_num]

    # get first fixup for this global
    add [rb + global], GLOBAL_FIXUPS_HEAD, [ip + 1]
    add [0], 0, [rb + fixup]
    jz  [rb + fixup], .after_location

    # read fixup line num
    add [rb + fixup], FIXUP_LINE_NUM, [ip + 1]
    add [0], 0, [rb + line_num]

    # read fixup column num
    add [rb + fixup], FIXUP_COLUMN_NUM, [ip + 1]
    add [0], 0, [rb + column_num]

.after_location:
    # we don't bother with updating the stack pointer, this function never returns
    add [rb + message], 0, [rb + 2]
    add [rb + line_num], 0, [rb + 1]
    add [rb + column_num], 0, [rb]
    call report_error_at_location
.ENDFRAME

##########
print_symbol_identifier:
.FRAME symbol; identifier, tmp
    arb -2

    out '('

    # is this a child symbol?
    add [rb + symbol], GLOBAL_TYPE, [ip + 1]
    eq  [0], 5, [rb + tmp]
    jz  [rb + tmp], .after_parent

    # yes, print the parent identifier first
    add [rb + symbol], GLOBAL_PARENT, [ip + 1]
    add [0], GLOBAL_IDENTIFIER_PTR, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_str

    out '.'

.after_parent:
    add [rb + symbol], GLOBAL_IDENTIFIER_PTR, [ip + 1]
    add [0], 0, [rb + identifier]

    jnz [rb + identifier], .have_identifier
    add .identifier_null, 0, [rb + identifier]

.have_identifier:
    # print the identifier
    add [rb + identifier], 0, [rb - 1]
    arb -1
    call print_str

    out ')'
    out ' '

    arb 2
    ret 1

.identifier_null:
    db  "null identifier", 0
.ENDFRAME

.EOF
