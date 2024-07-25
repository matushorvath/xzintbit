.EXPORT parse_symbol
.EXPORT parse_dir_symbol
.EXPORT parse_dir_import_export

# from child.s
.IMPORT add_or_find_current_child_symbol
.IMPORT last_global_symbol

# from error.s
.IMPORT report_error

# from global.s
.IMPORT add_or_find_global_symbol
.IMPORT set_global_symbol_address
.IMPORT set_global_symbol_type

# from lexer.s
.IMPORT get_token
.IMPORT token_type
.IMPORT token_value

# from parse_param.s
.IMPORT parse_number_or_char

# from parser.s
.IMPORT current_address
.IMPORT err_expect_eol
.IMPORT err_expect_number
.IMPORT err_expect_identifier

##########
parse_symbol:
.FRAME tmp, address, symbol
    arb -3

    # default symbol address is current_address
    add [current_address], 0, [rb + address]

    # check if there is an offset
    eq  [token_type], '+', [rb + tmp]
    jz  [rb + tmp], parse_symbol_after_offset

    call get_token
    eq  [token_type], 'n', [rb + tmp]
    jnz [rb + tmp], parse_symbol_have_offset

    add err_expect_number, 0, [rb]
    call report_error

parse_symbol_have_offset:
    # add offset to symbol address
    add [rb + address], [token_value], [rb + address]
    call get_token

    eq  [token_type], '=', [rb + tmp]
    jnz [rb + tmp], parse_symbol_after_equals

    add err_expect_equals, 0, [rb]
    call report_error

parse_symbol_after_equals:
    call get_token

parse_symbol_after_offset:
    eq  [token_type], 'i', [rb + tmp]
    jnz [rb + tmp], parse_symbol_have_identifier

    eq  [token_type], 'd', [rb + tmp]
    jnz [rb + tmp], parse_symbol_have_dot_identifier

    add err_expect_identifier, 0, [rb]
    call report_error

parse_symbol_have_identifier:
    # add or retrieve symbol from the symbol table
    add [token_value], 0, [rb - 1]
    arb -1
    call add_or_find_global_symbol
    add [rb - 3], 0, [rb + symbol]

    add 0, 0, [token_value]             # token_value is now owned by add_or_find_global_symbol

    # set symbol address
    add [rb + symbol], 0, [rb - 1]
    add [rb + address], 0, [rb - 2]
    arb -2
    call set_global_symbol_address

    # this is the now the current global symbol, any future child references are related to it
    add [rb + symbol], 0, [last_global_symbol]

    jz  0, parse_symbol_colon

parse_symbol_have_dot_identifier:
    # add or retrieve child symbol of the current global symbol
    add [token_value], 0, [rb - 1]
    arb -1
    call add_or_find_current_child_symbol
    add [rb - 3], 0, [rb - 1]           # result of add_or_find_current_child_symbol -> first param of set_global_symbol_address
    add 0, 0, [token_value]             # token_value is now owned by add_or_find_current_child_symbol

    # set child symbol address
    add [rb + address], 0, [rb - 2]
    arb -2
    call set_global_symbol_address

parse_symbol_colon:
    call get_token

    eq  [token_type], ':', [rb + tmp]
    jnz [rb + tmp], parse_symbol_done

    add err_expect_colon, 0, [rb]
    call report_error

parse_symbol_done:
    arb 3
    ret 0
.ENDFRAME

##########
parse_dir_symbol:
.FRAME tmp, identifier, address, symbol
    arb -4

    # get the identifier
    call get_token

    eq  [token_type], 'i', [rb + tmp]
    jnz [rb + tmp], parse_dir_symbol_have_identifier

    add err_expect_identifier, 0, [rb]
    call report_error

parse_dir_symbol_have_identifier:
    # save the identifier
    add [token_value], 0, [rb + identifier]

    # get the value
    call get_token

    call parse_number_or_char
    add [rb - 2], 0, [rb + address]

    # add or retrieve symbol from the symbol table
    add [rb + identifier], 0, [rb - 1]
    arb -1
    call add_or_find_global_symbol
    add [rb - 3], 0, [rb + symbol]
    add 0, 0, [rb + identifier]             # identifier is now owned by add_or_find_global_symbol

    # set symbol address and type
    add [rb + symbol], 0, [rb - 1]
    add [rb + address], 0, [rb - 2]
    arb -2
    call set_global_symbol_address

    add [rb + symbol], 0, [rb - 1]
    add 3, 0, [rb - 2]
    arb -2
    call set_global_symbol_type

    # check end of line
    eq  [token_type], '$', [rb + tmp]
    jnz [rb + tmp], parse_dir_symbol_done

    add err_expect_eol, 0, [rb]
    call report_error

parse_dir_symbol_done:
    arb 4
    ret 0
.ENDFRAME

##########
parse_dir_import_export:
.FRAME tmp, type
    arb -2

    # determine if it's import (1) or export (2)
    add 1, 0, [rb + type]
    eq  [token_type], 'I', [rb + tmp]
    jnz [rb + tmp], parse_dir_import_export_have_type
    add 2, 0, [rb + type]

parse_dir_import_export_have_type:
    # get the identifier
    call get_token

    eq  [token_type], 'i', [rb + tmp]
    jnz [rb + tmp], parse_dir_import_export_have_identifier

    add err_expect_identifier, 0, [rb]
    call report_error

parse_dir_import_export_have_identifier:
    # add or retrieve symbol from the symbol table
    add [token_value], 0, [rb - 1]
    arb -1
    call add_or_find_global_symbol
    add [rb - 3], 0, [rb - 1]           # result of add_or_find_global_symbol -> first param of set_global_symbol_type
    add 0, 0, [token_value]             # token_value is now owned by add_or_find_global_symbol

    # set the global symbol as exported
    add [rb + type], 0, [rb - 2]
    arb -2
    call set_global_symbol_type

    # read end of line
    call get_token

    eq  [token_type], '$', [rb + tmp]
    jnz [rb + tmp], parse_dir_import_export_done

    add err_expect_eol, 0, [rb]
    call report_error

parse_dir_import_export_done:
    arb 2
    ret 0
.ENDFRAME

err_expect_equals:
    db  "Expecting an equals sign", 0
err_expect_colon:
    db  "Expecting a colon", 0

.EOF
