# TODO
# - return address frame symbol?
# - check the typescript as implementation for missing error handling
# - test: both global and frame symbol; access frame outside of frame
# - don't duplicate constants in many files

.EXPORT initialize
.EXPORT parse
.EXPORT current_address
.EXPORT relocation_symbol

.EXPORT err_expect_comma
.EXPORT err_expect_eol
.EXPORT err_expect_identifier
.EXPORT err_expect_number

# from libxib/heap.s
.IMPORT free

# from fixup.s
.IMPORT add_fixup

# from global.s
.IMPORT set_global_symbol_type

# from lexer.s
.IMPORT get_token
.IMPORT start_new_file
.IMPORT token_type
.IMPORT token_value
.IMPORT token_line_num
.IMPORT token_column_num

# from memory.s
.IMPORT set_mem
.IMPORT set_mem_str

# from parse_frame.s
.IMPORT parse_dir_frame
.IMPORT parse_dir_endframe
.IMPORT is_frame

# from parse_instruction.s
.IMPORT parse_add_mul_lt_eq
.IMPORT parse_jnz_jz
.IMPORT parse_arb_out
.IMPORT parse_in
.IMPORT parse_hlt

# from parse_param.s
.IMPORT parse_in_param
.IMPORT parse_value
.IMPORT parse_number_or_char

# from parse_symbol.s
.IMPORT parse_symbol
.IMPORT parse_dir_symbol
.IMPORT parse_dir_import_export

# from util.s
.IMPORT report_error

##########
parse:
.FRAME tmp
    arb -1

parse_loop:
    call get_token

    # skip empty lines
    eq  [token_type], '$', [rb + tmp]
    jnz [rb + tmp], parse_loop

    # instructions
    eq  [token_type], 1, [rb + tmp]
    jnz [rb + tmp], parse_call_add_mul_lt_eq
    eq  [token_type], 2, [rb + tmp]
    jnz [rb + tmp], parse_call_add_mul_lt_eq
    eq  [token_type], 7, [rb + tmp]
    jnz [rb + tmp], parse_call_add_mul_lt_eq
    eq  [token_type], 8, [rb + tmp]
    jnz [rb + tmp], parse_call_add_mul_lt_eq

    eq  [token_type], 5, [rb + tmp]
    jnz [rb + tmp], parse_call_jnz_jz
    eq  [token_type], 6, [rb + tmp]
    jnz [rb + tmp], parse_call_jnz_jz

    eq  [token_type], 9, [rb + tmp]
    jnz [rb + tmp], parse_call_arb_out
    eq  [token_type], 4, [rb + tmp]
    jnz [rb + tmp], parse_call_arb_out

    eq  [token_type], 3, [rb + tmp]
    jnz [rb + tmp], parse_call_in
    eq  [token_type], 99, [rb + tmp]
    jnz [rb + tmp], parse_call_hlt

    # pseudo-instructions
    eq  [token_type], 'C', [rb + tmp]
    jnz [rb + tmp], parse_call_call
    eq  [token_type], 'R', [rb + tmp]
    jnz [rb + tmp], parse_call_ret
    eq  [token_type], 'B', [rb + tmp]
    jnz [rb + tmp], parse_call_db
    eq  [token_type], 'S', [rb + tmp]
    jnz [rb + tmp], parse_call_ds

    # symbols
    eq  [token_type], 'i', [rb + tmp]
    jnz [rb + tmp], parse_call_symbol
    eq  [token_type], '+', [rb + tmp]
    jnz [rb + tmp], parse_call_symbol

    # directives
    eq  [token_type], 'F', [rb + tmp]
    jnz [rb + tmp], parse_call_directive_frame
    eq  [token_type], 'D', [rb + tmp]
    jnz [rb + tmp], parse_call_directive_endframe
    eq  [token_type], 'Y', [rb + tmp]
    jnz [rb + tmp], parse_call_directive_symbol
    eq  [token_type], 'E', [rb + tmp]
    jnz [rb + tmp], parse_call_directive_import_export
    eq  [token_type], 'I', [rb + tmp]
    jnz [rb + tmp], parse_call_directive_import_export
    eq  [token_type], 'N', [rb + tmp]
    jnz [rb + tmp], parse_call_directive_eof
    eq  [token_type], 'O', [rb + tmp]
    jnz [rb + tmp], parse_call_directive_eoi

    add err_unexpected_token, 0, [rb]
    call report_error

parse_call_add_mul_lt_eq:
    call parse_add_mul_lt_eq
    jz  0, parse_loop

parse_call_jnz_jz:
    call parse_jnz_jz
    jz  0, parse_loop

parse_call_arb_out:
    call parse_arb_out
    jz  0, parse_loop

parse_call_in:
    call parse_in
    jz  0, parse_loop

parse_call_hlt:
    call parse_hlt
    jz  0, parse_loop

parse_call_call:
    call parse_call
    jz  0, parse_loop

parse_call_ret:
    call parse_ret
    jz  0, parse_loop

parse_call_db:
    call parse_db
    jz  0, parse_loop

parse_call_ds:
    call parse_ds
    jz  0, parse_loop

parse_call_symbol:
    call parse_symbol
    jz  0, parse_loop

parse_call_directive_frame:
    call parse_dir_frame
    jz  0, parse_loop

parse_call_directive_endframe:
    call parse_dir_endframe
    jz  0, parse_loop

parse_call_directive_symbol:
    call parse_dir_symbol
    jz  0, parse_loop

parse_call_directive_import_export:
    call parse_dir_import_export
    jz  0, parse_loop

parse_call_directive_eof:
    call parse_dir_eof
    jz  0, parse_done

parse_call_directive_eoi:
    call parse_dir_eoi
    jz  0, parse_loop

parse_done:
    arb 1
    ret 0
.ENDFRAME

##########
initialize:
.FRAME symbol
    arb -1

    # add a dummy symbol to store relocations not related to a symbol
    # set symbol type to 4 (relocation)
    add relocation_symbol, 0, [rb - 1]
    add 4, 0, [rb - 2]
    arb -2
    call set_global_symbol_type

    arb 1
    ret 0
.ENDFRAME

##########
parse_call:
.FRAME tmp, param, mode
    arb -3

    # eat the 'call' token
    call get_token

    # generate code: add current_address + 2 + 4 + 3, 0, [rb - 1]
    # 21101, current_address + 9, 0, -1
    add 21101, 0, [rb - 1]
    arb -1
    call set_mem

    add [current_address], 9, [rb - 1]
    arb -1
    call set_mem

    # add a fixup (actually a relocation) for the use of current_address
    add relocation_symbol, 0, [rb - 1]
    add [current_address], 1, [rb - 2]
    add [token_line_num], 0, [rb - 3]
    add [token_column_num], 0, [rb - 4]
    arb -4
    call add_fixup

    add 0, 0, [rb - 1]
    arb -1
    call set_mem

    add -1, 0, [rb - 1]
    arb -1
    call set_mem

    # generate code: arb -1
    # 109, -1
    add 109, 0, [rb - 1]
    arb -1
    call set_mem

    add -1, 0, [rb - 1]
    arb -1
    call set_mem

    # this parameter is located at current_address + 8
    add 8, 0, [rb - 1]
    add 9, 0, [rb - 2]
    arb -2
    call parse_in_param

    add [rb - 4], 0, [rb + param]
    add [rb - 5], 0, [rb + mode]

    # generate code: jz 0, $param
    # 106 + mode * 1000, 0, param
    mul [rb + mode], 1000, [rb + tmp]
    add 106, [rb + tmp], [rb - 1]
    arb -1
    call set_mem

    add 0, 0, [rb - 1]
    arb -1
    call set_mem

    add [rb + param], 0, [rb - 1]
    arb -1
    call set_mem

    eq  [token_type], '$', [rb + tmp]
    jnz [rb + tmp], parse_call_done

    add err_expect_eol, 0, [rb]
    call report_error

parse_call_done:
    add [current_address], 9, [current_address]

    arb 3
    ret 0
.ENDFRAME

##########
parse_ret:
.FRAME tmp, param
    arb -2

    # eat the 'ret' token
    call get_token

    # parse the parameter
    call parse_number_or_char
    add [rb - 2], 0, [rb + param]

    # generate code: arb $param + 1
    # 109, param + 1
    add 109, 0, [rb - 1]
    arb -1
    call set_mem

    add [rb + param], 1, [rb - 1]
    arb -1
    call set_mem

    # generate code: jz 0, [rb - ($param + 1)]
    # 2106, 0, -(param + 1)
    add 2106, 0, [rb - 1]
    arb -1
    call set_mem

    add 0, 0, [rb - 1]
    arb -1
    call set_mem

    add [rb + param], 1, [rb + tmp]
    mul [rb + tmp], -1, [rb - 1]
    arb -1
    call set_mem

    eq  [token_type], '$', [rb + tmp]
    jnz [rb + tmp], parse_ret_done

    add err_expect_eol, 0, [rb]
    call report_error

parse_ret_done:
    add [current_address], 5, [current_address]

    arb 2
    ret 0
.ENDFRAME

##########
parse_db:
.FRAME tmp, data, offset
    arb -3

    add 0, 0, [rb + offset]

parse_db_loop:
    # eat the 'db' token (first parameter) or the comma (subsequent parameters)
    call get_token

    eq  [token_type], 's', [rb + tmp]
    jnz [rb + tmp], parse_db_string

    # not a string, so it must be a value
    add [rb + offset], 0, [rb - 1]
    # next instruction address with db would need a fixup, so we just pass next value address
    add [rb + offset], 1, [rb - 2]
    add 1, 0, [rb - 3]  # global symbols
    add 1, 0, [rb - 4]  # frame symbols
    arb -4
    call parse_value
    add [rb - 6], 0, [rb + data]

    add [rb + data], 0, [rb - 1]
    arb -1
    call set_mem

    add [rb + offset], 1, [rb + offset]
    jz  0, parse_db_after_param

parse_db_string:
    # store the string, don't store zero termination
    add [token_value], 0, [rb - 1]
    arb -1
    call set_mem_str

    # string length is returned by set_mem_str
    add [rb + offset], [rb - 3], [rb + offset]

    # free the string
    add [token_value], 0, [rb - 1]
    arb -1
    call free
    add 0, 0, [token_value]

    call get_token

parse_db_after_param:
    eq  [token_type], ',', [rb + tmp]
    jnz [rb + tmp], parse_db_loop
    eq  [token_type], '$', [rb + tmp]
    jnz [rb + tmp], parse_db_done

    add err_expect_comma_eol, 0, [rb]
    call report_error

parse_db_done:
    add [current_address], [rb + offset], [current_address]

    arb 3
    ret 0
.ENDFRAME

##########
parse_ds:
.FRAME tmp, count, data
    arb -3

    # eat the 'ds' token
    call get_token

    eq  [token_type], 'n', [rb + tmp]
    jnz [rb + tmp], parse_ds_have_count

    add err_expect_number, 0, [rb]
    call report_error

parse_ds_have_count:
    add [token_value], 0, [rb + count]
    call get_token

    eq  [token_type], ',', [rb + tmp]
    jnz [rb + tmp], parse_ds_have_comma

    add err_expect_comma, 0, [rb]
    call report_error

parse_ds_have_comma:
    call get_token
    call parse_number_or_char
    add [rb - 2], 0, [rb + data]

    add [current_address], [rb + count], [current_address]

    eq  [token_type], '$', [rb + tmp]
    jnz [rb + tmp], parse_ds_loop

    add err_expect_eol, 0, [rb]
    call report_error

parse_ds_loop:
    add [rb + data], 0, [rb - 1]
    arb -1
    call set_mem

    add [rb + count], -1, [rb + count]
    lt  0, [rb + count], [rb + tmp]
    jnz [rb + tmp], parse_ds_loop

    arb 3
    ret 0
.ENDFRAME

##########
parse_dir_eof:
.FRAME
    arb -0

    # detect missing .ENDFRAME
    jz  [is_frame], parse_dir_eof_done

    add err_expect_endframe, 0, [rb + 0]
    call report_error

parse_dir_eof_done:
    arb 0
    ret 0
.ENDFRAME

##########
parse_dir_eoi:
.FRAME
    arb -0

    # detect missing .ENDFRAME
    jz  [is_frame], parse_dir_eoi_done

    add err_expect_endframe, 0, [rb + 0]
    call report_error

parse_dir_eoi_done:
    # end of included file, reset lexer position
    call start_new_file

    arb 0
    ret 0
.ENDFRAME

##########
# globals

# current instruction pointer
current_address:
    db 0

# dummy symbol identifier used to store non-symbol related relocations
relocation_symbol:
    db  "", 0

##########
# error messages

err_unexpected_token:
    db  "Unexpected token", 0
err_expect_comma:
    db  "Expecting a comma", 0
err_expect_eol:
    db  "Unexpected extra tokens on line", 0
err_expect_comma_eol:
    db  "Expecting a comma or a line end", 0
err_expect_number:
    db  "Expecting a number", 0
err_expect_identifier:
    db  "Expecting an identifier", 0
err_expect_endframe:
    db  "Expecting .ENDFRAME", 0

.EOF
