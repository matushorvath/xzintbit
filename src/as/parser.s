.EXPORT parse
.EXPORT current_address

.EXPORT err_expect_comma
.EXPORT err_expect_eol
.EXPORT err_expect_identifier
.EXPORT err_expect_number

# from error.s
.IMPORT report_error

# from lexer.s
.IMPORT get_token
.IMPORT token_type

# from parse_call_ret.s
.IMPORT parse_call
.IMPORT parse_ret

# from parse_data.s
.IMPORT parse_db
.IMPORT parse_ds

# from parse_eof_eoi.s
.IMPORT parse_dir_eof
.IMPORT parse_dir_eoi

# from parse_frame.s
.IMPORT parse_dir_frame
.IMPORT parse_dir_endframe

# from parse_instruction.s
.IMPORT parse_add_mul_lt_eq
.IMPORT parse_jnz_jz
.IMPORT parse_arb_out
.IMPORT parse_in
.IMPORT parse_hlt

# from parse_symbol.s
.IMPORT parse_symbol
.IMPORT parse_dir_symbol
.IMPORT parse_dir_import_export

##########
parse:
.FRAME tmp
    arb -1

.loop:
    call get_token

    # skip empty lines
    eq  [token_type], '$', [rb + tmp]
    jnz [rb + tmp], .loop

    # instructions
    eq  [token_type], 1, [rb + tmp]
    jnz [rb + tmp], .call_add_mul_lt_eq
    eq  [token_type], 2, [rb + tmp]
    jnz [rb + tmp], .call_add_mul_lt_eq
    eq  [token_type], 7, [rb + tmp]
    jnz [rb + tmp], .call_add_mul_lt_eq
    eq  [token_type], 8, [rb + tmp]
    jnz [rb + tmp], .call_add_mul_lt_eq

    eq  [token_type], 5, [rb + tmp]
    jnz [rb + tmp], .call_jnz_jz
    eq  [token_type], 6, [rb + tmp]
    jnz [rb + tmp], .call_jnz_jz

    eq  [token_type], 9, [rb + tmp]
    jnz [rb + tmp], .call_arb_out
    eq  [token_type], 4, [rb + tmp]
    jnz [rb + tmp], .call_arb_out

    eq  [token_type], 3, [rb + tmp]
    jnz [rb + tmp], .call_in
    eq  [token_type], 99, [rb + tmp]
    jnz [rb + tmp], .call_hlt

    # pseudo-instructions
    eq  [token_type], 'C', [rb + tmp]
    jnz [rb + tmp], .call_call
    eq  [token_type], 'R', [rb + tmp]
    jnz [rb + tmp], .call_ret
    eq  [token_type], 'B', [rb + tmp]
    jnz [rb + tmp], .call_db
    eq  [token_type], 'S', [rb + tmp]
    jnz [rb + tmp], .call_ds

    # symbols
    eq  [token_type], 'i', [rb + tmp]
    jnz [rb + tmp], .call_symbol
    eq  [token_type], 'd', [rb + tmp]
    jnz [rb + tmp], .call_symbol
    eq  [token_type], '+', [rb + tmp]
    jnz [rb + tmp], .call_symbol

    # directives
    eq  [token_type], 'F', [rb + tmp]
    jnz [rb + tmp], .call_directive_frame
    eq  [token_type], 'D', [rb + tmp]
    jnz [rb + tmp], .call_directive_endframe
    eq  [token_type], 'Y', [rb + tmp]
    jnz [rb + tmp], .call_directive_symbol
    eq  [token_type], 'E', [rb + tmp]
    jnz [rb + tmp], .call_directive_import_export
    eq  [token_type], 'I', [rb + tmp]
    jnz [rb + tmp], .call_directive_import_export
    eq  [token_type], 'N', [rb + tmp]
    jnz [rb + tmp], .call_directive_eof
    eq  [token_type], 'O', [rb + tmp]
    jnz [rb + tmp], .call_directive_eoi

    add err_unexpected_token, 0, [rb]
    call report_error

.call_add_mul_lt_eq:
    call parse_add_mul_lt_eq
    jz  0, .loop

.call_jnz_jz:
    call parse_jnz_jz
    jz  0, .loop

.call_arb_out:
    call parse_arb_out
    jz  0, .loop

.call_in:
    call parse_in
    jz  0, .loop

.call_hlt:
    call parse_hlt
    jz  0, .loop

.call_call:
    call parse_call
    jz  0, .loop

.call_ret:
    call parse_ret
    jz  0, .loop

.call_db:
    call parse_db
    jz  0, .loop

.call_ds:
    call parse_ds
    jz  0, .loop

.call_symbol:
    call parse_symbol
    jz  0, .loop

.call_directive_frame:
    call parse_dir_frame
    jz  0, .loop

.call_directive_endframe:
    call parse_dir_endframe
    jz  0, .loop

.call_directive_symbol:
    call parse_dir_symbol
    jz  0, .loop

.call_directive_import_export:
    call parse_dir_import_export
    jz  0, .loop

.call_directive_eof:
    call parse_dir_eof
    jz  0, .done

.call_directive_eoi:
    call parse_dir_eoi
    jz  0, .loop

.done:
    arb 1
    ret 0
.ENDFRAME

##########
# globals

# current instruction pointer
current_address:
    db  0

##########
# error messages

err_unexpected_token:
    db  "Unexpected token", 0
err_expect_comma:
    db  "Expecting a comma", 0
err_expect_eol:
    db  "Unexpected extra tokens on line", 0
err_expect_number:
    db  "Expecting a number", 0
err_expect_identifier:
    db  "Expecting an identifier", 0

.EOF
