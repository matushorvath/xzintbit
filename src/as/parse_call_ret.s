.EXPORT parse_call
.EXPORT parse_ret

# from error.s
.IMPORT report_error

# from fixup.s
.IMPORT add_fixup

# from global.s
.IMPORT current_address_symbol

# from lexer.s
.IMPORT get_token
.IMPORT token_type
.IMPORT token_line_num
.IMPORT token_column_num

# from memory.s
.IMPORT set_as_mem

# from parse_param.s
.IMPORT parse_in_param
.IMPORT parse_number_or_char

# from parser.s
.IMPORT current_address
.IMPORT err_expect_eol

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
    call set_as_mem

    add [current_address], 9, [rb - 1]
    arb -1
    call set_as_mem

    # add a fixup (actually a relocation) for the use of current_address
    add [current_address_symbol], GLOBAL_FIXUPS_HEAD, [rb - 1]
    add [current_address], 1, [rb - 2]
    add [token_line_num], 0, [rb - 3]
    add [token_column_num], 0, [rb - 4]
    arb -4
    call add_fixup

    add 0, 0, [rb - 1]
    arb -1
    call set_as_mem

    add -1, 0, [rb - 1]
    arb -1
    call set_as_mem

    # generate code: arb -1
    # 109, -1
    add 109, 0, [rb - 1]
    arb -1
    call set_as_mem

    add -1, 0, [rb - 1]
    arb -1
    call set_as_mem

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
    call set_as_mem

    add 0, 0, [rb - 1]
    arb -1
    call set_as_mem

    # if mode is 2 (relative), we need to adjust param by +1, to compensate
    # for the 'arb -1' that we just performed one instruction before this one
    # see also test/call_local_variable
    eq  [rb + mode], 2, [rb + tmp]
    jz  [rb + tmp], .not_mode_3

    add [rb + param], 1, [rb + param]

.not_mode_3:
    add [rb + param], 0, [rb - 1]
    arb -1
    call set_as_mem

    eq  [token_type], '$', [rb + tmp]
    jnz [rb + tmp], .done

    add err_expect_eol, 0, [rb]
    call report_error

.done:
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
    call set_as_mem

    add [rb + param], 1, [rb - 1]
    arb -1
    call set_as_mem

    # generate code: jz 0, [rb - ($param + 1)]
    # 2106, 0, -(param + 1)
    add 2106, 0, [rb - 1]
    arb -1
    call set_as_mem

    add 0, 0, [rb - 1]
    arb -1
    call set_as_mem

    add [rb + param], 1, [rb + tmp]
    mul [rb + tmp], -1, [rb - 1]
    arb -1
    call set_as_mem

    eq  [token_type], '$', [rb + tmp]
    jnz [rb + tmp], .done

    add err_expect_eol, 0, [rb]
    call report_error

.done:
    add [current_address], 5, [current_address]

    arb 2
    ret 0
.ENDFRAME

.EOF
