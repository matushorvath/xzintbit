.EXPORT parse_add_mul_lt_eq
.EXPORT parse_jnz_jz
.EXPORT parse_arb_out
.EXPORT parse_in
.EXPORT parse_hlt

# from error.s
.IMPORT report_error

# from lexer.s
.IMPORT get_token
.IMPORT token_type

# from memory.s
.IMPORT set_as_mem

# from parse_param.s
.IMPORT parse_out_param
.IMPORT parse_in_param

# from parser.s
.IMPORT current_address
.IMPORT err_expect_comma
.IMPORT err_expect_eol

##########
parse_add_mul_lt_eq:
.FRAME tmp, op, param0, param1, param2
    arb -5

    # token type is conveniently also the op code
    add [token_type], 0, [rb + op]
    call get_token

    add 1, 0, [rb - 1]
    add 4, 0, [rb - 2]
    arb -2
    call parse_in_param

    # update opcode and param 0 value
    mul [rb - 5], 100, [rb - 5]
    add [rb + op], [rb - 5], [rb + op]
    add [rb - 4], 0, [rb + param0]

    eq  [token_type], ',', [rb + tmp]
    jnz [rb + tmp], .param1

    add err_expect_comma, 0, [rb]
    call report_error

.param1:
    call get_token

    add 2, 0, [rb - 1]
    add 4, 0, [rb - 2]
    arb -2
    call parse_in_param

    # update opcode and param 1 value
    mul [rb - 5], 1000, [rb - 5]
    add [rb + op], [rb - 5], [rb + op]
    add [rb - 4], 0, [rb + param1]

    eq  [token_type], ',', [rb + tmp]
    jnz [rb + tmp], .param2

    add err_expect_comma, 0, [rb]
    call report_error

.param2:
    call get_token

    add 3, 0, [rb - 1]
    add 4, 0, [rb - 2]
    arb -2
    call parse_out_param

    # update opcode and param 2 value
    mul [rb - 5], 10000, [rb - 5]
    add [rb + op], [rb - 5], [rb + op]
    add [rb - 4], 0, [rb + param2]

    eq  [token_type], '$', [rb + tmp]
    jnz [rb + tmp], .done

    add err_expect_eol, 0, [rb]
    call report_error

.done:
    add [current_address], 4, [current_address]

    add [rb + op], 0, [rb - 1]
    arb -1
    call set_as_mem

    add [rb + param0], 0, [rb - 1]
    arb -1
    call set_as_mem

    add [rb + param1], 0, [rb - 1]
    arb -1
    call set_as_mem

    add [rb + param2], 0, [rb - 1]
    arb -1
    call set_as_mem

    arb 5
    ret 0
.ENDFRAME

##########
parse_jnz_jz:
.FRAME tmp, op, param0, param1
    arb -4

    # token type is conveniently also the op code
    add [token_type], 0, [rb + op]
    call get_token

    add 1, 0, [rb - 1]
    add 3, 0, [rb - 2]
    arb -2
    call parse_in_param

    # update opcode and param 0 value
    mul [rb - 5], 100, [rb - 5]
    add [rb + op], [rb - 5], [rb + op]
    add [rb - 4], 0, [rb + param0]

    eq  [token_type], ',', [rb + tmp]
    jnz [rb + tmp], .param1

    add err_expect_comma, 0, [rb]
    call report_error

.param1:
    call get_token

    add 2, 0, [rb - 1]
    add 3, 0, [rb - 2]
    arb -2
    call parse_in_param

    # update opcode and param 1 value
    mul [rb - 5], 1000, [rb - 5]
    add [rb + op], [rb - 5], [rb + op]
    add [rb - 4], 0, [rb + param1]

    eq  [token_type], '$', [rb + tmp]
    jnz [rb + tmp], .done

    add err_expect_eol, 0, [rb]
    call report_error

.done:
    add [current_address], 3, [current_address]

    add [rb + op], 0, [rb - 1]
    arb -1
    call set_as_mem

    add [rb + param0], 0, [rb - 1]
    arb -1
    call set_as_mem

    add [rb + param1], 0, [rb - 1]
    arb -1
    call set_as_mem

    arb 4
    ret 0
.ENDFRAME

##########
parse_arb_out:
.FRAME tmp, op, param0
    arb -3

    # token type is conveniently also the op code
    add [token_type], 0, [rb + op]
    call get_token

    add 1, 0, [rb - 1]
    add 2, 0, [rb - 2]
    arb -2
    call parse_in_param

    # update opcode and param 0 value
    mul [rb - 5], 100, [rb - 5]
    add [rb + op], [rb - 5], [rb + op]
    add [rb - 4], 0, [rb + param0]

    eq  [token_type], '$', [rb + tmp]
    jnz [rb + tmp], .done

    add err_expect_eol, 0, [rb]
    call report_error

.done:
    add [current_address], 2, [current_address]

    add [rb + op], 0, [rb - 1]
    arb -1
    call set_as_mem

    add [rb + param0], 0, [rb - 1]
    arb -1
    call set_as_mem

    arb 3
    ret 0
.ENDFRAME

##########
parse_in:
.FRAME tmp, op, param0
    arb -3

    # token type is conveniently also the op code
    add [token_type], 0, [rb + op]
    call get_token

    add 1, 0, [rb - 1]
    add 2, 0, [rb - 2]
    arb -2
    call parse_out_param

    # update opcode and param 0 value
    mul [rb - 5], 100, [rb - 5]
    add [rb + op], [rb - 5], [rb + op]
    add [rb - 4], 0, [rb + param0]

    eq  [token_type], '$', [rb + tmp]
    jnz [rb + tmp], .done

    add err_expect_eol, 0, [rb]
    call report_error

.done:
    add [current_address], 2, [current_address]

    add [rb + op], 0, [rb - 1]
    arb -1
    call set_as_mem

    add [rb + param0], 0, [rb - 1]
    arb -1
    call set_as_mem

    arb 3
    ret 0
.ENDFRAME

##########
parse_hlt:
.FRAME tmp, op
    arb -2

    # token type is conveniently also the op code
    add [token_type], 0, [rb + op]
    call get_token

    eq  [token_type], '$', [rb + tmp]
    jnz [rb + tmp], .done

    add err_expect_eol, 0, [rb]
    call report_error

.done:
    add [current_address], 1, [current_address]

    add [rb + op], 0, [rb - 1]
    arb -1
    call set_as_mem

    arb 2
    ret 0
.ENDFRAME

.EOF
