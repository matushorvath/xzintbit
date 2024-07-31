.EXPORT parse_db
.EXPORT parse_ds

# from libxib/heap.s
.IMPORT free

# from error.s
.IMPORT report_error

# from lexer.s
.IMPORT get_token
.IMPORT token_type
.IMPORT token_value

# from memory.s
.IMPORT set_as_mem
.IMPORT set_as_mem_str

# from parse_param.s
.IMPORT parse_value
.IMPORT parse_number_or_char

# from parser.s
.IMPORT current_address
.IMPORT err_expect_eol
.IMPORT err_expect_comma
.IMPORT err_expect_number

##########
parse_db:
.FRAME tmp, data, offset
    arb -3

    add 0, 0, [rb + offset]

.loop:
    # eat the 'db' token (first parameter) or the comma (subsequent parameters)
    call get_token

    eq  [token_type], 's', [rb + tmp]
    jnz [rb + tmp], .string

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
    call set_as_mem

    add [rb + offset], 1, [rb + offset]
    jz  0, .after_param

.string:
    # store the string, don't store zero termination
    add [token_value], 0, [rb - 1]
    arb -1
    call set_as_mem_str

    # string length is returned by set_as_mem_str
    add [rb + offset], [rb - 3], [rb + offset]

    # free the string
    add [token_value], 0, [rb - 1]
    arb -1
    call free
    add 0, 0, [token_value]

    call get_token

.after_param:
    eq  [token_type], ',', [rb + tmp]
    jnz [rb + tmp], .loop
    eq  [token_type], '$', [rb + tmp]
    jnz [rb + tmp], .done

    add err_expect_comma_eol, 0, [rb]
    call report_error

.done:
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
    jnz [rb + tmp], .have_count

    add err_expect_number, 0, [rb]
    call report_error

.have_count:
    add [token_value], 0, [rb + count]
    call get_token

    eq  [token_type], ',', [rb + tmp]
    jnz [rb + tmp], .have_comma

    add err_expect_comma, 0, [rb]
    call report_error

.have_comma:
    call get_token
    call parse_number_or_char
    add [rb - 2], 0, [rb + data]

    add [current_address], [rb + count], [current_address]

    eq  [token_type], '$', [rb + tmp]
    jnz [rb + tmp], .loop

    add err_expect_eol, 0, [rb]
    call report_error

.loop:
    add [rb + data], 0, [rb - 1]
    arb -1
    call set_as_mem

    add [rb + count], -1, [rb + count]
    lt  0, [rb + count], [rb + tmp]
    jnz [rb + tmp], .loop

    arb 3
    ret 0
.ENDFRAME

##########
# error messages

err_expect_comma_eol:
    db  "Expecting a comma or a line end", 0

.EOF
