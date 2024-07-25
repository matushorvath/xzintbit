.EXPORT parse_out_param
.EXPORT parse_in_param
.EXPORT parse_value
.EXPORT parse_number_or_char

# from child.s
.IMPORT add_or_find_current_child_symbol

# from error.s
.IMPORT report_error

# from fixup.s
.IMPORT add_fixup

# from frame.s
.IMPORT find_frame_symbol

# from global.s
.IMPORT add_or_find_global_symbol

# from lexer.s
.IMPORT get_token
.IMPORT token_type
.IMPORT token_value
.IMPORT token_line_num
.IMPORT token_column_num

# from parser.s
.IMPORT current_address

# from libxib/heap.s
.IMPORT free

##########
parse_out_param:
.FRAME param_offset, instruction_length; result, mode, sign, tmp
    arb -4

    # default is position mode, unless we see a 'rb'
    add 0, 0, [rb + result]
    add 0, 0, [rb + mode]
    add 1, 0, [rb + sign]

    eq  [token_type], '[', [rb + tmp]
    jnz [rb + tmp], parse_out_param_try_rb

    add err_expect_open_brace, 0, [rb]
    call report_error

parse_out_param_try_rb:
    call get_token
    eq  [token_type], 'P', [rb + tmp]
    jz  [rb + tmp], parse_out_param_after_rb

    # rb means relative mode
    add 2, 0, [rb + mode]

    call get_token
    eq  [token_type], '+', [rb + tmp]
    jnz [rb + tmp], parse_out_param_rb_plus
    eq  [token_type], '-', [rb + tmp]
    jnz [rb + tmp], parse_out_param_rb_minus

    # if there is no +/i, this is just a plain [rb]
    jz  0, parse_out_param_after_value

parse_out_param_rb_minus:
    add -1, 0, [rb + sign]

parse_out_param_rb_plus:
    call get_token

parse_out_param_after_rb:
    add [rb + param_offset], 0, [rb - 1]
    add [rb + instruction_length], 0, [rb - 2]
    eq  [rb + mode], 0, [rb - 3]  # global symbols
    eq  [rb + mode], 2, [rb - 4]  # frame symbols
    arb -4
    call parse_value
    mul [rb - 6], [rb + sign], [rb + result]

    # we don't support 'rb - symbol', the fixup is always positive
    eq  [rb - 7], 1, [rb + tmp]
    jz  [rb + tmp], parse_out_param_after_value

    eq  [rb + sign], -1, [rb + tmp]
    jz  [rb + tmp], parse_out_param_after_value

    add err_subtract_symbol, 0, [rb]
    call report_error

parse_out_param_after_value:
    eq  [token_type], ']', [rb + tmp]
    jnz [rb + tmp], parse_out_param_done

    add err_expect_close_brace, 0, [rb]
    call report_error

parse_out_param_done:
    call get_token

    arb 4
    ret 2
.ENDFRAME

##########
parse_in_param:
.FRAME param_offset, instruction_length; result, mode, tmp
    arb -3

    # position and relative are handled same as out_param
    eq  [token_type], '[', [rb + tmp]
    jz  [rb + tmp], parse_in_param_immediate

    add [rb + param_offset], 0, [rb - 1]
    add [rb + instruction_length], 0, [rb - 2]
    arb -2
    call parse_out_param

    add [rb - 4], 0, [rb + result]
    add [rb - 5], 0, [rb + mode]
    jz  0, parse_in_param_done

parse_in_param_immediate:
    add [rb + param_offset], 0, [rb - 1]
    add [rb + instruction_length], 0, [rb - 2]
    add 1, 0, [rb - 3]  # global symbols
    add 1, 0, [rb - 4]  # frame symbols
    arb -4
    call parse_value

    # return the value and immediate mode
    add [rb - 6], 0, [rb + result]
    add 1, 0, [rb + mode]

parse_in_param_done:
    arb 3
    ret 2
.ENDFRAME

##########
parse_value:
.FRAME param_offset, instruction_length, allow_global_symbol, allow_frame_symbol; result, has_symbol, sign, tmp
    # param_offset: offset of the current parameter from current_address
    # instruction_length: current instruction length
    arb -4

    add 0, 0, [rb + result]
    add 0, 0, [rb + has_symbol]

    eq  [token_type], '+', [rb + tmp]
    jnz [rb + tmp], parse_value_number_or_char_1
    eq  [token_type], '-', [rb + tmp]
    jnz [rb + tmp], parse_value_number_or_char_1
    eq  [token_type], 'n', [rb + tmp]
    jnz [rb + tmp], parse_value_number_or_char_1
    eq  [token_type], 'c', [rb + tmp]
    jnz [rb + tmp], parse_value_number_or_char_1
    eq  [token_type], 'i', [rb + tmp]
    jnz [rb + tmp], parse_value_identifier
    eq  [token_type], 'd', [rb + tmp]
    jnz [rb + tmp], parse_value_dot_identifier
    eq  [token_type], 'I', [rb + tmp]
    jnz [rb + tmp], parse_value_ip

    add err_expect_number_char_identifier, 0, [rb]
    call report_error

parse_value_number_or_char_1:
    call parse_number_or_char
    add [rb - 2], 0, [rb + result]
    jz  0, parse_value_done

parse_value_identifier:
    add 1, 0, [rb + has_symbol]

    # check if this is a frame symbol, we can resolve those immediately
    add [token_value], 0, [rb - 1]
    arb -1
    call find_frame_symbol

    jz  [rb - 3], parse_value_is_global

    # it is a frame symbol
    jnz [rb + allow_frame_symbol], parse_value_frame_symbol_allowed

    add err_frame_symbol_not_allowed, 0, [rb]
    call report_error

parse_value_frame_symbol_allowed:
    # read its offset
    add [rb - 3], FRAME_OFFSET, [ip + 1]
    add [0], 0, [rb + result]

    # free the identifier, it is no longer needed
    add [token_value], 0, [rb - 1]
    arb -1
    call free
    add 0, 0, [token_value]

    call get_token

    jz  0, parse_value_after_symbol

parse_value_is_global:
    # it is a global symbol
    jnz [rb + allow_global_symbol], parse_value_global_symbol_allowed

    add err_global_symbol_not_allowed, 0, [rb]
    call report_error

parse_value_global_symbol_allowed:
    # add or retrieve symbol from the symbol table
    add [token_value], 0, [rb - 1]
    arb -1
    call add_or_find_global_symbol
    add [rb - 3], 0, [rb - 1]           # result of add_or_find_global_symbol -> first param of add_fixup
    add 0, 0, [token_value]             # token_value is now owned by add_or_find_global_symbol

    # is the global symbol followed by dot and a child symbol?
    # TODO implement global.child symbol
    # TODO move get_token here, check if it is ':' or 'd'; before get_token save token_line_num and token_column_num for add_fixup

    # add a fixup for this identifier
    add [current_address], [rb + param_offset], [rb - 2]
    add [token_line_num], 0, [rb - 3]
    add [token_column_num], 0, [rb - 4]
    arb -4
    call add_fixup

    call get_token

    jz  0, parse_value_after_symbol

parse_value_dot_identifier:
    add 1, 0, [rb + has_symbol]

    # child symbols are global symbols
    jnz [rb + allow_global_symbol], parse_value_dot_global_symbol_allowed

    add err_global_symbol_not_allowed, 0, [rb]
    call report_error

parse_value_dot_global_symbol_allowed:
    # add or retrieve symbol from the child table for the current global symbol
    add [token_value], 0, [rb - 1]
    arb -1
    call add_or_find_current_child_symbol
    add [rb - 3], 0, [rb - 1]           # result of add_or_find_current_child_symbol -> first param of add_fixup
    add 0, 0, [token_value]             # token_value is now owned by add_or_find_global_symbol

    # add a fixup for this identifier
    add [current_address], [rb + param_offset], [rb - 2]
    add [token_line_num], 0, [rb - 3]
    add [token_column_num], 0, [rb - 4]
    arb -4
    call add_fixup

    call get_token

    jz  0, parse_value_after_symbol

parse_value_ip:
    # [ip + 123] behaves similarly to [symbol + 123]
    add 1, 0, [rb + has_symbol]
    add [current_address], [rb + instruction_length], [rb + result]

    # retrieve relocation symbol from the symbol table
    add 0, 0, [rb - 1]
    arb -1
    call add_or_find_global_symbol
    add [rb - 3], 0, [rb - 1]           # result of add_or_find_global_symbol -> first param of add_fixup

    # add a fixup (actually a relocation) for the use of current_address
    add [current_address], [rb + param_offset], [rb - 2]
    add [token_line_num], 0, [rb - 3]
    add [token_column_num], 0, [rb - 4]
    arb -4
    call add_fixup

    call get_token

parse_value_after_symbol:
    # optionally followed by + or - and a number or char
    eq  [token_type], '+', [rb + tmp]
    jnz [rb + tmp], parse_value_identifier_plus
    eq  [token_type], '-', [rb + tmp]
    jnz [rb + tmp], parse_value_identifier_minus
    jz  0, parse_value_done

parse_value_identifier_plus:
    add 1, 0, [rb + sign]
    jz  0, parse_value_identifier_after_sign

parse_value_identifier_minus:
    add -1, 0, [rb + sign]
    jz  0, parse_value_identifier_after_sign

parse_value_identifier_after_sign:
    call get_token

    # technically this is also valid: [abcd + -2] or even [abcd + +2]
    eq  [token_type], '+', [rb + tmp]
    jnz [rb + tmp], parse_value_number_or_char_2
    eq  [token_type], '-', [rb + tmp]
    jnz [rb + tmp], parse_value_number_or_char_2
    eq  [token_type], 'n', [rb + tmp]
    jnz [rb + tmp], parse_value_number_or_char_2
    eq  [token_type], 'c', [rb + tmp]
    jnz [rb + tmp], parse_value_number_or_char_2

    add err_expect_number_char, 0, [rb]
    call report_error

parse_value_number_or_char_2:
    call parse_number_or_char
    mul [rb - 2], [rb + sign], [rb + tmp]
    add [rb + result], [rb + tmp], [rb + result]
    jz  0, parse_value_done

parse_value_done:
    arb 4
    ret 4
.ENDFRAME

##########
parse_number_or_char:
.FRAME result, sign, tmp
    arb -3

    add 1, 0, [rb + sign]

    # is there a sign?
    eq  [token_type], '+', [rb + tmp]
    jnz [rb + tmp], parse_number_or_char_plus
    eq  [token_type], '-', [rb + tmp]
    jnz [rb + tmp], parse_number_or_char_minus
    jz  0, parse_number_or_char_after_sign

parse_number_or_char_minus:
    add -1, 0, [rb + sign]

parse_number_or_char_plus:
    call get_token

parse_number_or_char_after_sign:
    eq  [token_type], 'n', [rb + tmp]
    jnz [rb + tmp], parse_number_or_char_have_value
    eq  [token_type], 'c', [rb + tmp]
    jnz [rb + tmp], parse_number_or_char_have_value

    add err_expect_number_char, 0, [rb]
    call report_error

parse_number_or_char_have_value:
    # return the number/char value
    mul [token_value], [rb + sign], [rb + result]
    call get_token

    arb 3
    ret 0
.ENDFRAME

##########
# error messages

err_expect_open_brace:
    db  "Expecting an opening brace", 0
err_expect_close_brace:
    db  "Expecting a closing brace", 0
err_subtract_symbol:
    db  "Symbol value can't be subtracted", 0
err_expect_number_char_identifier:
    db  "Expecting a number, character or identifier", 0
err_expect_number_char:
    db  "Expecting a number or character", 0
err_global_symbol_not_allowed:
    db  "Global symbol is not allowed here", 0
err_frame_symbol_not_allowed:
    db  "Frame symbol is not allowed here", 0

.EOF
