# TODO:
# validations: invalid use of global and frame symbols (e.g. [local] or [rb + global])
# return address frame symbol?
# allocate less than 50 for fixups
# have a printf-like function to print more info about errors
# check the typescript as implementation for missing error handling
# test: both global and frame symbol; access frame outside of frame
# compile-time constants: .SYMBOL abc = 42, use instead of 50

# token types:
# 1 add; 9 arb; 8 eq; 99 hlt; 3 in; 5 jnz; 6 jz; 7 lt; 2 mul; 4 out
# C call; R ret; B db; S ds
# F .FRAME; D .ENDFRAME; N EOF; $ EOL; P rb; I ip
# + - = , : ; [ ]
# n [0-9]+; i [a-zA-Z_][a-zA-Z0-9_]*; c '.'; s ".*"

##########
    arb stack

##########
parse:
.FRAME tmp
    arb -1

parse_loop:
    call get_token

    # skip empty lines
    eq  [token], '$', [rb + tmp]
    jnz [rb + tmp], parse_loop

    # instructions
    eq  [token], 1, [rb + tmp]
    jnz [rb + tmp], parse_call_add_mul_lt_eq
    eq  [token], 2, [rb + tmp]
    jnz [rb + tmp], parse_call_add_mul_lt_eq
    eq  [token], 7, [rb + tmp]
    jnz [rb + tmp], parse_call_add_mul_lt_eq
    eq  [token], 8, [rb + tmp]
    jnz [rb + tmp], parse_call_add_mul_lt_eq

    eq  [token], 5, [rb + tmp]
    jnz [rb + tmp], parse_call_jnz_jz
    eq  [token], 6, [rb + tmp]
    jnz [rb + tmp], parse_call_jnz_jz

    eq  [token], 9, [rb + tmp]
    jnz [rb + tmp], parse_call_arb_out
    eq  [token], 4, [rb + tmp]
    jnz [rb + tmp], parse_call_arb_out

    eq  [token], 3, [rb + tmp]
    jnz [rb + tmp], parse_call_in
    eq  [token], 99, [rb + tmp]
    jnz [rb + tmp], parse_call_hlt

    # pseudo-instructions
    eq  [token], 'C', [rb + tmp]
    jnz [rb + tmp], parse_call_call
    eq  [token], 'R', [rb + tmp]
    jnz [rb + tmp], parse_call_ret
    eq  [token], 'B', [rb + tmp]
    jnz [rb + tmp], parse_call_db
    eq  [token], 'S', [rb + tmp]
    jnz [rb + tmp], parse_call_ds

    # symbols
    eq  [token], 'i', [rb + tmp]
    jnz [rb + tmp], parse_call_symbol
    eq  [token], '+', [rb + tmp]
    jnz [rb + tmp], parse_call_symbol

    # directives
    eq  [token], 'F', [rb + tmp]
    jnz [rb + tmp], parse_call_directive_frame
    eq  [token], 'D', [rb + tmp]
    jnz [rb + tmp], parse_call_directive_endframe
    eq  [token], 'N', [rb + tmp]
    jnz [rb + tmp], parse_call_directive_eof

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

parse_call_directive_eof:
    call parse_dir_eof
    jz  0, parse_loop
.ENDFRAME

##########
parse_add_mul_lt_eq:
.FRAME tmp, op, param0, param1, param2
    arb -5

    # token type is conveniently also the op code
    add [token], 0, [rb + op]
    call get_token

    add 1, 0, [rb - 1]
    add 4, 0, [rb - 2]
    arb -2
    call parse_in_param

    # update opcode and param 0 value
    mul [rb - 5], 100, [rb - 5]
    add [rb + op], [rb - 5], [rb + op]
    add [rb - 4], 0, [rb + param0]

    eq  [token], ',', [rb + tmp]
    jnz [rb + tmp], parse_add_mul_lt_eq_param1

    add err_expect_comma, 0, [rb]
    call report_error

parse_add_mul_lt_eq_param1:
    call get_token

    add 2, 0, [rb - 1]
    add 4, 0, [rb - 2]
    arb -2
    call parse_in_param

    # update opcode and param 1 value
    mul [rb - 5], 1000, [rb - 5]
    add [rb + op], [rb - 5], [rb + op]
    add [rb - 4], 0, [rb + param1]

    eq  [token], ',', [rb + tmp]
    jnz [rb + tmp], parse_add_mul_lt_eq_param2

    add err_expect_comma, 0, [rb]
    call report_error

parse_add_mul_lt_eq_param2:
    call get_token

    add 3, 0, [rb - 1]
    add 4, 0, [rb - 2]
    arb -2
    call parse_out_param

    # update opcode and param 2 value
    mul [rb - 5], 10000, [rb - 5]
    add [rb + op], [rb - 5], [rb + op]
    add [rb - 4], 0, [rb + param2]

    eq  [token], '$', [rb + tmp]
    jnz [rb + tmp], parse_add_mul_lt_eq_done

    add err_expect_eol, 0, [rb]
    call report_error

parse_add_mul_lt_eq_done:
    add [current_address], 4, [current_address]

    add [rb + op], 0, [rb - 1]
    arb -1
    call set_mem

    add [rb + param0], 0, [rb - 1]
    arb -1
    call set_mem

    add [rb + param1], 0, [rb - 1]
    arb -1
    call set_mem

    add [rb + param2], 0, [rb - 1]
    arb -1
    call set_mem

    arb 5
    ret 0
.ENDFRAME

##########
parse_jnz_jz:
.FRAME tmp, op, param0, param1
    arb -4

    # token type is conveniently also the op code
    add [token], 0, [rb + op]
    call get_token

    add 1, 0, [rb - 1]
    add 3, 0, [rb - 2]
    arb -2
    call parse_in_param

    # update opcode and param 0 value
    mul [rb - 5], 100, [rb - 5]
    add [rb + op], [rb - 5], [rb + op]
    add [rb - 4], 0, [rb + param0]

    eq  [token], ',', [rb + tmp]
    jnz [rb + tmp], parse_jnz_jz_param1

    add err_expect_comma, 0, [rb]
    call report_error

parse_jnz_jz_param1:
    call get_token

    add 2, 0, [rb - 1]
    add 3, 0, [rb - 2]
    arb -2
    call parse_in_param

    # update opcode and param 1 value
    mul [rb - 5], 1000, [rb - 5]
    add [rb + op], [rb - 5], [rb + op]
    add [rb - 4], 0, [rb + param1]

    eq  [token], '$', [rb + tmp]
    jnz [rb + tmp], parse_jnz_jz_done

    add err_expect_eol, 0, [rb]
    call report_error

parse_jnz_jz_done:
    add [current_address], 3, [current_address]

    add [rb + op], 0, [rb - 1]
    arb -1
    call set_mem

    add [rb + param0], 0, [rb - 1]
    arb -1
    call set_mem

    add [rb + param1], 0, [rb - 1]
    arb -1
    call set_mem

    arb 4
    ret 0
.ENDFRAME

##########
parse_arb_out:
.FRAME tmp, op, param0
    arb -3

    # token type is conveniently also the op code
    add [token], 0, [rb + op]
    call get_token

    add 1, 0, [rb - 1]
    add 2, 0, [rb - 2]
    arb -2
    call parse_in_param

    # update opcode and param 0 value
    mul [rb - 5], 100, [rb - 5]
    add [rb + op], [rb - 5], [rb + op]
    add [rb - 4], 0, [rb + param0]

    eq  [token], '$', [rb + tmp]
    jnz [rb + tmp], parse_arb_out_done

    add err_expect_eol, 0, [rb]
    call report_error

parse_arb_out_done:
    add [current_address], 2, [current_address]

    add [rb + op], 0, [rb - 1]
    arb -1
    call set_mem

    add [rb + param0], 0, [rb - 1]
    arb -1
    call set_mem

    arb 3
    ret 0
.ENDFRAME

##########
parse_in:
.FRAME tmp, op, param0
    arb -3

    # token type is conveniently also the op code
    add [token], 0, [rb + op]
    call get_token

    add 1, 0, [rb - 1]
    add 2, 0, [rb - 2]
    arb -2
    call parse_out_param

    # update opcode and param 0 value
    mul [rb - 5], 100, [rb - 5]
    add [rb + op], [rb - 5], [rb + op]
    add [rb - 4], 0, [rb + param0]

    eq  [token], '$', [rb + tmp]
    jnz [rb + tmp], parse_in_done

    add err_expect_eol, 0, [rb]
    call report_error

parse_in_done:
    add [current_address], 2, [current_address]

    add [rb + op], 0, [rb - 1]
    arb -1
    call set_mem

    add [rb + param0], 0, [rb - 1]
    arb -1
    call set_mem

    arb 3
    ret 0
.ENDFRAME

##########
parse_hlt:
.FRAME tmp, op
    arb -2

    # token type is conveniently also the op code
    add [token], 0, [rb + op]
    call get_token

    eq  [token], '$', [rb + tmp]
    jnz [rb + tmp], parse_hlt_done

    add err_expect_eol, 0, [rb]
    call report_error

parse_hlt_done:
    add [current_address], 1, [current_address]

    add [rb + op], 0, [rb - 1]
    arb -1
    call set_mem

    arb 2
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

    eq  [token], '$', [rb + tmp]
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

    eq  [token], '$', [rb + tmp]
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

    eq  [token], 's', [rb + tmp]
    jnz [rb + tmp], parse_db_string

    # not a string, so it must be a value
    add [rb + offset], 0, [rb - 1]
    # next instruction address with db would need a fixup, so we just pass next value address
    add [rb + offset], 1, [rb - 2]
    arb -2
    call parse_value
    add [rb - 4], 0, [rb + data]

    add [rb + data], 0, [rb - 1]
    arb -1
    call set_mem

    add [rb + offset], 1, [rb + offset]
    jz  0, parse_db_after_param

parse_db_string:
    # store the string, don't store zero termination
    add [value], 0, [rb - 1]
    arb -1
    call set_mem_str

    # string length is returned by set_mem_str
    add [rb + offset], [rb - 3], [rb + offset]

    # free the string
    add [value], 0, [rb - 1]
    arb -1
    call free
    add 0, 0, [value]

    call get_token

parse_db_after_param:
    eq  [token], ',', [rb + tmp]
    jnz [rb + tmp], parse_db_loop
    eq  [token], '$', [rb + tmp]
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

    eq  [token], 'n', [rb + tmp]
    jnz [rb + tmp], parse_ds_have_count

    add err_expect_number, 0, [rb]
    call report_error

parse_ds_have_count:
    add [value], 0, [rb + count]
    call get_token

    eq  [token], ',', [rb + tmp]
    jnz [rb + tmp], parse_ds_have_comma

    add err_expect_comma, 0, [rb]
    call report_error

parse_ds_have_comma:
    call get_token
    call parse_number_or_char
    add [rb - 2], 0, [rb + data]

    add [current_address], [rb + count], [current_address]

    eq  [token], '$', [rb + tmp]
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
parse_symbol:
.FRAME tmp, address
    arb -2

    # default symbol address is current_address
    add [current_address], 0, [rb + address]

    # check if there is an offset
    eq  [token], '+', [rb + tmp]
    jz  [rb + tmp], parse_symbol_after_offset

    call get_token
    eq  [token], 'n', [rb + tmp]
    jnz [rb + tmp], parse_symbol_have_offset

    add err_expect_number, 0, [rb]
    call report_error

parse_symbol_have_offset:
    # add offset to symbol address
    add [rb + address], [value], [rb + address]
    call get_token

    eq  [token], '=', [rb + tmp]
    jnz [rb + tmp], parse_symbol_after_equals

    add err_expect_equals, 0, [rb]
    call report_error

parse_symbol_after_equals:
    call get_token

parse_symbol_after_offset:
    eq  [token], 'i', [rb + tmp]
    jnz [rb + tmp], parse_symbol_have_identifier

    add err_expect_identifier, 0, [rb]
    call report_error

parse_symbol_have_identifier:
    # add the symbol to symbol table
    add [value], 0, [rb - 1]
    add [rb + address], 0, [rb - 2]
    arb -2
    call set_global_symbol_address

    # free the symbol value
    add [value], 0, [rb - 1]
    arb -1
    call free
    add 0, 0, [value]

    call get_token

    eq  [token], ':', [rb + tmp]
    jnz [rb + tmp], parse_symbol_done

    add err_expect_colon, 0, [rb]
    call report_error

parse_symbol_done:
    arb 2
    ret 0
.ENDFRAME

##########
parse_dir_frame:
.FRAME tmp, block_index, symbol_count
    arb -3

    add 0, 0, [rb + block_index]

    # no nested frames, only one frame at a time
    # we can't use frame_head, because a frame could have no symbols
    jz  [is_frame], parse_dir_frame_loop

    add err_already_in_frame, 0, [rb]
    call report_error

parse_dir_frame_loop:
    # eat the 'FRAME' token (first parameter block) or the semicolon (subsequent parameter blocks)
    call get_token

    add [rb + block_index], 0, [rb - 1]
    arb -1
    call parse_dir_frame_block
    add [rb - 3], 0, [rb + symbol_count]

    # are there any more blocks, or was this the last one?
    eq  [token], ';', [rb + tmp]
    jnz [rb + tmp], parse_dir_frame_check_count
    eq  [token], '$', [rb + tmp]
    jnz [rb + tmp], parse_dir_frame_eol

    add err_expect_semicolon_eol, 0, [rb]
    call report_error

parse_dir_frame_check_count:
    # we support up to three identifier lists
    add [rb + block_index], 1, [rb + block_index]
    lt  [rb + block_index], 3, [rb + tmp]
    jnz [rb + tmp], parse_dir_frame_loop

    add err_too_many_lists, 0, [rb]
    call report_error

parse_dir_frame_eol:
    # we have up to three identifier blocks stored in the frame list
    # now decide which block represents parameters, local variables and downstream variables
    # then assign offsets to all symbols in the frame
    add [rb + block_index], 1, [rb - 1]
    add [rb + symbol_count], 0, [rb - 2]
    arb -2
    call parse_dir_frame_offset_blocks

    add 1, 0, [is_frame]

    arb 3
    ret 0
.ENDFRAME

##########
parse_dir_frame_block:
.FRAME block_count; symbol_count, tmp
    arb -2

    add 0, 0, [rb + symbol_count]

    # handle empty block
    eq  [token], ';', [rb + tmp]
    jnz [rb + tmp], parse_dir_frame_block_done
    eq  [token], '$', [rb + tmp]
    jnz [rb + tmp], parse_dir_frame_block_done

parse_dir_frame_block_loop:
    eq  [token], 'i', [rb + tmp]
    jnz [rb + tmp], parse_dir_frame_block_identifier

    add err_expect_identifier, 0, [rb]
    call report_error

parse_dir_frame_block_identifier:
    # check for duplicate symbols
    add [value], 0, [rb - 1]
    arb -1
    call find_frame_symbol
    jz  [rb - 3], parse_dir_frame_block_is_unique

    add err_duplicate_symbol, 0, [rb]
    call report_error

parse_dir_frame_block_is_unique:
    # store the symbol with block index
    add [value], 0, [rb - 1]
    add [rb + block_count], 0, [rb - 2]
    arb -2
    call add_frame_symbol

    # free the identifier
    add [value], 0, [rb - 1]
    arb -1
    call free

    add [rb + symbol_count], 1, [rb + symbol_count]
    call get_token

    eq  [token], ',', [rb + tmp]
    jnz [rb + tmp], parse_dir_frame_block_comma
    eq  [token], ';', [rb + tmp]
    jnz [rb + tmp], parse_dir_frame_block_done
    eq  [token], '$', [rb + tmp]
    jnz [rb + tmp], parse_dir_frame_block_done

    add err_expect_comma_semicolon_eol, 0, [rb]
    call report_error

parse_dir_frame_block_comma:
    call get_token
    jz  0, parse_dir_frame_block_loop

parse_dir_frame_block_done:
    arb 2
    ret 1
.ENDFRAME

##########
parse_dir_frame_offset_blocks:
.FRAME block_count, symbol_count; tmp, offset
    arb -2

    # we have stored all frame symbols, now we can assign offsets to them

    # are there any downstream symbols?
    lt  [rb + block_count], 3, [rb + tmp]
    jnz [rb + tmp], parse_dir_frame_offset_blocks_after_downstream

    # process block 2 as downstream variables
    add 2, 0, [rb - 1]
    mul [rb + symbol_count], -1, [rb - 2]
    arb -2
    call parse_dir_frame_offset

parse_dir_frame_offset_blocks_after_downstream:
    # if there is at least one block, that will be local variables
    lt  [rb + block_count], 1, [rb + tmp]
    jnz [rb + tmp], parse_dir_frame_offset_blocks_done

    # process block 0 or 1 as local variables
    lt  1, [rb + block_count], [rb - 1]
    add 0, 0, [rb - 2]
    arb -2
    call parse_dir_frame_offset

    # save ending offset for local variables so we know where to continue
    # we will skip one offset for return value stored on stack
    add [rb - 4], 1, [rb + offset]

    # if there were at least two blocks, the last one is parameters
    lt  1, [rb + block_count], [rb + tmp]
    jz  [rb + tmp], parse_dir_frame_offset_blocks_done

    # process block 0 as parameters
    add 0, 0, [rb - 1]
    add [rb + offset], 0, [rb - 2]
    arb -2
    call parse_dir_frame_offset

parse_dir_frame_offset_blocks_done:
    arb 2
    ret 2
.ENDFRAME

##########
parse_dir_frame_offset:
.FRAME current_block, start_offset; offset, record, record_block, tmp
    arb -4

    add [rb + start_offset], 0, [rb + offset]
    add [frame_head], 0, [rb + record]

parse_dir_frame_offset_loop:
    # are there any more records?
    jz  [rb + record], parse_dir_frame_offset_done

    # read block index field
    add [rb + record], 49, [ip + 1]
    add [0], 0, [rb + record_block]

    # for records where block matches the block we are processing, set offset
    eq  [rb + record_block], [rb + current_block], [rb + tmp]
    jz  [rb + tmp], parse_dir_frame_offset_after_update

    # write offset field
    add [rb + record], 48, [ip + 3]
    add [rb + offset], 0, [0]

    # increment offset for next symbol
    add [rb + offset], 1, [rb + offset]

parse_dir_frame_offset_after_update:
    # move to next record
    add [rb + record], 0, [ip + 1]
    add [0], 0, [rb + record]

    jz  0, parse_dir_frame_offset_loop

parse_dir_frame_offset_done:
    arb 4
    ret 2
.ENDFRAME

##########
parse_dir_endframe:
.FRAME tmp
    arb -1

    # check if we are in fact in a frame
    # we can't use frame_head, because a frame could have no symbols
    eq  [is_frame], 1, [rb + tmp]
    jnz [rb + tmp], parse_dir_endframe_check_params

    add err_not_in_frame, 0, [rb]
    call report_error

parse_dir_endframe_check_params:
    call get_token

    eq  [token], '$', [rb + tmp]
    jnz [rb + tmp], parse_dir_endframe_done

    add err_expect_eol, 0, [rb]
    call report_error

parse_dir_endframe_done:
    call reset_frame
    add  0, 0, [is_frame]

    arb 1
    ret 0
.ENDFRAME

##########
parse_dir_eof:
.FRAME
    # detect missing .ENDFRAME
    jz  [is_frame], parse_dir_eof_have_endframe

    add err_expect_endframe, 0, [rb + 0]
    call report_error

parse_dir_eof_have_endframe:
    # run fixups
    call do_fixups

    # print compiled memory contents
    call print_mem

    hlt
.ENDFRAME

##########
parse_out_param:
.FRAME param_offset, instruction_length; result, mode, sign, tmp
    arb -4

    # default is position mode, unless we see a 'rb'
    add 0, 0, [rb + result]
    add 0, 0, [rb + mode]
    add 1, 0, [rb + sign]

    eq  [token], '[', [rb + tmp]
    jnz [rb + tmp], parse_out_param_try_rb

    add err_expect_open_brace, 0, [rb]
    call report_error

parse_out_param_try_rb:
    call get_token
    eq  [token], 'P', [rb + tmp]
    jz  [rb + tmp], parse_out_param_after_rb

    # rb means relative mode
    add 2, 0, [rb + mode]

    call get_token
    eq  [token], '+', [rb + tmp]
    jnz [rb + tmp], parse_out_param_rb_plus
    eq  [token], '-', [rb + tmp]
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
    arb -2
    call parse_value
    mul [rb - 4], [rb + sign], [rb + result]

    # we don't support 'rb - symbol', the fixup is always positive
    eq  [rb - 5], 1, [rb + tmp]
    jz  [rb + tmp], parse_out_param_after_value

    eq  [rb + sign], -1, [rb + tmp]
    jz  [rb + tmp], parse_out_param_after_value

    add err_subtract_symbol, 0, [rb]
    call report_error

parse_out_param_after_value:
    eq  [token], ']', [rb + tmp]
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
    eq  [token], '[', [rb + tmp]
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
    arb -2
    call parse_value

    # return the value and immediate mode
    add [rb - 4], 0, [rb + result]
    add 1, 0, [rb + mode]

parse_in_param_done:
    arb 3
    ret 2
.ENDFRAME

##########
parse_value:
.FRAME param_offset, instruction_length; result, has_symbol, sign, tmp
    # param_offset: offset of the current parameter from current_address
    # instruction_length: current instruction length
    arb -4

    add 0, 0, [rb + result]
    add 0, 0, [rb + has_symbol]

    eq  [token], '+', [rb + tmp]
    jnz [rb + tmp], parse_value_number_or_char_1
    eq  [token], '-', [rb + tmp]
    jnz [rb + tmp], parse_value_number_or_char_1
    eq  [token], 'n', [rb + tmp]
    jnz [rb + tmp], parse_value_number_or_char_1
    eq  [token], 'c', [rb + tmp]
    jnz [rb + tmp], parse_value_number_or_char_1
    eq  [token], 'i', [rb + tmp]
    jnz [rb + tmp], parse_value_identifier
    eq  [token], 'I', [rb + tmp]
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
    add [value], 0, [rb - 1]
    arb -1
    call find_frame_symbol

    jz  [rb - 3], parse_value_is_global

    # it is a frame symbol, read its offset
    add [rb - 3], 48, [ip + 1]
    add [0], 0, [rb + result]

    jz  0, parse_value_after_global

parse_value_is_global:
    # it is a global symbol, add a fixup for this identifier
    add [value], 0, [rb - 1]
    add [current_address], [rb + param_offset], [rb - 2]
    add [token_line_num], 0, [rb - 3]
    add [token_column_num], 0, [rb - 4]
    arb -4
    call add_fixup

parse_value_after_global:
    # free the symbol value
    add [value], 0, [rb - 1]
    arb -1
    call free
    add 0, 0, [value]

    jz  0, parse_value_after_symbol

parse_value_ip:
    # [ip + 123] behaves similarly to [symbol + 123]
    add 1, 0, [rb + has_symbol]
    add [current_address], [rb + instruction_length], [rb + result]

parse_value_after_symbol:
    # optionally followed by + or - and a number or char
    call get_token
    eq  [token], '+', [rb + tmp]
    jnz [rb + tmp], parse_value_identifier_plus
    eq  [token], '-', [rb + tmp]
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
    eq  [token], '+', [rb + tmp]
    jnz [rb + tmp], parse_value_number_or_char_2
    eq  [token], '-', [rb + tmp]
    jnz [rb + tmp], parse_value_number_or_char_2
    eq  [token], 'n', [rb + tmp]
    jnz [rb + tmp], parse_value_number_or_char_2
    eq  [token], 'c', [rb + tmp]
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
    ret 2
.ENDFRAME

##########
parse_number_or_char:
.FRAME result, sign, tmp
    arb -3

    add 1, 0, [rb + sign]

    # is there a sign?
    eq  [token], '+', [rb + tmp]
    jnz [rb + tmp], parse_number_or_char_plus
    eq  [token], '-', [rb + tmp]
    jnz [rb + tmp], parse_number_or_char_minus
    jz  0, parse_number_or_char_after_sign

parse_number_or_char_minus:
    add -1, 0, [rb + sign]

parse_number_or_char_plus:
    call get_token

parse_number_or_char_after_sign:
    eq  [token], 'n', [rb + tmp]
    jnz [rb + tmp], parse_number_or_char_have_value
    eq  [token], 'c', [rb + tmp]
    jnz [rb + tmp], parse_number_or_char_have_value

    add err_expect_number_char, 0, [rb]
    call report_error

parse_number_or_char_have_value:
    # return the number/char value
    mul [value], [rb + sign], [rb + result]
    call get_token

    arb 3
    ret 0
.ENDFRAME

##########
dump_token:
.FRAME tmp
    arb -1

    # print token type
    out [token]

    # print token value if relevant
    eq  [token], 'n', [rb + tmp]
    jnz [rb + tmp], dump_token_print_n
    eq  [token], 'c', [rb + tmp]
    jnz [rb + tmp], dump_token_print_c
    eq  [token], 'i', [rb + tmp]
    jnz [rb + tmp], dump_token_print_i
    jz  0, dump_token_finish

dump_token_print_n:
    out ' '
    add [value], 0, [rb - 1]
    arb -1
    call print_num
    jz  0, dump_token_finish

dump_token_print_c:
    out ' '
    out [value]
    jz  0, dump_token_finish

dump_token_print_i:
    out ' '
    add [value], 0, [rb - 1]
    arb -1
    call print_str
    jz  0, dump_token_finish

dump_token_finish:
    out 10

    arb 1
    ret 0
.ENDFRAME

##########
get_input:
.FRAME char, tmp
    arb -2

    # get input from the buffer if we have it
    add [input_buffer], 0, [rb + char]
    add 0, 0, [input_buffer]

    jnz [rb + char], get_input_have_char
    in  [rb + char]

get_input_have_char:
    # track line and column number
    eq  [rb + char], 10, [rb + tmp]
    jz  [rb + tmp], get_input_same_line

    # we have a new line
    add [input_line_num], 1, [input_line_num]
    add [input_column_num], 0, [input_prev_column_num]
    add 1, 0, [input_column_num]
    jz  0, get_input_done

get_input_same_line:
    # we are on the same line
    add [input_column_num], 1, [input_column_num]

get_input_done:
    arb 2
    ret 0
.ENDFRAME

##########
unget_input:
.FRAME char; tmp
    arb -1

    # "unget" an unused char so we get it again later
    add [rb + char], 0, [input_buffer]

    # track line and column number
    eq  [rb + char], 10, [rb + tmp]
    jz  [rb + tmp], unget_input_same_line

    # we moved back to previous line, use [input_prev_column_num] as column num
    add [input_line_num], -1, [input_line_num]
    add [input_prev_column_num], 0, [input_column_num]
    jz  0, unget_input_done

unget_input_same_line:
    add [input_column_num], -1, [input_column_num]

unget_input_done:
    arb 1
    ret 1
.ENDFRAME

##########
get_token:
.FRAME tmp, char
    arb -2

get_token_loop:
    # remember position at the start of the token
    add [input_line_num], 0, [token_line_num]
    add [input_column_num], 0, [token_column_num]

    # get next input character
    call get_input
    add [rb - 2], 0, [rb + char]

    # skip whitespace
    eq  [rb + char], ' ', [rb + tmp]
    jnz [rb + tmp], get_token_loop
    eq  [rb + char], 7, [rb + tmp]
    jnz [rb + tmp], get_token_loop

    # comments
    eq  [rb + char], 35, [rb + tmp]
    jnz [rb + tmp], get_token_eat_comment

    # end of line
    eq  [rb + char], 10, [rb + tmp]
    jnz [rb + tmp], get_token_eol
    eq  [rb + char], 13, [rb + tmp]
    jnz [rb + tmp], get_token_eol

    # identifiers and keywords
    eq  [rb + char], '_', [rb + tmp]
    jnz [rb + tmp], get_token_identifier

    add [rb + char], 0, [rb - 1]
    arb -1
    call is_alpha
    jnz [rb - 3], get_token_identifier

    # directives
    eq  [rb + char], '.', [rb + tmp]
    jnz [rb + tmp], get_token_directive

    # symbols
    add [rb + char], 0, [rb - 1]
    arb -1
    call is_symbol
    jnz [rb - 3], get_token_symbol

    # string literals
    eq  [rb + char], '"', [rb + tmp]
    jnz [rb + tmp], get_token_string

    # character literals
    eq  [rb + char], ''', [rb + tmp]
    jnz [rb + tmp], get_token_char

    # number literals
    add [rb + char], 0, [rb - 1]
    arb -1
    call is_digit
    jnz [rb - 3], get_token_number

    add err_invalid_token, 0, [rb]
    call report_error

get_token_eat_comment:
    # eat everything until end of line

get_token_eat_comment_loop:
    call get_input
    add [rb - 2], 0, [rb + char]

    # the comment ends with an EOL, so we jump to EOL handling
    eq  [rb + char], 10, [rb + tmp]
    jnz [rb + tmp], get_token_eol
    eq  [rb + char], 13, [rb + tmp]
    jnz [rb + tmp], get_token_eol

    jz  0, get_token_eat_comment_loop

get_token_eol:
    # handle Windows-style 13,10 line ends
    eq  [rb + char], 13, [rb + tmp]
    jz  [rb + tmp], get_token_eol_unix

    call get_input
    eq  [rb - 2], 10, [rb + tmp]
    jnz [rb + tmp], get_token_eol_unix

    add err_expect_10_after_13, 0, [rb]
    call report_error

get_token_eol_unix:
    add '$', 0, [token]
    jz  0, get_token_done

get_token_identifier:
    # unget last char, read_identifier will get it again
    add [rb + char], 0, [rb - 1]
    arb -1
    call unget_input

    # return read identifier pointer in [value]
    # this memory needs to be freed by caller of get_token
    call read_identifier_or_keyword
    add [rb - 2], 0, [token]
    add [rb - 3], 0, [value]

    jz  0, get_token_done

get_token_directive:
    call read_directive
    add [rb - 2], 0, [token]
    jz  0, get_token_done

get_token_symbol:
    add [rb + char], 0, [token]
    jz  0, get_token_done

get_token_string:
    # return read string pointer in [value]
    # this memory needs to be freed by caller of get_token
    call read_string
    add [rb - 2], 0, [value]

    add 's', 0, [token]
    jz  0, get_token_done

get_token_char:
    # get one character and return it as token value
    call get_input
    add [rb - 2], 0, [value]

    # get closing quote
    call get_input
    eq  [rb - 2], ''', [rb + tmp]
    jnz [rb + tmp], get_token_char_done

    add err_expect_double_quote, 0, [rb]
    call report_error

get_token_char_done:
    add 'c', 0, [token]
    jz  0, get_token_done

get_token_number:
    # unget last char, read_number will get it again
    add [rb + char], 0, [rb - 1]
    arb -1
    call unget_input

    # return read number in [value]
    call read_number
    add [rb - 2], 0, [value]

    add 'n', 0, [token]
    jz  0, get_token_done

get_token_done:
    arb 2
    ret 0
.ENDFRAME

##########
is_symbol:
.FRAME char; tmp
    arb -1

    eq  [rb + char], '+', [rb + tmp]
    jnz [rb + tmp], is_symbol_end
    eq  [rb + char], '-', [rb + tmp]
    jnz [rb + tmp], is_symbol_end
    eq  [rb + char], '=', [rb + tmp]
    jnz [rb + tmp], is_symbol_end
    eq  [rb + char], ',', [rb + tmp]
    jnz [rb + tmp], is_symbol_end
    eq  [rb + char], ':', [rb + tmp]
    jnz [rb + tmp], is_symbol_end
    eq  [rb + char], ';', [rb + tmp]
    jnz [rb + tmp], is_symbol_end
    eq  [rb + char], '[', [rb + tmp]
    jnz [rb + tmp], is_symbol_end
    eq  [rb + char], ']', [rb + tmp]

is_symbol_end:
    arb 1
    ret 1
.ENDFRAME

##########
is_digit:
.FRAME char; tmp
    arb -1

    # check if 0 <= char <= 9
    lt  '9', [rb + char], [rb + tmp]                        # if char > 9, not a digit
    jnz [rb + tmp], is_digit_end
    lt  [rb + char], '0', [rb + tmp]                        # if char < 0, not a digit

is_digit_end:
    # the result is a logical negation of [rb + tmp]
    eq  [rb + tmp], 0, [rb + tmp]

    arb 1
    ret 1
.ENDFRAME

##########
is_alpha:
.FRAME char; tmp
    arb -1

    # check if a <= char <= z
    lt  'z', [rb + char], [rb + tmp]                        # if char > z, not a letter
    jnz [rb + tmp], is_alpha_end
    lt  [rb + char], 'a', [rb + tmp]                        # if !(char < a), is a letter
    jz  [rb + tmp], is_alpha_end

    # check if A <= char <= Z
    lt  'Z', [rb + char], [rb + tmp]                        # if char > Z, not a letter
    jnz [rb + tmp], is_alpha_end
    lt  [rb + char], 'A', [rb + tmp]                        # if !(char < A), is a letter

is_alpha_end:
    # the result is a logical negation of [rb + tmp]
    eq  [rb + tmp], 0, [rb + tmp]

    arb 1
    ret 1
.ENDFRAME

##########
is_alphanum:
.FRAME char; tmp
    arb -1

    add [rb + char], 0, [rb - 1]
    arb -1
    call is_alpha
    add [rb - 3], 0, [rb + tmp]
    jnz [rb + tmp], is_alphanum_end

    add [rb + char], 0, [rb - 1]
    arb -1
    call is_digit
    add [rb - 3], 0, [rb + tmp]
    jnz [rb + tmp], is_alphanum_end

    eq  [rb + char], '_', [rb + tmp]

is_alphanum_end:
    arb 1
    ret 1
.ENDFRAME

##########
read_string:
.FRAME buffer, index, char, tmp
    arb -4

    # we will store the string in dynamic memory that needs to be freed by caller
    add 50, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + buffer]

    add 0, 0, [rb + index]

    # the opening quote was already processed by caller

read_string_loop:
    call get_input
    add [rb - 2], 0, [rb + char]

    # when we find a quote character, we are done
    # TODO escaping
    eq  [rb + char], '"', [rb + tmp]
    jnz [rb + tmp], read_string_done

    # store the character in buffer
    add [rb + buffer], [rb + index], [ip + 3]
    add [rb + char], 0, [0]

    # increase index and check for maximum string length (50 - 1)
    add [rb + index], 1, [rb + index]
    lt  [rb + index], 49, [rb + tmp]
    jnz [rb + tmp], read_string_loop

    add err_max_string_length, 0, [rb]
    call report_error

read_string_done:
    # zero terminate
    add [rb + buffer], [rb + index], [ip + 3]
    add 0, 0, [0]

    arb 4
    ret 0
.ENDFRAME

##########
read_number:
.FRAME byte, digit, tmp
    arb -3

    add 0, 0, [rb + byte]

read_number_loop:
    # get next character
    call get_input
    add [rb - 2], 0, [rb + digit]

    # if it is not a digit, end
    add [rb + digit], 0, [rb - 1]
    arb -1
    call is_digit
    jz  [rb - 3], read_number_end

    # convert ASCII to a number
    add [rb + digit], -'0', [rb + digit]

    # byte = byte * 10 + digit
    mul [rb + byte], 10, [rb + byte]
    add [rb + byte], [rb + digit], [rb + byte]

    jz  0, read_number_loop

read_number_end:
    # unget last char
    add [rb + digit], 0, [rb - 1]
    arb -1
    call unget_input

    arb 3
    ret 0
.ENDFRAME

##########
read_identifier_or_keyword:
.FRAME token, buffer, tmp
    arb -3

    # read the identifier into a buffer
    call read_identifier
    add [rb - 2], 0, [rb + buffer]

    # check if the identifier is actually a keyword
    # reuse buffer [rb - 2] and length [rb - 3] as parameters
    arb -3
    call detect_keyword
    arb 1
    add [rb - 5], 0, [rb + token]

    # if it is a keyword, free the buffer, we don't need it
    eq  [rb + token], 'i', [rb + tmp]
    jnz [rb + tmp], read_identifier_or_keyword_skip_free

    add [rb + buffer], 0, [rb - 1]
    arb -1
    call free
    add 0, 0, [rb + buffer]

read_identifier_or_keyword_skip_free:
    arb 3
    ret 0
.ENDFRAME
##########

##########
read_identifier:
.FRAME buffer, index, char, tmp
    arb -4

    # we will store the identifier in dynamic memory that needs to be freed by caller
    add 50, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + buffer]

    add 0, 0, [rb + index]

read_identifier_loop:
    call get_input
    add [rb - 2], 0, [rb + char]

    # when we find first non-alphanumeric character, we are done
    add [rb + char], 0, [rb - 1]
    arb -1
    call is_alphanum
    jz  [rb - 3], read_identifier_done

    # store the character in buffer
    add [rb + buffer], [rb + index], [ip + 3]
    add [rb + char], 0, [0]

    # increase index and check for maximum identifier length (50 - 3)
    add [rb + index], 1, [rb + index]
    lt  [rb + index], 47, [rb + tmp]
    jnz [rb + tmp], read_identifier_loop

    add err_max_identifier_length, 0, [rb]
    call report_error

read_identifier_done:
    # zero terminate
    add [rb + buffer], [rb + index], [ip + 3]
    add 0, 0, [0]

    # unget last char
    add [rb + char], 0, [rb - 1]
    arb -1
    call unget_input

    arb 4
    ret 0
.ENDFRAME

##########
detect_keyword:
.FRAME string, length; tmp, char0, char1
    arb -3

    # this uses a perfect hash function, generated using gperf and a list of keywords
    # gperf < src/gperf.in

    # check string length against MAX_WORD_LENGTH and MIN_WORD_LENGTH
    lt  4, [rb + length], [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not
    lt  [rb + length], 2, [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not

    # read first character, check that it is a lowercase letter
    add [rb + string], 0, [ip + 1]
    add [0], 0, [rb + char0]

    lt  [rb + char0], 'a', [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not
    lt  'z', [rb + char0], [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not

    # read second character, check that it is a lowercase letter
    add [rb + string], 1, [ip + 1]
    add [0], 0, [rb + char1]

    lt  [rb + char1], 'a', [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not
    lt  'z', [rb + char1], [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not

    # calculate indexes into the asso_values table
    add [rb + char0], -97, [rb + char0]
    add [rb + char1], -97, [rb + char1]

    # look up the hash value (store to char0)
    add detect_keyword_asso_values, [rb + char0], [ip + 1]
    add [0], [rb + length], [rb + char0]

    add detect_keyword_asso_values, [rb + char1], [ip + 1]
    add [0], [rb + char0], [rb + char0]

    # check hash limit MAX_HASH_VALUE
    lt  37, [rb + char0], [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not

    # find candidate keyword, compare input string with the candidate
    mul [rb + char0], 5, [rb + tmp]
    add detect_keyword_wordlist, [rb + tmp], [rb - 1]
    add [rb + string], 0, [rb - 2]
    arb -2
    call strcmp

    jnz [rb - 4], detect_keyword_is_not

    # find token id, return it
    add detect_keyword_tokens, [rb + char0], [ip + 1]
    add [0], 0, [rb + tmp]

    arb 3
    ret 2

detect_keyword_is_not:
    # not a keyword, so it is an identifier
    add 'i', 0, [rb + tmp]

    arb 3
    ret 2

detect_keyword_asso_values:
    # copied from gperf.c
    db                               0,  0,  0
    db   5, 10, 38, 38, 10, 20, 10, 38, 20, 15
    db   5, 10, 10,  5,  0,  5, 15, 10, 38, 38
    db  38, 38, 10

detect_keyword_wordlist:
    # copied from gperf.c
    ds  10, 0
    db  "rb", 0, 0, 0
    db  "arb", 0, 0
    db  "call", 0
    ds  10, 0
    db  "db", 0, 0, 0
    db  "add", 0, 0
    ds  15, 0
    db  "ds", 0, 0, 0
    db  "ret", 0, 0
    ds  15, 0
    db  "eq", 0, 0, 0
    db  "jnz", 0, 0
    ds  15, 0
    db  "jz", 0, 0, 0
    db  "out", 0, 0
    ds  15, 0
    db  "in", 0, 0, 0
    db  "mul", 0, 0
    ds  15, 0
    db  "ip", 0, 0, 0
    db  "hlt", 0, 0
    ds  15, 0
    db  "lt", 0, 0, 0

detect_keyword_tokens:
    ds  2, 0
    db  'P'
    db  9
    db  'C'
    ds  2, 0
    db  'B'
    db  1
    ds  3, 0
    db  'S'
    db  'R'
    ds  3, 0
    db  8
    db  5
    ds  3, 0
    db  6
    db  4
    ds  3, 0
    db  3
    db  2
    ds  3, 0
    db  'I'
    db  99
    ds  3, 0
    db  7
.ENDFRAME

##########
read_directive:
.FRAME tmp
    arb -1

    # TODO consider reading the whole string and doing a gperf
    # TODO perhaps use the same gperf code as above, just parametrized

    call get_input

    eq  [rb - 2], 'E', [rb + tmp]
    jnz [rb + tmp], read_directive_e
    eq  [rb - 2], 'F', [rb + tmp]
    jnz [rb + tmp], read_directive_frame

    jz  0, read_directive_fail

read_directive_e:
    call get_input

    eq  [rb - 2], 'N', [rb + tmp]
    jnz [rb + tmp], read_directive_endframe
    eq  [rb - 2], 'O', [rb + tmp]
    jnz [rb + tmp], read_directive_eof

    jz  0, read_directive_fail

read_directive_endframe:
    call get_input
    eq  [rb - 2], 'D', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    call get_input
    eq  [rb - 2], 'F', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    call get_input
    eq  [rb - 2], 'R', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    call get_input
    eq  [rb - 2], 'A', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    call get_input
    eq  [rb - 2], 'M', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    call get_input
    eq  [rb - 2], 'E', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    add 'D', 0, [rb + tmp]
    arb 1
    ret 0

read_directive_eof:
    call get_input
    eq  [rb - 2], 'F', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    add 'N', 0, [rb + tmp]
    arb 1
    ret 0

read_directive_frame:
    call get_input
    eq  [rb - 2], 'R', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    call get_input
    eq  [rb - 2], 'A', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    call get_input
    eq  [rb - 2], 'M', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    call get_input
    eq  [rb - 2], 'E', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    add 'F', 0, [rb + tmp]
    arb 1
    ret 0

read_directive_fail:
    add err_invalid_directive, 0, [rb]
    call report_error
.ENDFRAME

##########
# convert number to string
print_num:
.FRAME num; tmp, order, digit; digits
    arb -3

    # determine highest power of 10
    add 1, 0, [rb + order]

    # handle sign if negative
    lt  [rb + num], 0, [rb + tmp]
    jz  [rb + tmp], print_num_next_order
    out '-'
    mul [rb + num], -1, [rb + num]

print_num_next_order:
+3 = print_num_digit_ptr_1:
    add [rb + order], 0, [rb + digits]
    add [print_num_digit_ptr_1], -1, [print_num_digit_ptr_1]

    mul [rb + order], 10, [rb + order]
    lt  [rb + num], [rb + order], [rb + tmp]
    jz  [rb + tmp], print_num_next_order

print_num_finish_order:
    add [print_num_digit_ptr_1], 1, [print_num_digit_ptr_2]

print_num_next_digit:
+1 = print_num_digit_ptr_2:
    add [rb + digits], 0, [rb + order]
    add -1, 0, [rb + digit]

print_num_increase:
    add [rb + digit], 1, [rb + digit]
    mul [rb + order], -1, [rb + tmp]
    add [rb + num], [rb + tmp], [rb + num]
    lt  [rb + num], 0, [rb + tmp]
    jz  [rb + tmp], print_num_increase

    add [rb + num], [rb + order], [rb + num]
    add [rb + digit], '0', [rb + digit]
    out [rb + digit]

    eq  [rb + order], 1, [rb + tmp]
    jnz [rb + tmp], print_num_finish

    add [print_num_digit_ptr_2], 1, [print_num_digit_ptr_2]
    jz  0, print_num_next_digit

print_num_finish:
    add digits, 0, [print_num_digit_ptr_2]
    add digits, 0, [print_num_digit_ptr_1]

    arb 3
    ret 1
.ENDFRAME

##########
alloc:
.FRAME size; block, tmp
    arb -2

    # we only support certain block sizes
    lt  50, [rb + size], [rb + tmp]
    jz  [rb + tmp], alloc_size_ok

    add err_allocation_size, 0, [rb]
    call report_error

alloc_size_ok:
    # do we have any free blocks?
    jz  [free_head], alloc_create_block

    # yes, remove first block from the list and return it
    add [free_head], 0, [rb + block]

    add [free_head], 0, [ip + 1]
    add [0], 0, [free_head]

    jz  0, alloc_done

alloc_create_block:
    # there are no free blocks, create one
    add [heap_end], 0, [rb + block]
    add [heap_end], 50, [heap_end]

alloc_done:
    arb 2
    ret 1
.ENDFRAME

##########
free:
.FRAME block; tmp
    arb -1

    # set pointer to next free block in the block we are returning
    add [rb + block], 0, [ip + 3]
    add [free_head], 0, [0]

    # set new free block head
    add [rb + block], 0, [free_head]

    arb 1
    ret 1
.ENDFRAME

##########
find_global_symbol:
.FRAME identifier; record
    arb -1

    add [global_head], 0, [rb + record]

find_global_symbol_loop:
    # are there any more records?
    jz  [rb + record], find_global_symbol_done

    # does this record contain the identifier?
    add [rb + identifier], 0, [rb - 1]
    add [rb + record], 1, [rb - 2]
    arb -2
    call strcmp

    # if strcmp result is 0, we are done
    jz  [rb - 4], find_global_symbol_done

    # move to next record
    add [rb + record], 0, [ip + 1]
    add [0], 0, [rb + record]

    jz  0, find_global_symbol_loop

find_global_symbol_done:
    arb 1
    ret 1
.ENDFRAME

##########
add_global_symbol:
.FRAME identifier; record
    arb -1

    # allocate a block
    add 50, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + record]

    # set pointer to next symbol
    add [rb + record], 0, [ip + 3]
    add [global_head], 0, [0]

    # store the identifier
    add [rb + identifier], 0, [rb - 1]
    add [rb + record], 1, [rb - 2]
    arb -2
    call strcpy

    # set address to -1, so we can detect when the address is set
    add [rb + record], 48, [ip + 3]
    add -1, 0, [0]

    # set fixup head to 0
    add [rb + record], 49, [ip + 3]
    add 0, 0, [0]

    # set new symbol head
    add [rb + record], 0, [global_head]

    arb 1
    ret 1
.ENDFRAME

##########
add_fixup:
.FRAME identifier, address, line_num, column_num; symbol, fixup, tmp
    arb -3

    # find or create the symbol record
    add [rb + identifier], 0, [rb - 1]
    arb -1
    call find_global_symbol
    add [rb - 3], 0, [rb + symbol]

    jnz [rb + symbol], add_fixup_have_symbol

    add [rb + identifier], 0, [rb - 1]
    arb -1
    call add_global_symbol
    add [rb - 3], 0, [rb + symbol]

add_fixup_have_symbol:
    # allocate a block
    # TODO use smaller blocks, or collect multiple fixups in one block
    add 50, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + fixup]

    # store the address of the fixup
    add [rb + fixup], 1, [ip + 3]
    add [rb + address], 0, [0]

    # store line number
    add [rb + fixup], 2, [ip + 3]
    add [rb + line_num], 0, [0]

    # store column number
    add [rb + fixup], 3, [ip + 3]
    add [rb + column_num], 0, [0]

    # read current fixup list head
    add [rb + symbol], 49, [ip + 1]
    add [0], 0, [rb + tmp]

    # set pointer to next fixup
    add [rb + fixup], 0, [ip + 3]
    add [rb + tmp], 0, [0]

    # set new fixup list head
    add [rb + symbol], 49, [ip + 3]
    add [rb + fixup], 0, [0]

    arb 3
    ret 4
.ENDFRAME

##########
set_global_symbol_address:
.FRAME identifier, address; symbol, tmp
    arb -2

    # find or create the symbol record
    add [rb + identifier], 0, [rb - 1]
    arb -1
    call find_global_symbol
    add [rb - 3], 0, [rb + symbol]

    jnz [rb + symbol], set_global_symbol_address_check_duplicate

    add [rb + identifier], 0, [rb - 1]
    arb -1
    call add_global_symbol
    add [rb - 3], 0, [rb + symbol]

    jz  0, set_global_symbol_address_have_symbol

set_global_symbol_address_check_duplicate:
    # check for duplicate symbol definitions
    add [rb + symbol], 48, [ip + 1]
    add [0], 0, [rb + tmp]

    eq  [rb + tmp], -1, [rb + tmp]
    jnz [rb + tmp], set_global_symbol_address_have_symbol

    add err_duplicate_symbol, 0, [rb]
    call report_error

set_global_symbol_address_have_symbol:
    # store the address of the symbol
    add [rb + symbol], 48, [ip + 3]
    add [rb + address], 0, [0]

    arb 2
    ret 2
.ENDFRAME

##########
find_frame_symbol:
.FRAME identifier; record
    arb -1

    add [frame_head], 0, [rb + record]

find_frame_symbol_loop:
    # are there any more records?
    jz  [rb + record], find_frame_symbol_done

    # does this record contain the identifier?
    add [rb + identifier], 0, [rb - 1]
    add [rb + record], 1, [rb - 2]
    arb -2
    call strcmp

    # if strcmp result is 0, we are done
    jz  [rb - 4], find_frame_symbol_done

    # move to next record
    add [rb + record], 0, [ip + 1]
    add [0], 0, [rb + record]

    jz  0, find_frame_symbol_loop

find_frame_symbol_done:
    arb 1
    ret 1
.ENDFRAME

##########
add_frame_symbol:
.FRAME identifier, block_index; record
    arb -1

    # allocate a block
    add 50, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + record]

    # set pointer to next symbol
    add [rb + record], 0, [ip + 3]
    add [frame_head], 0, [0]

    # store the identifier
    add [rb + identifier], 0, [rb - 1]
    add [rb + record], 1, [rb - 2]
    arb -2
    call strcpy

    # set block index
    add [rb + record], 49, [ip + 3]
    add [rb + block_index], 0, [0]

    # set new symbol head
    add [rb + record], 0, [frame_head]

    arb 1
    ret 2
.ENDFRAME

##########
reset_frame:
.FRAME tmp, record
    arb -2

    add [frame_head], 0, [rb + record]

reset_frame_symbol_loop:
    # are there any more records?
    jz  [rb + record], reset_frame_symbol_done

    # save current record pointer
    add [rb + record], 0, [rb + tmp]

    # move to next record
    add [rb + record], 0, [ip + 1]
    add [0], 0, [rb + record]

    # free current record
    add [rb + tmp], 0, [rb - 1]
    arb -1
    call free

    jz  0, reset_frame_symbol_loop

reset_frame_symbol_done:
    # reset list head
    add 0, 0, [frame_head]

    arb 2
    ret 0
.ENDFRAME

##########
set_mem:
.FRAME byte; buffer, tmp
    arb -2

    add [mem_tail], 0, [rb + buffer]

    # do we have a buffer at all?
    jnz [rb + buffer], set_mem_have_buffer

    # no, create one
    add 50, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + buffer]

    # reset next buffer pointer
    add [rb + buffer], 0, [ip + 3]
    add 0, 0, [0]

    add [rb + buffer], 0, [mem_head]
    add [rb + buffer], 0, [mem_tail]
    add 1, 0, [mem_index]

    jz  0, set_mem_have_space

set_mem_have_buffer:
    # is there enough space for one more byte?
    lt  [mem_index], 50, [rb + tmp]
    jnz [rb + tmp], set_mem_have_space

    # no, create a new buffer
    add 50, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + buffer]

    # reset next buffer pointer
    add [rb + buffer], 0, [ip + 3]
    add 0, 0, [0]

    # add it to the tail of buffer linked list
    add [mem_tail], 0, [ip + 3]
    add [rb + buffer], 0, [0]

    add [rb + buffer], 0, [mem_tail]
    add 1, 0, [mem_index]

set_mem_have_space:
    add [mem_tail], [mem_index], [ip + 3]
    add [rb + byte], 0, [0]

    add [mem_index], 1, [mem_index]

    arb 2
    ret 1
.ENDFRAME

##########
set_mem_str:
.FRAME string; index, char
    arb -2

    add 0, 0, [rb + index]

set_mem_str_loop:
    add [rb + string], [rb + index], [ip + 1]
    add [0], 0, [rb + char]

    jz  [rb + char], set_mem_str_done

    add [rb + char], 0, [rb - 1]
    arb -1
    call set_mem

    add [rb + index], 1, [rb + index]
    jz  0, set_mem_str_loop

set_mem_str_done:
    arb 2
    ret 1
.ENDFRAME

##########
inc_mem_at:
.FRAME byte, index, offset; buffer
    arb -1

    # increase memory location in index-th memory block, location offset
    # assume this does not require creating new blocks

    add [mem_head], 0, [rb + buffer]

inc_mem_at_loop:
    # any more blocks?
    jnz [rb + buffer], inc_mem_at_have_block

    add err_invalid_fixup, 0, [rb]
    call report_error

inc_mem_at_have_block:
    # is this the block we need?
    jz  [rb + index], inc_mem_at_this_block
    add [rb + index], -1, [rb + index]

    # next block in linked list
    add [rb + buffer], 0, [ip + 1]
    add [0], 0, [rb + buffer]

    jz  0, inc_mem_at_loop

inc_mem_at_this_block:
    # set the value
    add [rb + buffer], [rb + offset], [inc_mem_at_ptr_in]
    add [rb + buffer], [rb + offset], [inc_mem_at_ptr_out]
+1 = inc_mem_at_ptr_in:
+3 = inc_mem_at_ptr_out:
    add [0], [rb + byte], [0]

    arb 1
    ret 3
.ENDFRAME

##########
print_mem:
.FRAME tmp, buffer, limit, index, first
    arb -5

    add 1, 0, [rb + first]

    add [mem_head], 0, [rb + buffer]
    jz  [mem_head], print_mem_done

print_mem_block:
    add 1, 0, [rb + index]

    # maximum index within a block is 50, except for last block
    add 50, 0, [rb + limit]
    eq  [rb + buffer], [mem_tail], [rb + tmp]
    jz  [rb + tmp], print_mem_byte
    add [mem_index], 0, [rb + limit]

print_mem_byte:
    lt  [rb + index], [rb + limit], [rb + tmp]
    jz  [rb + tmp], print_mem_block_done

    # skip comma when printing first byte
    eq  [rb + first], 1, [rb + tmp]
    jnz [rb + tmp], print_mem_skip_comma
    out ','

print_mem_skip_comma:
    add 0, 0, [rb + first]

    add [rb + buffer], [rb + index], [ip + 1]
    add [0], 0, [rb + tmp]

    add [rb + tmp], 0, [rb - 1]
    arb -1
    call print_num

    add [rb + index], 1, [rb + index]
    jz  0, print_mem_byte

print_mem_block_done:
    # next block in linked list
    add [rb + buffer], 0, [ip + 1]
    add [0], 0, [rb + buffer]

    jnz [rb + buffer], print_mem_block

print_mem_done:
    out 10

    arb 5
    ret 0
.ENDFRAME

##########
do_fixups:
.FRAME tmp, symbol, fixup, symbol_address, fixup_address, index, offset
    arb -7

    add [global_head], 0, [rb + symbol]

do_fixups_symbol:
    # do we have more symbols?
    jz  [rb + symbol], do_fixups_done

    # each symbol needs to have an address
    add [rb + symbol], 48, [ip + 1]
    add [0], 0, [rb + symbol_address]

    eq  [rb + symbol_address], -1, [rb + tmp]
    jz  [rb + tmp], do_fixups_have_address

    add [rb + symbol], 0, [rb + 1]
    add err_unknown_symbol, 0, [rb]
    call report_symbol_error

do_fixups_have_address:
    # iterate through all fixups for this symbol
    add [rb + symbol], 49, [ip + 1]
    add [0], 0, [rb + fixup]

do_fixups_fixup:
    # do we have more fixups for this symbol?
    jz  [rb + fixup], do_fixups_symbol_done

    # read fixup address
    add [rb + fixup], 1, [ip + 1]
    add [0], 0, [rb + fixup_address]

    # find out which memory block should be updated
    add [rb + fixup_address], 0, [rb - 1]
    arb -1
    call calc_fixup

    # do the fixup
    add [rb + symbol_address], 0, [rb - 1]
    add [rb - 3], 0, [rb - 2]
    add [rb - 4], 0, [rb - 3]
    arb -3
    call inc_mem_at

    # move to next fixup
    add [rb + fixup], 0, [ip + 1]
    add [0], 0, [rb + fixup]

    jz  0, do_fixups_fixup

do_fixups_symbol_done:
    # move to next symbol
    add [rb + symbol], 0, [ip + 1]
    add [0], 0, [rb + symbol]

    jz  0, do_fixups_symbol

do_fixups_done:
    arb 7
    ret 0
.ENDFRAME

##########
report_symbol_error:
.FRAME symbol, message; fixup, line_num, column_num
    arb -3

    add 0, 0, [rb + line_num]
    add 0, 0, [rb + column_num]

    # get first fixup for this symbol
    add [rb + symbol], 49, [ip + 1]
    add [0], 0, [rb + fixup]
    jz  [rb + fixup], report_symbol_error_after_location

    # read fixup line num
    add [rb + fixup], 2, [ip + 1]
    add [0], 0, [rb + line_num]

    # read fixup column num
    add [rb + fixup], 3, [ip + 1]
    add [0], 0, [rb + column_num]

report_symbol_error_after_location:
    # we don't bother with updating the stack pointer, this function never returns
    add [rb + message], 0, [rb + 2]
    add [rb + line_num], 0, [rb + 1]
    add [rb + column_num], 0, [rb]
    call report_error_at_location
.ENDFRAME

##########
calc_fixup:
.FRAME address; index, offset, tmp
    arb -3

    # calculate index = address / (50 - 1) ; offset = address % (50 - 1) + 1
    add 0, 0, [rb + index]
    add [rb + address], 0, [rb + offset]

calc_fixup_loop:
    lt  [rb + offset], 49, [rb + tmp]
    jnz [rb + tmp], calc_fixup_done

    add [rb + offset], -49, [rb + offset]
    add [rb + index], 1, [rb + index]

    jz  0, calc_fixup_loop

calc_fixup_done:
    # data in memory blocks starts at offset 1
    add [rb + offset], 1, [rb + offset]

    arb 3
    ret 1
.ENDFRAME

##########
report_error:
.FRAME message;
    # we don't bother with updating the stack pointer, this function never returns
    add [rb + message], 0, [rb + 2]
    add [token_line_num], 0, [rb + 1]
    add [token_column_num], 0, [rb]
    call report_error_at_location
.ENDFRAME

##########
report_error_at_location:
.FRAME message, line_num, column_num;
    #call print_mem

    add report_error_msg_start, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + message], 0, [rb - 1]
    arb -1
    call print_str

    add report_error_msg_line, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + line_num], 0, [rb - 1]
    arb -1
    call print_num

    add report_error_msg_column, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + column_num], 0, [rb - 1]
    arb -1
    call print_num

    add report_error_msg_end, 0, [rb - 1]
    arb -1
    call print_str

    out 10

    # instead of halting, we will read all inputs, which hopefully
    # will cause the intcode vm to crash, indicating a problem
    # this is the only way we can return a non-zero result from intcode

report_error_loop:
    in  [0]
    jz  0, report_error_loop

report_error_msg_start:
    db "Error: ", 0
report_error_msg_line:
    db " (line ", 0
report_error_msg_column:
    db ", column ", 0
report_error_msg_end:
    db ")", 0
.ENDFRAME

##########
strcmp:
.FRAME str1, str2; tmp, char1, char2, index
    arb -4

    add 0, 0, [rb + index]

strcmp_loop:
    add [rb + str1], [rb + index], [ip + 1]
    add [0], 0, [rb + char1]

    add [rb + str2], [rb + index], [ip + 1]
    add [0], 0, [rb + char2]

    # different characters, we are done
    eq  [rb + char1], [rb + char2], [rb + tmp]
    jz  [rb + tmp], strcmp_done

    # same character, is it 0?
    jz  [rb + char1], strcmp_done

    add [rb + index], 1, [rb + index]
    jz  0, strcmp_loop

strcmp_done:
    mul [rb + char2], -1, [rb + tmp]
    add [rb + char1], [rb + tmp], [rb + tmp]

    arb 4
    ret 2
.ENDFRAME

##########
strcpy:
.FRAME src, tgt; index, char
    arb -2

    add 0, 0, [rb + index]

strcpy_loop:
    add [rb + src], [rb + index], [ip + 1]
    add [0], 0, [rb + char]

    add [rb + tgt], [rb + index], [ip + 3]
    add [rb + char], 0, [0]

    jz  [rb + char], strcpy_done

    add [rb + index], 1, [rb + index]
    jz  0, strcpy_loop

strcpy_done:
    arb 2
    ret 2
.ENDFRAME

##########
print_str:
.FRAME str; index, tmp, char
    arb -3

    add 0, 0, [rb + index]

print_str_loop:
    add [rb + str], [rb + index], [ip + 1]
    add [0], 0, [rb + char]

    jz  [rb + char], print_str_done

    out [rb + char]

    add [rb + index], 1, [rb + index]
    jz  0, print_str_loop

print_str_done:
    arb 3
    ret 1
.ENDFRAME

##########
# globals

# head of the linked list of free blocks
free_head:
    db  0

# start of unused memory
heap_end:
    db  stack

# line and column number of next input character
input_line_num:
    db  1
input_column_num:
    db  1

# column number before last input character
input_prev_column_num:
    db  0

# last input char buffer
input_buffer:
    db  0

# line and column number of current token
token_line_num:
    db  1
token_column_num:
    db  1

# lookahead token type
token:
    db  0

# lookahead token value, if any
value:
    db  0

# global symbol record layout:
# 0: pointer to next symbol
# 1-47: zero-terminated symbol identifier
# 48: symbol value (address)
# 49: linked list of fixups

# fixup record layout:
# 0: pointer to next fixup
# 1: fixup address
# 2: fixup line number
# 3: fixup column number

# head of the linked list of global symbols
global_head:
    db  0

# frame symbol record layout:
# 0: pointer to next symbol
# 1-47: zero-terminated symbol identifier
# 48: symbol value (offset)
# 49: block index (.FRAME block0; block1; block2)

# head of the linked list of frame symbols
frame_head:
    db  0

# are we currently in a frame?
is_frame:
    db  0

# current instruction pointer
current_address:
    db 0

# output memory buffer
mem_head:
    db 0

mem_tail:
    db 0

# index of next unused byte in last memory buffer
mem_index:
    db 0

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
err_expect_semicolon_eol:
    db  "Expecting a semicolon or a line end", 0
err_expect_comma_semicolon_eol:
    db  "Expecting a comma, a semicolon or a line end", 0
err_expect_number:
    db  "Expecting a number", 0
err_expect_equals:
    db  "Expecting an equals sign", 0
err_expect_identifier:
    db  "Expecting an identifier", 0
err_expect_colon:
    db  "Expecting a colon", 0
err_already_in_frame:
    db  "A frame is already active", 0
err_too_many_lists:
    db  "Expecting up to three identifier lists", 0
err_not_in_frame:
    db  "There is no frame active", 0
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
err_invalid_token:
    db  "Invalid token", 0
err_expect_double_quote:
    db  "Expecting a double quote", 0
err_max_string_length:
    db  "Maximum string length exceeded", 0
err_max_identifier_length:
    db  "Maximum identifier length exceeded", 0
err_invalid_directive:
    db  "Invalid directive", 0
err_allocation_size:
    db  "Unsupported allocation size", 0
err_duplicate_symbol:
    db  "Duplicate symbol definition", 0
err_invalid_fixup:
    db  "Invalid fixup location", 0
err_unknown_symbol:
    db  "Unknown symbol", 0
err_expect_10_after_13:
    db  "Expecting character 10 after character 13", 0
err_expect_endframe:
    db  "Expecting .ENDFRAME", 0

##########
    ds  50, 0
stack:

.EOF
