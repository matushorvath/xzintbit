# TODO
# - return address frame symbol?
# - check the typescript as implementation for missing error handling
# - test: both global and frame symbol; access frame outside of frame
# - don't duplicate constants in many files

.EXPORT initialize
.EXPORT parse
.EXPORT do_fixups

.EXPORT global_head
.EXPORT mem_head
.EXPORT mem_tail
.EXPORT mem_index

# from libxib/heap.s
.IMPORT alloc
.IMPORT free

# from libxib/string.s
.IMPORT strcmp
.IMPORT strcpy

# from util.s
.IMPORT report_error
.IMPORT report_symbol_error

# from lexer.s
.IMPORT get_token
.IMPORT token_type
.IMPORT token_value
.IMPORT token_line_num
.IMPORT token_column_num

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

    eq  [token_type], ',', [rb + tmp]
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

    eq  [token_type], '$', [rb + tmp]
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

    eq  [token_type], '$', [rb + tmp]
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
    add [token_type], 0, [rb + op]
    call get_token

    eq  [token_type], '$', [rb + tmp]
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
parse_symbol:
.FRAME tmp, address
    arb -2

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

    add err_expect_identifier, 0, [rb]
    call report_error

parse_symbol_have_identifier:
    # add the symbol to symbol table
    add [token_value], 0, [rb - 1]
    add [rb + address], 0, [rb - 2]
    arb -2
    call set_global_symbol_address

    # free the symbol value
    add [token_value], 0, [rb - 1]
    arb -1
    call free
    add 0, 0, [token_value]

    call get_token

    eq  [token_type], ':', [rb + tmp]
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
    eq  [token_type], ';', [rb + tmp]
    jnz [rb + tmp], parse_dir_frame_check_count
    eq  [token_type], '$', [rb + tmp]
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
    # now decide which block represents parameters, local variables and negative variables
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
    eq  [token_type], ';', [rb + tmp]
    jnz [rb + tmp], parse_dir_frame_block_done
    eq  [token_type], '$', [rb + tmp]
    jnz [rb + tmp], parse_dir_frame_block_done

parse_dir_frame_block_loop:
    eq  [token_type], 'i', [rb + tmp]
    jnz [rb + tmp], parse_dir_frame_block_identifier

    add err_expect_identifier, 0, [rb]
    call report_error

parse_dir_frame_block_identifier:
    # check for duplicate symbols
    add [token_value], 0, [rb - 1]
    arb -1
    call find_frame_symbol
    jz  [rb - 3], parse_dir_frame_block_is_unique

    add err_duplicate_symbol, 0, [rb]
    call report_error

parse_dir_frame_block_is_unique:
    # store the symbol with block index
    add [token_value], 0, [rb - 1]
    add [rb + block_count], 0, [rb - 2]
    arb -2
    call add_frame_symbol

    # free the identifier
    add [token_value], 0, [rb - 1]
    arb -1
    call free

    add [rb + symbol_count], 1, [rb + symbol_count]
    call get_token

    eq  [token_type], ',', [rb + tmp]
    jnz [rb + tmp], parse_dir_frame_block_comma
    eq  [token_type], ';', [rb + tmp]
    jnz [rb + tmp], parse_dir_frame_block_done
    eq  [token_type], '$', [rb + tmp]
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

    # are there any negative symbols?
    lt  [rb + block_count], 3, [rb + tmp]
    jnz [rb + tmp], parse_dir_frame_offset_blocks_after_negative

    # process block 2 as negative variables
    add 2, 0, [rb - 1]
    mul [rb + symbol_count], -1, [rb - 2]
    arb -2
    call parse_dir_frame_offset

parse_dir_frame_offset_blocks_after_negative:
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
    add [rb + record], FRAME_BLOCK, [ip + 1]
    add [0], 0, [rb + record_block]

    # for records where block matches the block we are processing, set offset
    eq  [rb + record_block], [rb + current_block], [rb + tmp]
    jz  [rb + tmp], parse_dir_frame_offset_after_update

    # write offset field
    add [rb + record], FRAME_OFFSET, [ip + 3]
    add [rb + offset], 0, [0]

    # increment offset for next symbol
    add [rb + offset], 1, [rb + offset]

parse_dir_frame_offset_after_update:
    # move to next record
    add [rb + record], FRAME_NEXT_PTR, [ip + 1]
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

    eq  [token_type], '$', [rb + tmp]
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
parse_dir_symbol:
.FRAME tmp, identifier
    arb -2

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
    add [rb - 2], 0, [rb + tmp]

    # add the symbol to symbol table
    add [rb + identifier], 0, [rb - 1]
    add [rb + tmp], 0, [rb - 2]
    arb -2
    call set_global_symbol_address

    add [rb + identifier], 0, [rb - 1]
    add 3, 0, [rb - 2]
    arb -2
    call set_global_symbol_type

    # free the identifier
    add [rb + identifier], 0, [rb - 1]
    arb -1
    call free
    add 0, 0, [rb + identifier]

    # check end of line
    eq  [token_type], '$', [rb + tmp]
    jnz [rb + tmp], parse_dir_symbol_done

    add err_expect_eol, 0, [rb]
    call report_error

parse_dir_symbol_done:
    arb 2
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
    # set the global symbol as exported
    add [token_value], 0, [rb - 1]
    add [rb + type], 0, [rb - 2]
    arb -2
    call set_global_symbol_type

    # free the identifier
    add [token_value], 0, [rb - 1]
    arb -1
    call free
    add 0, 0, [token_value]

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

    jz  0, parse_value_after_global

parse_value_is_global:
    # it is a global symbol
    jnz [rb + allow_global_symbol], parse_value_global_symbol_allowed

    add err_global_symbol_not_allowed, 0, [rb]
    call report_error

parse_value_global_symbol_allowed:
    # add a fixup for this identifier
    add [token_value], 0, [rb - 1]
    add [current_address], [rb + param_offset], [rb - 2]
    add [token_line_num], 0, [rb - 3]
    add [token_column_num], 0, [rb - 4]
    arb -4
    call add_fixup

parse_value_after_global:
    # free the symbol value
    add [token_value], 0, [rb - 1]
    arb -1
    call free
    add 0, 0, [token_value]

    jz  0, parse_value_after_symbol

parse_value_ip:
    # [ip + 123] behaves similarly to [symbol + 123]
    add 1, 0, [rb + has_symbol]
    add [current_address], [rb + instruction_length], [rb + result]

    # add a fixup (actually a relocation) for the use of current_address
    add relocation_symbol, 0, [rb - 1]
    add [current_address], [rb + param_offset], [rb - 2]
    add [token_line_num], 0, [rb - 3]
    add [token_column_num], 0, [rb - 4]
    arb -4
    call add_fixup

parse_value_after_symbol:
    # optionally followed by + or - and a number or char
    call get_token
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
find_global_symbol:
.FRAME identifier; record
    arb -1

    add [global_head], 0, [rb + record]

find_global_symbol_loop:
    # are there any more records?
    jz  [rb + record], find_global_symbol_done

    # does this record contain the identifier?
    add [rb + identifier], 0, [rb - 1]
    add [rb + record], GLOBAL_IDENTIFIER, [rb - 2]
    arb -2
    call strcmp

    # if strcmp result is 0, we are done
    jz  [rb - 4], find_global_symbol_done

    # move to next record
    add [rb + record], GLOBAL_NEXT_PTR, [ip + 1]
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
    add GLOBAL_SIZE, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + record]

    # set pointer to next symbol
    add [rb + record], GLOBAL_NEXT_PTR, [ip + 3]
    add [global_head], 0, [0]

    # store the identifier
    add [rb + identifier], 0, [rb - 1]
    add [rb + record], GLOBAL_IDENTIFIER, [rb - 2]
    arb -2
    call strcpy

    # set the symbol as local by default
    add [rb + record], GLOBAL_TYPE, [ip + 3]
    add 0, 0, [0]

    # set address to -1, so we can detect when the address is set
    add [rb + record], GLOBAL_ADDRESS, [ip + 3]
    add -1, 0, [0]

    # set fixup head to 0
    add [rb + record], GLOBAL_FIXUPS_HEAD, [ip + 3]
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
    add FIXUP_SIZE, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + fixup]

    # store the address of the fixup
    add [rb + fixup], FIXUP_ADDRESS, [ip + 3]
    add [rb + address], 0, [0]

    # store line number
    add [rb + fixup], FIXUP_LINE_NUM, [ip + 3]
    add [rb + line_num], 0, [0]

    # store column number
    add [rb + fixup], FIXUP_COLUMN_NUM, [ip + 3]
    add [rb + column_num], 0, [0]

    # read current fixup list head
    add [rb + symbol], GLOBAL_FIXUPS_HEAD, [ip + 1]
    add [0], 0, [rb + tmp]

    # set pointer to next fixup
    add [rb + fixup], FIXUP_NEXT_PTR, [ip + 3]
    add [rb + tmp], 0, [0]

    # set new fixup list head
    add [rb + symbol], GLOBAL_FIXUPS_HEAD, [ip + 3]
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
    add [rb + symbol], GLOBAL_ADDRESS, [ip + 1]
    add [0], 0, [rb + tmp]

    eq  [rb + tmp], -1, [rb + tmp]
    jnz [rb + tmp], set_global_symbol_address_have_symbol

    add err_duplicate_symbol, 0, [rb]
    call report_error

set_global_symbol_address_have_symbol:
    # store the address of the symbol
    add [rb + symbol], GLOBAL_ADDRESS, [ip + 3]
    add [rb + address], 0, [0]

    arb 2
    ret 2
.ENDFRAME

##########
set_global_symbol_type:
.FRAME identifier, type; symbol, tmp
    arb -2

    # find or create the symbol record
    add [rb + identifier], 0, [rb - 1]
    arb -1
    call find_global_symbol
    add [rb - 3], 0, [rb + symbol]

    jnz [rb + symbol], set_global_symbol_type_check

    add [rb + identifier], 0, [rb - 1]
    arb -1
    call add_global_symbol
    add [rb - 3], 0, [rb + symbol]

    jz  0, set_global_symbol_type_have_symbol

set_global_symbol_type_check:
    # check for symbol already imported/exported
    add [rb + symbol], GLOBAL_TYPE, [ip + 1]
    add [0], 0, [rb + tmp]

    jz  [rb + tmp], set_global_symbol_type_have_symbol

    eq  [rb + tmp], [rb + type], [rb + tmp]
    jnz [rb + tmp], set_global_symbol_type_check_same

    add err_symbol_symbol_type_mix, 0, [rb]
    call report_error

set_global_symbol_type_check_same:
    eq  [rb + type], 1, [rb + tmp]
    jnz [rb + tmp], set_global_symbol_type_error_imported
    eq  [rb + type], 2, [rb + tmp]
    jnz [rb + tmp], set_global_symbol_type_error_exported

    add err_constant_already_defined, 0, [rb]
    call report_error

set_global_symbol_type_error_imported:
    add err_symbol_already_imported, 0, [rb]
    call report_error

set_global_symbol_type_error_exported:
    add err_symbol_already_exported, 0, [rb]
    call report_error

set_global_symbol_type_have_symbol:
    # set symbol type
    add [rb + symbol], GLOBAL_TYPE, [ip + 3]
    add [rb + type], 0, [0]

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
    add [rb + record], FRAME_IDENTIFIER, [rb - 2]
    arb -2
    call strcmp

    # if strcmp result is 0, we are done
    jz  [rb - 4], find_frame_symbol_done

    # move to next record
    add [rb + record], FRAME_NEXT_PTR, [ip + 1]
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
    add FRAME_SIZE, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + record]

    # set pointer to next symbol
    add [rb + record], FRAME_NEXT_PTR, [ip + 3]
    add [frame_head], 0, [0]

    # store the identifier
    add [rb + identifier], 0, [rb - 1]
    add [rb + record], FRAME_IDENTIFIER, [rb - 2]
    arb -2
    call strcpy

    # set block index
    add [rb + record], FRAME_BLOCK, [ip + 3]
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
    add [rb + record], FRAME_NEXT_PTR, [ip + 1]
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
    add MEM_BLOCK_SIZE, 0, [rb - 1]
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
    lt  [mem_index], MEM_BLOCK_SIZE, [rb + tmp]
    jnz [rb + tmp], set_mem_have_space

    # no, create a new buffer
    add MEM_BLOCK_SIZE, 0, [rb - 1]
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
do_fixups:
.FRAME tmp, symbol, fixup, symbol_address, fixup_address
    arb -5

    add [global_head], 0, [rb + symbol]

do_fixups_symbol:
    # do we have more symbols?
    jz  [rb + symbol], do_fixups_done

    # special handling of imported symbols and relocations, they don't require fixups
    add [rb + symbol], GLOBAL_TYPE, [ip + 1]
    eq  [0], 1, [rb + tmp]
    jnz [rb + tmp], do_fixups_symbol_done
    add [rb + symbol], GLOBAL_TYPE, [ip + 1]
    eq  [0], 4, [rb + tmp]
    jnz [rb + tmp], do_fixups_symbol_done

    # each non-imported symbol needs to have an address
    add [rb + symbol], GLOBAL_ADDRESS, [ip + 1]
    add [0], 0, [rb + symbol_address]

    eq  [rb + symbol_address], -1, [rb + tmp]
    jz  [rb + tmp], do_fixups_have_address

    add [rb + symbol], 0, [rb + 1]
    add err_unknown_symbol, 0, [rb]
    call report_symbol_error

do_fixups_have_address:
    # iterate through all fixups for this symbol
    add [rb + symbol], GLOBAL_FIXUPS_HEAD, [ip + 1]
    add [0], 0, [rb + fixup]

do_fixups_fixup:
    # do we have more fixups for this symbol?
    jz  [rb + fixup], do_fixups_symbol_done

    # read fixup address
    add [rb + fixup], FIXUP_ADDRESS, [ip + 1]
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
    add [rb + fixup], FIXUP_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + fixup]

    jz  0, do_fixups_fixup

do_fixups_symbol_done:
    # move to next symbol
    add [rb + symbol], GLOBAL_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + symbol]

    jz  0, do_fixups_symbol

do_fixups_done:
    arb 5
    ret 0
.ENDFRAME

##########
calc_fixup:
.FRAME address; index, offset, tmp
    arb -3

    # calculate index = address / (MEM_BLOCK_SIZE - 1) ; offset = address % (MEM_BLOCK_SIZE - 1) + 1
    add 0, 0, [rb + index]
    add [rb + address], 0, [rb + offset]

calc_fixup_loop:
    lt  [rb + offset], MEM_BLOCK_SIZE - 1, [rb + tmp]
    jnz [rb + tmp], calc_fixup_done

    mul MEM_BLOCK_SIZE - 1, -1, [rb + tmp]
    add [rb + offset], [rb + tmp], [rb + offset]
    add [rb + index], 1, [rb + index]

    jz  0, calc_fixup_loop

calc_fixup_done:
    # data in memory blocks starts at offset 1
    add [rb + offset], 1, [rb + offset]

    arb 3
    ret 1
.ENDFRAME

##########
# globals

# head of the linked list of global symbols
global_head:
    db  0

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
err_duplicate_symbol:
    db  "Duplicate symbol definition", 0
err_invalid_fixup:
    db  "Invalid fixup location", 0
err_unknown_symbol:
    db  "Unknown symbol", 0
err_expect_endframe:
    db  "Expecting .ENDFRAME", 0
err_global_symbol_not_allowed:
    db  "Global symbol is not allowed here", 0
err_frame_symbol_not_allowed:
    db  "Frame symbol is not allowed here", 0
err_symbol_symbol_type_mix:
    db  "Redefining symbol type is not allowed", 0
err_symbol_already_imported:
    db  "Symbol is already imported", 0
err_symbol_already_exported:
    db  "Symbol is already exported", 0
err_constant_already_defined:
    db  "Constant symbol is already defined", 0

.EOF
