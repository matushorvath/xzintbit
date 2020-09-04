.EXPORT parse_dir_frame
.EXPORT parse_dir_endframe
.EXPORT is_frame

# from libxib/heap.s
.IMPORT free

# from frame.s
.IMPORT find_frame_symbol
.IMPORT add_frame_symbol
.IMPORT reset_frame
.IMPORT frame_head

# from lexer.s
.IMPORT get_token
.IMPORT token_type
.IMPORT token_value

# from parser.s
.IMPORT current_address
.IMPORT err_expect_eol
.IMPORT err_expect_identifier

# from error.s
.IMPORT report_error

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

    add err_duplicate_frame_symbol, 0, [rb]
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
# globals

# are we currently in a frame?
is_frame:
    db  0

##########
# error messages

err_expect_semicolon_eol:
    db  "Expecting a semicolon or a line end", 0
err_expect_comma_semicolon_eol:
    db  "Expecting a comma, a semicolon or a line end", 0
err_already_in_frame:
    db  "A frame is already active", 0
err_too_many_lists:
    db  "Expecting up to three identifier lists", 0
err_duplicate_frame_symbol:
    db  "Duplicate frame symbol definition", 0
err_not_in_frame:
    db  "There is no frame active", 0

.EOF
