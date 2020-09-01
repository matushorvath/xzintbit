.EXPORT add_fixup
.EXPORT do_fixups

# from libxib/heap.s
.IMPORT alloc

# from global.s
.IMPORT find_global_symbol
.IMPORT add_global_symbol
.IMPORT global_head

# from memory.s
.IMPORT inc_mem_at

# from util.s
.IMPORT report_symbol_error

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
# error messages

err_unknown_symbol:
    db  "Unknown symbol", 0

.EOF
