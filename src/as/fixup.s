.EXPORT add_fixup
.EXPORT process_fixups

# from libxib/heap.s
.IMPORT alloc_blocks
.IMPORT free

# from libxib/memory.s
.IMPORT inc_mem

# from error.s
.IMPORT report_global_fixup_error

# from global.s
.IMPORT find_global_symbol
.IMPORT add_global_symbol
.IMPORT global_head

# from memory.s
.IMPORT mem_head
.IMPORT mem_tail
.IMPORT mem_index

##########
add_fixup:
.FRAME fixups_head_ptr, address, line_num, column_num; fixup, tmp
    arb -2

    # allocate a block
    add FIXUP_ALLOC_SIZE, 0, [rb - 1]
    arb -1
    call alloc_blocks
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
    add [rb + fixups_head_ptr], 0, [ip + 1]
    add [0], 0, [rb + tmp]

    # set pointer to next fixup
    add [rb + fixup], FIXUP_NEXT_PTR, [ip + 3]
    add [rb + tmp], 0, [0]

    # set new fixup list head
    add [rb + fixups_head_ptr], 0, [ip + 3]
    add [rb + fixup], 0, [0]

    arb 2
    ret 4
.ENDFRAME

##########
process_fixups:
.FRAME tmp, global, child
    arb -3

    add [global_head], 0, [rb + global]

.global_loop:
    # do we have more global symbols?
    jz  [rb + global], .done

    # special handling of imported symbols, they don't require fixups
    add [rb + global], GLOBAL_TYPE, [ip + 1]
    eq  [0], 1, [rb + tmp]
    jnz [rb + tmp], .global_next

    # process all fixups for this global symbol
    add [rb + global], 0, [rb - 1]
    arb -1
    call process_symbol_fixups

    # process child symbols as well, if any
    add [rb + global], GLOBAL_CHILDREN_HEAD, [ip + 1]
    add [0], 0, [rb + child]

.child_loop:
    # do we have more child symbols?
    jz  [rb + child], .global_next

    # process all fixups for this child symbol
    add [rb + child], 0, [rb - 1]
    arb -1
    call process_symbol_fixups

    # move to next child symbol
    add [rb + child], GLOBAL_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + child]

    jz  0, .child_loop

.global_next:
    # move to next global symbol
    add [rb + global], GLOBAL_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + global]

    jz  0, .global_loop

.done:
    arb 3
    ret 0
.ENDFRAME

##########
process_symbol_fixups:
.FRAME symbol; tmp, fixup, symbol_address, fixup_address
    arb -4

    # the symbol needs to have an address
    add [rb + symbol], GLOBAL_ADDRESS, [ip + 1]
    add [0], 0, [rb + symbol_address]

    eq  [rb + symbol_address], -1, [rb + tmp]
    jz  [rb + tmp], .have_address

    add [rb + symbol], 0, [rb + 1]
    add err_unknown_symbol, 0, [rb]
    call report_global_fixup_error

.have_address:
    # iterate through all fixups for this symbol
    add [rb + symbol], GLOBAL_FIXUPS_HEAD, [ip + 1]
    add [0], 0, [rb + fixup]

.loop:
    # do we have more fixups for this symbol?
    jz  [rb + fixup], .done

    # read fixup address
    add [rb + fixup], FIXUP_ADDRESS, [ip + 1]
    add [0], 0, [rb + fixup_address]

    # increment the memory to do the fixup
    add [mem_head], 0, [rb - 1]
    add [mem_tail], 0, [rb - 2]
    add [mem_index], 0, [rb - 3]
    add [rb + fixup_address], 0, [rb - 4]
    add [rb + symbol_address], 0, [rb - 5]
    arb -5
    call inc_mem

    # move to next fixup
    add [rb + fixup], FIXUP_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + fixup]

    jz  0, .loop

.done:
    arb 4
    ret 1
.ENDFRAME

##########
# error messages

err_unknown_symbol:
    db  "Unknown symbol", 0

.EOF
