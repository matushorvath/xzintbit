.EXPORT relocate

# from libxib/memory.s
.IMPORT inc_mem
.IMPORT mem_block_size

# from data.s
.IMPORT module_head

##########
relocate:
.FRAME module, address, tmp
    arb -3

    add [module_head], 0, [rb + module]
    add 0, 0, [rb + address]

.loop:
    jz  [rb + module], .done

    # skip modules that are not included
    add [rb + module], MODULE_INCLUDED, [ip + 1]
    jz  [0], .next

    # set module address
    add [rb + module], MODULE_ADDRESS, [ip + 3]
    add [rb + address], 0, [0]

    # relocate the module
    add [rb + module], 0, [rb - 1]
    arb -1
    call relocate_module

    # add module size to the address
    add [rb + module], MODULE_CODE_LENGTH, [ip + 1]
    add [0], [rb + address], [rb + address]

.next:
    add [rb + module], MODULE_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + module]

    jz  0, .loop

.done:
    arb 3
    ret 0
.ENDFRAME

##########
relocate_module:
.FRAME module; buffer, limit, index, tmp
    arb -4

    add [rb + module], MODULE_RELOC_HEAD, [ip + 1]
    add [0], 0, [rb + buffer]

.block:
    jz  [rb + buffer], .done
    add 1, 0, [rb + index]

    # maximum index within a block is mem_block_size, except for last block
    add [mem_block_size], 0, [rb + limit]
    add [rb + module], MODULE_RELOC_TAIL, [ip + 1]
    eq  [0], [rb + buffer], [rb + tmp]
    jz  [rb + tmp], .byte

    add [rb + module], MODULE_RELOC_INDEX, [ip + 1]
    add [0], 0, [rb + limit]

.byte:
    lt  [rb + index], [rb + limit], [rb + tmp]
    jz  [rb + tmp], .block_done

    # increment memory at relocation address by module address
    add [rb + module], MODULE_CODE_HEAD, [ip + 1]
    add [0], 0, [rb - 1]
    add [rb + module], MODULE_CODE_TAIL, [ip + 1]
    add [0], 0, [rb - 2]
    add [rb + module], MODULE_CODE_INDEX, [ip + 1]
    add [0], 0, [rb - 3]
    add [rb + buffer], [rb + index], [ip + 1]
    add [0], 0, [rb - 4]
    add [rb + module], MODULE_ADDRESS, [ip + 1]
    add [0], 0, [rb - 5]
    arb -5
    call inc_mem

    add [rb + index], 1, [rb + index]
    jz  0, .byte

.block_done:
    # next block in linked list
    add [rb + buffer], 0, [ip + 1]
    add [0], 0, [rb + buffer]

    jz  0, .block

.done:
    arb 4
    ret 1
.ENDFRAME

.EOF
