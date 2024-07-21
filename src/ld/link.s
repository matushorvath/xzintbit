.EXPORT link_imports

# from libxib/memory.s
.IMPORT inc_mem
.IMPORT mem_block_size

# from data.s
.IMPORT symbol_head

##########
link_imports:
.FRAME symbol, import, tmp
    arb -3

    add [symbol_head], 0, [rb + symbol]

link_imports_symbols_loop:
    jz  [rb + symbol], link_imports_symbols_done

    add [rb + symbol], EXPORT_IMPORTS_HEAD, [ip + 1]
    add [0], 0, [rb + import]

link_imports_imports_loop:
    jz  [rb + import], link_imports_imports_done

    add [rb + symbol], 0, [rb - 1]
    add [rb + import], 0, [rb - 2]
    arb -2
    call link_one_import

    add [rb + import], IMPORT_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + import]

    jz  0, link_imports_imports_loop

link_imports_imports_done:
    add [rb + symbol], EXPORT_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + symbol]

    jz  0, link_imports_symbols_loop

link_imports_symbols_done:
    arb 3
    ret 0
.ENDFRAME

##########
link_one_import:
.FRAME symbol, import; buffer, limit, index, import_module, symbol_address, tmp
    arb -6

    add [rb + import], IMPORT_MODULE, [ip + 1]
    add [0], 0, [rb + import_module]

    # symbol address = symbol module address + symbol address within module
    add [rb + symbol], EXPORT_MODULE, [ip + 1]
    add [0], MODULE_ADDRESS, [ip + 1]
    add [0], 0, [rb + symbol_address]
    add [rb + symbol], EXPORT_ADDRESS, [ip + 1]
    add [0], [rb + symbol_address], [rb + symbol_address]

    add [rb + import], IMPORT_FIXUPS_HEAD, [ip + 1]
    add [0], 0, [rb + buffer]

link_one_import_block:
    jz  [rb + buffer], link_one_import_done
    add 1, 0, [rb + index]

    # maximum index within a block is mem_block_size, except for last block
    add [mem_block_size], 0, [rb + limit]
    add [rb + import], IMPORT_FIXUPS_TAIL, [ip + 1]
    eq  [0], [rb + buffer], [rb + tmp]
    jz  [rb + tmp], link_one_import_byte

    add [rb + import], IMPORT_FIXUPS_INDEX, [ip + 1]
    add [0], 0, [rb + limit]

link_one_import_byte:
    lt  [rb + index], [rb + limit], [rb + tmp]
    jz  [rb + tmp], link_one_import_block_done

    # increment memory at in import module at import fixup address
    # by export module address + export symbol address
    #
    # for (fixup in import.fixups) {
    #     inc_mem(import.module.code, fixup, export.module.address + export.address)
    # }

    add [rb + import_module], MODULE_CODE_HEAD, [ip + 1]
    add [0], 0, [rb - 1]
    add [rb + import_module], MODULE_CODE_TAIL, [ip + 1]
    add [0], 0, [rb - 2]
    add [rb + import_module], MODULE_CODE_INDEX, [ip + 1]
    add [0], 0, [rb - 3]
    add [rb + buffer], [rb + index], [ip + 1]
    add [0], 0, [rb - 4]
    add [rb + symbol_address], 0, [rb - 5]
    arb -5
    call inc_mem

    add [rb + index], 1, [rb + index]
    jz  0, link_one_import_byte

link_one_import_block_done:
    # next block in linked list
    add [rb + buffer], 0, [ip + 1]
    add [0], 0, [rb + buffer]

    jz  0, link_one_import_block

link_one_import_done:
    arb 6
    ret 2
.ENDFRAME

.EOF
