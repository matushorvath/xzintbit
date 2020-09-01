.EXPORT relocate
.EXPORT connect_imports
.EXPORT print_modules
#.EXPORT dump_symbols

.EXPORT module_head
.EXPORT module_tail
.EXPORT symbol_head
.EXPORT symbol_tail

# from libxib/print.s
.IMPORT print_num
.IMPORT print_str

# from memory.s
.IMPORT inc_mem
.IMPORT print_mem

##########
relocate:
.FRAME module, address, tmp
    arb -3

    add [module_head], 0, [rb + module]
    add 0, 0, [rb + address]

relocate_loop:
    jz  [rb + module], relocate_done

    # skip modules that are not included
    add [rb + module], MODULE_INCLUDED, [ip + 1]
    jz  [0], relocate_next

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

relocate_next:
    add [rb + module], MODULE_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + module]

    jz  0, relocate_loop

 relocate_done:
    arb 3
    ret 0
.ENDFRAME

##########
relocate_module:
.FRAME module; buffer, limit, index, tmp
    arb -4

    add [rb + module], MODULE_RELOC_HEAD, [ip + 1]
    add [0], 0, [rb + buffer]

relocate_module_block:
    jz  [rb + buffer], relocate_module_done
    add 1, 0, [rb + index]

    # maximum index within a block is MEM_BLOCK_SIZE, except for last block
    add MEM_BLOCK_SIZE, 0, [rb + limit]
    add [rb + module], MODULE_RELOC_TAIL, [ip + 1]
    eq  [0], [rb + buffer], [rb + tmp]
    jz  [rb + tmp], relocate_module_byte

    add [rb + module], MODULE_RELOC_INDEX, [ip + 1]
    add [0], 0, [rb + limit]

relocate_module_byte:
    lt  [rb + index], [rb + limit], [rb + tmp]
    jz  [rb + tmp], relocate_module_block_done

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
    jz  0, relocate_module_byte

relocate_module_block_done:
    # next block in linked list
    add [rb + buffer], 0, [ip + 1]
    add [0], 0, [rb + buffer]

    jz  0, relocate_module_block

relocate_module_done:
    arb 4
    ret 1
.ENDFRAME

##########
connect_imports:
.FRAME symbol, import, tmp
    arb -3

    add [symbol_head], 0, [rb + symbol]

connect_imports_symbols_loop:
    jz  [rb + symbol], connect_imports_symbols_done

    add [rb + symbol], EXPORT_IMPORTS_HEAD, [ip + 1]
    add [0], 0, [rb + import]

connect_imports_imports_loop:
    jz  [rb + import], connect_imports_imports_done

    add [rb + symbol], 0, [rb - 1]
    add [rb + import], 0, [rb - 2]
    arb -2
    call connect_one_import

    add [rb + import], IMPORT_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + import]

    jz  0, connect_imports_imports_loop

connect_imports_imports_done:
    add [rb + symbol], EXPORT_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + symbol]

    jz  0, connect_imports_symbols_loop

connect_imports_symbols_done:
    arb 3
    ret 0
.ENDFRAME

##########
connect_one_import:
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

connect_one_import_block:
    jz  [rb + buffer], connect_one_import_done
    add 1, 0, [rb + index]

    # maximum index within a block is MEM_BLOCK_SIZE, except for last block
    add MEM_BLOCK_SIZE, 0, [rb + limit]
    add [rb + import], IMPORT_FIXUPS_TAIL, [ip + 1]
    eq  [0], [rb + buffer], [rb + tmp]
    jz  [rb + tmp], connect_one_import_byte

    add [rb + import], IMPORT_FIXUPS_INDEX, [ip + 1]
    add [0], 0, [rb + limit]

connect_one_import_byte:
    lt  [rb + index], [rb + limit], [rb + tmp]
    jz  [rb + tmp], connect_one_import_block_done

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
    jz  0, connect_one_import_byte

connect_one_import_block_done:
    # next block in linked list
    add [rb + buffer], 0, [ip + 1]
    add [0], 0, [rb + buffer]

    jz  0, connect_one_import_block

connect_one_import_done:
    arb 6
    ret 2
.ENDFRAME

##########
print_modules:
.FRAME module, first, tmp
    arb -3

    # print all included modules
    add [module_head], 0, [rb + module]
    add 1, 0, [rb + first]

print_modules_loop:
    jz  [rb + module], print_modules_done

    add [rb + module], MODULE_INCLUDED, [ip + 1]
    jz  [0], print_modules_next
    add [rb + module], MODULE_CODE_HEAD, [ip + 1]
    jz  [0], print_modules_next

    jnz [rb + first], print_modules_skip_comma
    out ','

print_modules_skip_comma:
    add 0, 0, [rb + first]

    add [rb + module], MODULE_CODE_HEAD, [ip + 1]
    add [0], 0, [rb - 1]
    add [rb + module], MODULE_CODE_TAIL, [ip + 1]
    add [0], 0, [rb - 2]
    add [rb + module], MODULE_CODE_INDEX, [ip + 1]
    add [0], 0, [rb - 3]
    arb -3
    call print_mem

print_modules_next:
    add [rb + module], MODULE_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + module]

    jz  0, print_modules_loop

print_modules_done:
    out 10

    arb 3
    ret 0
.ENDFRAME

##########
dump_symbols:
.FRAME module, symbol, import, tmp
    arb -4

    add [module_head], 0, [rb + module]

dump_symbols_modules_loop:
    jz  [rb + module], dump_symbols_modules_done

    add [rb + module], 0, [rb - 1]
    arb -1
    call print_num
    out ' '

    add [rb + module], MODULE_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + module]

    jz  0, dump_symbols_modules_loop

dump_symbols_modules_done:
    add [symbol_head], 0, [rb + symbol]

dump_symbols_symbols_loop:
    jz  [rb + symbol], dump_symbols_symbols_done

    add [rb + symbol], EXPORT_IDENTIFIER, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_str

    add dump_symbols_str_export_mod_start, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + symbol], EXPORT_MODULE, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_num

    add dump_symbols_str_export_mod_end, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + symbol], EXPORT_IMPORTS_HEAD, [ip + 1]
    add [0], 0, [rb + import]

dump_symbols_imports_loop:
    jz  [rb + import], dump_symbols_imports_done

    add dump_symbols_str_import_mod_start, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + import], IMPORT_MODULE, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_num

    add dump_symbols_str_import_mod_end, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + import], IMPORT_FIXUPS_HEAD, [ip + 1]
    add [0], 0, [rb - 1]
    add [rb + import], IMPORT_FIXUPS_TAIL, [ip + 1]
    add [0], 0, [rb - 2]
    add [rb + import], IMPORT_FIXUPS_INDEX, [ip + 1]
    add [0], 0, [rb - 3]
    arb -3
    call print_mem

    add dump_symbols_str_import_end, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + import], IMPORT_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + import]

    jz  0, dump_symbols_imports_loop

dump_symbols_imports_done:
    add [rb + symbol], EXPORT_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + symbol]

    out 10

    jz  0, dump_symbols_symbols_loop

dump_symbols_symbols_done:
    arb 4
    ret 0

dump_symbols_str_export_mod_start:
    db  " (", 0
dump_symbols_str_export_mod_end:
    db  "):", 0
dump_symbols_str_import_mod_start:
    db  " [", 0
dump_symbols_str_import_mod_end:
    db  "] {", 0
dump_symbols_str_import_end:
    db  "}", 0
.ENDFRAME

##########
# globals

# loaded modules
module_head:
    db 0
module_tail:
    db 0

# included symbols
symbol_head:
    db 0
symbol_tail:
    db 0

.EOF
