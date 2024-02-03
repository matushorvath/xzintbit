.EXPORT print_modules
.EXPORT print_map

# from libxib/memory.s
.IMPORT print_mem

# from libxib/print.s
.IMPORT print_num
.IMPORT print_str

# from data.s
.IMPORT module_head
.IMPORT symbol_head

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
print_map:
.FRAME
    arb -0

    call dump_modules
    out 10
    call dump_symbols

    arb 0
    ret 0
.ENDFRAME

##########
dump_modules:
.FRAME module
    arb -1

    add [module_head], 0, [rb + module]

dump_modules_loop:
    jz  [rb + module], dump_modules_done

    add [rb + module], 0, [rb - 1]
    arb -1
    call print_num
    out ' '

    add [rb + module], MODULE_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + module]

    jz  0, dump_modules_loop

dump_modules_done:
    arb 1
    ret 0
.ENDFRAME

##########
dump_symbols:
.FRAME symbol, import
    arb -3

    add [symbol_head], 0, [rb + symbol]

dump_symbols_loop:
    jz  [rb + symbol], dump_symbols_done

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

    jz  0, dump_symbols_loop

dump_symbols_done:
    arb 3
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

.EOF
