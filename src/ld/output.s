.EXPORT print_modules
.EXPORT print_map

# from libxib/memory.s
.IMPORT print_mem
.IMPORT pretty_print_mem

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

.loop:
    jz  [rb + module], .done

    add [rb + module], MODULE_INCLUDED, [ip + 1]
    jz  [0], .next
    add [rb + module], MODULE_CODE_HEAD, [ip + 1]
    jz  [0], .next

    jnz [rb + first], .skip_comma
    out ','

.skip_comma:
    add 0, 0, [rb + first]

    add [rb + module], MODULE_CODE_HEAD, [ip + 1]
    add [0], 0, [rb - 1]
    add [rb + module], MODULE_CODE_TAIL, [ip + 1]
    add [0], 0, [rb - 2]
    add [rb + module], MODULE_CODE_INDEX, [ip + 1]
    add [0], 0, [rb - 3]
    arb -3
    call print_mem

.next:
    add [rb + module], MODULE_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + module]

    jz  0, .loop

.done:
    out 10

    arb 3
    ret 0
.ENDFRAME

##########
print_map:
.FRAME
    arb -0

    call dump_modules
    call dump_symbols

    arb 0
    ret 0
.ENDFRAME

##########
dump_modules:
.FRAME module, count
    arb -2

    add .str_start, 0, [rb - 1]
    arb -1
    call print_str

    add [module_head], 0, [rb + module]
    jz  [rb + module], .done

    add .str_head, 0, [rb - 1]
    arb -1
    call print_str

    # add a line end after each N modules
    add [.num_modules_on_line], 0, [rb + count]

.loop:
    # print module address
    add [rb + module], MODULE_ADDRESS, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_num

.next:
    # advance to next module
    add [rb + module], MODULE_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + module]

    jz  [rb + module], .done

    add [rb + module], MODULE_INCLUDED, [ip + 1]
    jz  [0], .next

    # more modules, print separator
    add [rb + count], -1, [rb + count]
    jz  [rb + count], .end_line

    add .str_space_separator, 0, [rb - 1]
    arb -1
    call print_str

    jz  0, .loop

.end_line:
    add .str_line_separator, 0, [rb - 1]
    arb -1
    call print_str

    add [.num_modules_on_line], 0, [rb + count]

    jz  0, .loop

.done:
    add .str_end, 0, [rb - 1]
    arb -1
    call print_str

    arb 2
    ret 0

.num_modules_on_line:
    db  10
.str_start:
    db  "modules:", 0
.str_head:
    db  " [", 10, "  ", 0
.str_space_separator:
    db  ", ", 0
.str_line_separator:
    db  ",", 10, "  ", 0
.str_end:
    db  10, "]", 10, 0

.ENDFRAME

##########
dump_symbols:
.FRAME symbol, import, module
    arb -3

    add .str_start, 0, [rb - 1]
    arb -1
    call print_str

    add [symbol_head], 0, [rb + symbol]

.loop:
    jz  [rb + symbol], .done

    # get the module for this symbol
    add [rb + symbol], EXPORT_MODULE, [ip + 1]
    add [0], 0, [rb + module]

    # skip exports from excluded modules
    add [rb + module], MODULE_INCLUDED, [ip + 1]
    jz  [0], .next_export

    # print symbol identifier
    add .str_symbol_start, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + symbol], EXPORT_IDENTIFIER, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_str

    add .str_symbol_end, 0, [rb - 1]
    arb -1
    call print_str

    # print export
    add .str_export_module_start, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + module], MODULE_ADDRESS, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_num

    add .str_export_offset_start, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + symbol], EXPORT_ADDRESS, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_num

    add .str_export_end, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + symbol], EXPORT_IMPORTS_HEAD, [ip + 1]
    add [0], 0, [rb + import]

    # print array of imports header
    add .str_imports_start, 0, [rb - 1]
    arb -1
    call print_str

.imports_loop:
    jz  [rb + import], .next_export

    # get the module for this import
    add [rb + import], IMPORT_MODULE, [ip + 1]
    add [0], 0, [rb + module]

    # skip imports into excluded modules
    add [rb + module], MODULE_INCLUDED, [ip + 1]
    jz  [0], .next_import

    # print array of imports
    add .str_import_module_start, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + module], MODULE_ADDRESS, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_num

    add .str_import_offsets_start, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + import], IMPORT_FIXUPS_HEAD, [ip + 1]
    add [0], 0, [rb - 1]
    add [rb + import], IMPORT_FIXUPS_TAIL, [ip + 1]
    add [0], 0, [rb - 2]
    add [rb + import], IMPORT_FIXUPS_INDEX, [ip + 1]
    add [0], 0, [rb - 3]
    arb -3

    call pretty_print_mem

    add .str_import_end, 0, [rb - 1]
    arb -1
    call print_str

.next_import:
    add [rb + import], IMPORT_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + import]

    jz  0, .imports_loop

.next_export:
    add [rb + symbol], EXPORT_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + symbol]

    jz  0, .loop

.done:
    arb 3
    ret 0

.str_start:
    db  "symbols:", 10, 0
.str_symbol_start:
    db  "  ", 0
.str_symbol_end:
    db  ":", 10, 0

.str_export_module_start:
    db  "    export:", 10, "      module: ", 0
.str_export_offset_start:
    db  10, "      offset: ", 0
.str_export_end:
    db  10, 0

.str_imports_start:
    db  "    imports:", 10, 0
.str_import_module_start:
    db  "      - module: ", 0
.str_import_offsets_start:
    db  10, "        offsets: [", 0
.str_import_end:
    db  "]", 10, 0

.ENDFRAME

.EOF
