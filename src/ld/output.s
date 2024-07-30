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
.IMPORT resolved_head

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
.FRAME module
    arb -1

    add [module_head], 0, [rb + module]

.loop:
    jz  [rb + module], .done

    # skip excluded modules
    add [rb + module], MODULE_INCLUDED, [ip + 1]
    jz  [0], .next

    # print the module address
    add [rb + module], MODULE_ADDRESS, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_num

    out ':'
    out 10

    # dump all exports and local symbols within the module
    add [rb + module], 0, [rb - 1]
    arb -1
    call print_map_exports

    add [rb + module], 0, [rb - 1]
    arb -1
    call print_map_symbols

.next:
    # advance to next module
    add [rb + module], MODULE_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + module]

    jz  0, .loop

.done:
    arb 1
    ret 0
.ENDFRAME

##########
print_map_exports:
.FRAME module; export, module_address
    arb -2

    add [rb + module], MODULE_EXPORTS_HEAD, [ip + 1]
    add [0], 0, [rb + export]

.loop:
    jz  [rb + export], .done

    # print the identifier
    out ' '
    out ' '

    add [rb + export], EXPORT_IDENTIFIER, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_str

    out ':'
    out 10

    # print the absolute address
    add map_address_str, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + module], MODULE_ADDRESS, [ip + 1]
    add [0], 0, [rb + module_address]

    add [rb + export], EXPORT_ADDRESS, [ip + 1]
    add [0], [rb + module_address], [rb - 1]
    arb -1
    call print_num

    out 10

    add [rb + export], EXPORT_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + export]

    jz  0, .loop

.done:
    arb 2
    ret 1
.ENDFRAME

##########
print_map_symbols:
.FRAME module; symbol, module_address
    arb -2

    add [rb + module], MODULE_SYMBOLS_HEAD, [ip + 1]
    add [0], 0, [rb + symbol]

.loop:
    jz  [rb + symbol], .done

    # print the parent identifier
    out ' '
    out ' '

    add [rb + symbol], SYMBOL_PARENT_IDENTIFIER, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_str

    # print the child identifier, if present
    add [rb + symbol], SYMBOL_CHILD_IDENTIFIER, [ip + 1]
    jz  [0], .after_child

    out '.'

    add [rb + symbol], SYMBOL_CHILD_IDENTIFIER, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_str

.after_child:
    out ':'
    out 10

    # print the absolute address
    add map_address_str, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + module], MODULE_ADDRESS, [ip + 1]
    add [0], 0, [rb + module_address]

    add [rb + symbol], SYMBOL_ADDRESS, [ip + 1]
    add [0], [rb + module_address], [rb - 1]
    arb -1
    call print_num

    out 10

    add [rb + symbol], SYMBOL_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + symbol]

    jz  0, .loop

.done:
    arb 2
    ret 1
.ENDFRAME

##########
map_address_str:
    db  "    address: ", 0

.EOF
