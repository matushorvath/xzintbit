.EXPORT print_modules
.EXPORT print_map

# from libxib/memory.s
.IMPORT print_mem
.IMPORT pretty_print_mem
.IMPORT inc_all_mem

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

    # dump all symbols within the module
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
print_map_symbols:
.FRAME module; module_address, symbol
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
    add .str_address, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + module], MODULE_ADDRESS, [ip + 1]
    add [0], 0, [rb + module_address]

    add [rb + symbol], SYMBOL_ADDRESS, [ip + 1]
    add [0], [rb + module_address], [rb - 1]
    arb -1
    call print_num

    out 10

    # convert addresses in the fixup list to absolute
    add [rb + symbol], SYMBOL_FIXUPS_HEAD, [ip + 1]
    add [0], 0, [rb - 1]
    add [rb + symbol], SYMBOL_FIXUPS_TAIL, [ip + 1]
    add [0], 0, [rb - 2]
    add [rb + symbol], SYMBOL_FIXUPS_INDEX, [ip + 1]
    add [0], 0, [rb - 3]
    add [rb + module_address], 0, [rb - 4]
    arb -4
    call inc_all_mem

    # print the references
    add .str_references, 0, [rb - 1]
    arb -1
    call print_str

    out '['

    add [rb + symbol], SYMBOL_FIXUPS_HEAD, [ip + 1]
    add [0], 0, [rb - 1]
    add [rb + symbol], SYMBOL_FIXUPS_TAIL, [ip + 1]
    add [0], 0, [rb - 2]
    add [rb + symbol], SYMBOL_FIXUPS_INDEX, [ip + 1]
    add [0], 0, [rb - 3]
    arb -3
    call pretty_print_mem

    out ']'
    out 10

    add [rb + symbol], SYMBOL_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + symbol]

    jz  0, .loop

.done:
    arb 2
    ret 1

.str_address:
    db  "    address: ", 0
.str_references:
    db  "    references: ", 0
.ENDFRAME

# TODO add the imported locations to fixups
# increased by their respecitive MODULE_ADDRESS, only from included modules

.EOF
