.EXPORT output_object

# from libxib/print.s
.IMPORT print_num
.IMPORT print_str

# from libxib/memory.s
.IMPORT print_mem

# from error.s
.IMPORT report_symbol_fixup_error

# from global.s
.IMPORT global_head
.IMPORT current_address_fixups_head

# from memory.s
.IMPORT mem_head
.IMPORT mem_tail
.IMPORT mem_index

##########
output_object:
.FRAME
    call print_code
    call print_reloc
    call print_imports
    call print_exports
    call print_symbols

    ret 0
.ENDFRAME

##########
print_code:
.FRAME
    # print .C
    out '.'
    out 'C'
    out 10

    jz  [mem_head], .done

    # print compiled memory contents
    add [mem_head], 0, [rb - 1]
    add [mem_tail], 0, [rb - 2]
    add [mem_index], 0, [rb - 3]
    arb -3
    call print_mem

    out 10

.done:
    ret 0
.ENDFRAME

##########
print_reloc:
.FRAME tmp, global, child, printed
    arb -4

    add 0, 0, [rb + printed]

    # print .R
    out '.'
    out 'R'
    out 10

    # print relocations for the current_address symbol
    add current_address_fixups_head, 0, [ip + 1]
    add [0], 0, [rb - 1]
    add [rb + printed], 0, [rb - 2]
    arb -2
    call print_fixups_list
    add [rb + printed], [rb - 4], [rb + printed]

    # print relocations for the regular symbols
    add [global_head], 0, [rb + global]

.global_loop:
    # do we have more symbols?
    jz  [rb + global], .done

    # check symbol type (skip imported and constants)
    add [rb + global], GLOBAL_TYPE, [ip + 1]
    eq  [0], 1, [rb + tmp]
    jnz [rb + tmp], .global_next
    add [rb + global], GLOBAL_TYPE, [ip + 1]
    eq  [0], 3, [rb + tmp]
    jnz [rb + tmp], .global_next

    # print relocations for the global symbol
    add [rb + global], GLOBAL_FIXUPS_HEAD, [ip + 1]
    add [0], 0, [rb - 1]
    add [rb + printed], 0, [rb - 2]
    arb -2
    call print_fixups_list
    add [rb + printed], [rb - 4], [rb + printed]

    # process child symbols as well, if any
    add [rb + global], GLOBAL_CHILDREN_HEAD, [ip + 1]
    add [0], 0, [rb + child]

.child_loop:
    # do we have more child symbols?
    jz  [rb + child], .global_next

    # print relocations for this child symbol
    add [rb + child], GLOBAL_FIXUPS_HEAD, [ip + 1]
    add [0], 0, [rb - 1]
    add [rb + printed], 0, [rb - 2]
    arb -2
    call print_fixups_list
    add [rb + printed], [rb - 4], [rb + printed]

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
    # skip endline if nothing was printed
    jz  [rb + printed], .after_eol
    out 10

.after_eol:
    arb 4
    ret 0
.ENDFRAME

##########
print_imports:
.FRAME tmp, symbol, address
    arb -3

    # print .I
    out '.'
    out 'I'
    out 10

    add [global_head], 0, [rb + symbol]

.symbol_loop:
    # do we have more symbols?
    jz  [rb + symbol], print_imports_done

    # check symbol type
    add [rb + symbol], GLOBAL_TYPE, [ip + 1]
    eq  [0], 1, [rb + tmp]
    jz  [rb + tmp], .symbol_done

    # don't print symbols with no fixups
    add [rb + symbol], GLOBAL_FIXUPS_HEAD, [ip + 1]
    jz  [0], .symbol_done

    # imported symbols must not have an address
    add [rb + symbol], GLOBAL_ADDRESS, [ip + 1]
    add [0], 0, [rb + address]

    eq  [rb + address], -1, [rb + tmp]
    jnz [rb + tmp], .no_address

    add [rb + symbol], 0, [rb + 1]
    add err_imported_symbol_defined, 0, [rb]
    call report_symbol_fixup_error

.no_address:
    # print the identifier
    add [rb + symbol], GLOBAL_IDENTIFIER_PTR, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_str

    # print a colon followed by a list of fixup addresses
    out ':'

    # iterate through all fixups for this symbol
    add [rb + symbol], GLOBAL_FIXUPS_HEAD, [ip + 1]
    add [0], 0, [rb - 1]
    add 0, 0, [rb - 2]                  # always skip the initial comma
    arb -2
    call print_fixups_list

    out 10

.symbol_done:
    # move to next symbol
    add [rb + symbol], GLOBAL_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + symbol]

    jz  0, .symbol_loop

print_imports_done:
    arb 3
    ret 0
.ENDFRAME

##########
print_exports:
.FRAME tmp, symbol
    arb -2

    # print .E
    out '.'
    out 'E'
    out 10

    add [global_head], 0, [rb + symbol]

.symbol_loop:
    # do we have more symbols?
    jz  [rb + symbol], .done

    # check symbol type
    add [rb + symbol], GLOBAL_TYPE, [ip + 1]
    eq  [0], 2, [rb + tmp]
    jz  [rb + tmp], .symbol_done

    # print the identifier and the address
    add [rb + symbol], 0, [rb - 1]
    arb -1
    call print_identifier_and_address

    out 10

.symbol_done:
    # move to next symbol
    add [rb + symbol], GLOBAL_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + symbol]

    jz  0, .symbol_loop

.done:
    arb 2
    ret 0
.ENDFRAME

##########
print_symbols:
.FRAME tmp, global, child
    arb -3

    # print .S
    out '.'
    out 'S'
    out 10

    # print the regular symbols
    add [global_head], 0, [rb + global]

.global_loop:
    # do we have more symbols?
    jz  [rb + global], .done

    # check symbol type, print local symbols only
    add [rb + global], GLOBAL_TYPE, [ip + 1]
    jnz [0], .global_next

    # print the identifier and the address
    add [rb + global], 0, [rb - 1]
    arb -1
    call print_identifier_and_address

    out 10

    # process child symbols as well, if any
    add [rb + global], GLOBAL_CHILDREN_HEAD, [ip + 1]
    add [0], 0, [rb + child]

.child_loop:
    # do we have more child symbols?
    jz  [rb + child], .global_next

    # print the identifier and the address
    add [rb + child], 0, [rb - 1]
    arb -1
    call print_identifier_and_address

    out 10

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
print_identifier_and_address:
.FRAME symbol; tmp, parent, address
    arb -3

    # local symbols must have an address
    add [rb + symbol], GLOBAL_ADDRESS, [ip + 1]
    add [0], 0, [rb + address]

    eq  [rb + address], -1, [rb + tmp]
    jz  [rb + tmp], .have_address

    add [rb + symbol], 0, [rb + 1]
    add err_unknown_symbol, 0, [rb]
    call report_symbol_fixup_error

.have_address:
    # is this a child symbol?
    add [rb + symbol], GLOBAL_PARENT, [ip + 1]
    add [0], 0, [rb + parent]

    jz  [rb + parent], .after_parent

    # yes, print the parent identifier first
    add [rb + parent], GLOBAL_IDENTIFIER_PTR, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_str

    out '.'

.after_parent:
    # print the identifier
    add [rb + symbol], GLOBAL_IDENTIFIER_PTR, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_str

    out ':'

    # print the address
    add [rb + address], 0, [rb - 1]
    arb -1
    call print_num

    arb 3
    ret 1
.ENDFRAME

##########
print_fixups_list:
.FRAME fixups_head, printed_in; printed_out, fixup, address
    arb -3

    # iterate through all fixups starting from the head
    add [rb + fixups_head], 0, [rb + fixup]

.loop:
    # do we have more fixups for this symbol?
    jz  [rb + fixup], .done

    # skip comma when printing first reloc
    jz  [rb + printed_in], .skip_comma
    out ','

.skip_comma:
    add 1, 0, [rb + printed_in]

    # read fixup address
    add [rb + fixup], FIXUP_ADDRESS, [ip + 1]
    add [0], 0, [rb + address]

    # print the fixup
    add [rb + address], 0, [rb - 1]
    arb -1
    call print_num

    # move to next fixup
    add [rb + fixup], FIXUP_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + fixup]

    jz  0, .loop

.done:
    add [rb + printed_in], 0, [rb + printed_out]

    arb 3
    ret 2
.ENDFRAME

##########
# error messages

err_unknown_symbol:
    db  "Unknown symbol", 0
err_imported_symbol_defined:
    db  "Imported symbol must not have an address defined", 0

.EOF
