.EXPORT output_object

# from libxib/print.s
.IMPORT print_num
.IMPORT print_str

# from libxib/memory.s
.IMPORT print_mem

# from global.s
.IMPORT global_head

# from memory.s
.IMPORT mem_head
.IMPORT mem_tail
.IMPORT mem_index

# from error.s
.IMPORT report_symbol_error

##########
output_object:
.FRAME
    arb -0

    call print_code
    call print_reloc
    call print_imports
    call print_exports

    arb 0
    ret 0
.ENDFRAME

##########
print_code:
.FRAME
    arb -0

    # print .C
    out '.'
    out 'C'
    out 10

    # print compiled memory contents
    add [mem_head], 0, [rb - 1]
    add [mem_tail], 0, [rb - 2]
    add [mem_index], 0, [rb - 3]
    arb -3
    call print_mem

    out 10

    arb 0
    ret 0
.ENDFRAME

##########
print_reloc:
.FRAME tmp, symbol, fixup, symbol_address, fixup_address, first
    arb -6

    add 1, 0, [rb + first]

    # print .R
    out '.'
    out 'R'
    out 10

    add [global_head], 0, [rb + symbol]

print_reloc_symbol:
    # do we have more symbols?
    jz  [rb + symbol], print_reloc_done

    # check symbol type (skip imported and constants)
    add [rb + symbol], GLOBAL_TYPE, [ip + 1]
    eq  [0], 1, [rb + tmp]
    jnz [rb + tmp], print_reloc_symbol_done
    add [rb + symbol], GLOBAL_TYPE, [ip + 1]
    eq  [0], 3, [rb + tmp]
    jnz [rb + tmp], print_reloc_symbol_done

    # iterate through all fixups for this symbol
    add [rb + symbol], GLOBAL_FIXUPS_HEAD, [ip + 1]
    add [0], 0, [rb + fixup]

    jz  [rb + fixup], print_reloc_symbol_done

print_reloc_fixup:
    # do we have more fixups for this symbol?
    jz  [rb + fixup], print_reloc_symbol_done

    # skip comma when printing first reloc
    jnz [rb + first], print_reloc_skip_comma
    out ','

print_reloc_skip_comma:
    add 0, 0, [rb + first]

    # read fixup address
    add [rb + fixup], FIXUP_ADDRESS, [ip + 1]
    add [0], 0, [rb + fixup_address]

    # print the fixup
    add [rb + fixup_address], 0, [rb - 1]
    arb -1
    call print_num

    # move to next fixup
    add [rb + fixup], FIXUP_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + fixup]

    jz  0, print_reloc_fixup

print_reloc_symbol_done:
    # move to next symbol
    add [rb + symbol], GLOBAL_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + symbol]

    jz  0, print_reloc_symbol

print_reloc_done:
    # skip endline if nothing was printed
    jnz [rb + first], print_reloc_after_eol
    out 10

print_reloc_after_eol:
    arb 6
    ret 0
.ENDFRAME

##########
print_imports:
.FRAME tmp, symbol, fixup, symbol_address, fixup_address
    arb -5

    # print .I
    out '.'
    out 'I'
    out 10

    add [global_head], 0, [rb + symbol]

print_imports_symbol:
    # do we have more symbols?
    jz  [rb + symbol], print_imports_done

    # check symbol type
    add [rb + symbol], GLOBAL_TYPE, [ip + 1]
    eq  [0], 1, [rb + tmp]
    jz  [rb + tmp], print_imports_symbol_done

    # don't print symbols with no fixups
    add [rb + symbol], GLOBAL_FIXUPS_HEAD, [ip + 1]
    jz  [0], print_imports_symbol_done

    # imported symbols must not have an address
    add [rb + symbol], GLOBAL_ADDRESS, [ip + 1]
    add [0], 0, [rb + symbol_address]

    eq  [rb + symbol_address], -1, [rb + tmp]
    jnz [rb + tmp], print_imports_no_address

    add [rb + symbol], 0, [rb + 1]
    add err_imported_symbol_defined, 0, [rb]
    call report_symbol_error

print_imports_no_address:
    # print the identifier
    add [rb + symbol], GLOBAL_IDENTIFIER, [rb - 1]
    arb -1
    call print_str

    # print a colon followed by a list of fixup addresses
    out ':'

    # iterate through all fixups for this symbol
    add [rb + symbol], GLOBAL_FIXUPS_HEAD, [ip + 1]
    add [0], 0, [rb + fixup]

    jz  [rb + fixup], print_imports_symbol_line_end

print_imports_fixup:
    # read fixup address
    add [rb + fixup], FIXUP_ADDRESS, [ip + 1]
    add [0], 0, [rb + fixup_address]

    # print the fixup
    add [rb + fixup_address], 0, [rb - 1]
    arb -1
    call print_num

    # move to next fixup
    add [rb + fixup], FIXUP_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + fixup]

    # do we have more fixups for this symbol?
    jz  [rb + fixup], print_imports_symbol_line_end

    # print a comma
    out ','

    jz  0, print_imports_fixup

print_imports_symbol_line_end:
    out 10

print_imports_symbol_done:
    # move to next symbol
    add [rb + symbol], GLOBAL_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + symbol]

    jz  0, print_imports_symbol

print_imports_done:
    arb 5
    ret 0
.ENDFRAME

##########
print_exports:
.FRAME tmp, symbol, symbol_address
    arb -3

    # print .E
    out '.'
    out 'E'
    out 10

    add [global_head], 0, [rb + symbol]

print_exports_symbol:
    # do we have more symbols?
    jz  [rb + symbol], print_exports_done

    # check symbol type
    add [rb + symbol], GLOBAL_TYPE, [ip + 1]
    eq  [0], 2, [rb + tmp]
    jz  [rb + tmp], print_exports_symbol_done

    # exported symbols must have an address
    add [rb + symbol], GLOBAL_ADDRESS, [ip + 1]
    add [0], 0, [rb + symbol_address]

    eq  [rb + symbol_address], -1, [rb + tmp]
    jz  [rb + tmp], print_exports_have_address

    add [rb + symbol], 0, [rb + 1]
    add err_unknown_symbol, 0, [rb]
    call report_symbol_error

print_exports_have_address:
    # print the identifier
    add [rb + symbol], GLOBAL_IDENTIFIER, [rb - 1]
    arb -1
    call print_str

    out ':'

    # print the address
    add [rb + symbol], GLOBAL_ADDRESS, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_num

    out 10

print_exports_symbol_done:
    # move to next symbol
    add [rb + symbol], GLOBAL_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + symbol]

    jz  0, print_exports_symbol

print_exports_done:
    arb 3
    ret 0
.ENDFRAME

##########
# error messages

err_unknown_symbol:
    db  "Unknown symbol", 0
err_imported_symbol_defined:
    db  "Imported symbol must not have an address defined", 0

.EOF
