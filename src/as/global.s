.EXPORT find_global_symbol
.EXPORT add_global_symbol
.EXPORT set_global_symbol_address
.EXPORT set_global_symbol_type
.EXPORT global_head
.EXPORT init_relocations
.EXPORT relocation_symbol

# from libxib/heap.s
.IMPORT alloc_blocks

# from libxib/string.s
.IMPORT strcmp
.IMPORT strcpy

# from error.s
.IMPORT report_symbol_token_error

##########
init_relocations:
.FRAME symbol
    arb -1

    # add a dummy symbol to store relocations not related to a symbol
    # set symbol type to 4 (relocation)
    add relocation_symbol, 0, [rb - 1]
    add 4, 0, [rb - 2]
    arb -2
    call set_global_symbol_type

    arb 1
    ret 0
.ENDFRAME

##########
find_global_symbol:
.FRAME identifier; record
    arb -1

    add [global_head], 0, [rb + record]

find_global_symbol_loop:
    # are there any more records?
    jz  [rb + record], find_global_symbol_done

    # does this record contain the identifier?
    add [rb + identifier], 0, [rb - 1]
    add [rb + record], GLOBAL_IDENTIFIER, [rb - 2]
    arb -2
    call strcmp

    # if strcmp result is 0, we are done
    jz  [rb - 4], find_global_symbol_done

    # move to next record
    add [rb + record], GLOBAL_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + record]

    jz  0, find_global_symbol_loop

find_global_symbol_done:
    arb 1
    ret 1
.ENDFRAME

##########
add_global_symbol:
.FRAME identifier; record
    arb -1

    # allocate a block
    add GLOBAL_ALLOC_SIZE, 0, [rb - 1]
    arb -1
    call alloc_blocks
    add [rb - 3], 0, [rb + record]

    # set pointer to next symbol
    add [rb + record], GLOBAL_NEXT_PTR, [ip + 3]
    add [global_head], 0, [0]

    # store the identifier
    add [rb + identifier], 0, [rb - 1]
    add [rb + record], GLOBAL_IDENTIFIER, [rb - 2]
    arb -2
    call strcpy

    # set the symbol as local by default
    add [rb + record], GLOBAL_TYPE, [ip + 3]
    add 0, 0, [0]

    # set address to -1, so we can detect when the address is set
    add [rb + record], GLOBAL_ADDRESS, [ip + 3]
    add -1, 0, [0]

    # set fixup head to 0
    add [rb + record], GLOBAL_FIXUPS_HEAD, [ip + 3]
    add 0, 0, [0]

    # set new symbol head
    add [rb + record], 0, [global_head]

    arb 1
    ret 1
.ENDFRAME

##########
set_global_symbol_address:
.FRAME identifier, address; symbol, tmp
    arb -2

    # find or create the symbol record
    add [rb + identifier], 0, [rb - 1]
    arb -1
    call find_global_symbol
    add [rb - 3], 0, [rb + symbol]

    jnz [rb + symbol], set_global_symbol_address_check_duplicate

    add [rb + identifier], 0, [rb - 1]
    arb -1
    call add_global_symbol
    add [rb - 3], 0, [rb + symbol]

    jz  0, set_global_symbol_address_have_symbol

set_global_symbol_address_check_duplicate:
    # check for duplicate symbol definitions
    add [rb + symbol], GLOBAL_ADDRESS, [ip + 1]
    add [0], 0, [rb + tmp]

    eq  [rb + tmp], -1, [rb + tmp]
    jnz [rb + tmp], set_global_symbol_address_have_symbol

    add [rb + symbol], 0, [rb + 1]
    add err_duplicate_global_symbol, 0, [rb]
    call report_symbol_token_error

set_global_symbol_address_have_symbol:
    # store the address of the symbol
    add [rb + symbol], GLOBAL_ADDRESS, [ip + 3]
    add [rb + address], 0, [0]

    arb 2
    ret 2
.ENDFRAME

##########
set_global_symbol_type:
.FRAME identifier, type; symbol, tmp
    arb -2

    # find or create the symbol record
    add [rb + identifier], 0, [rb - 1]
    arb -1
    call find_global_symbol
    add [rb - 3], 0, [rb + symbol]

    jnz [rb + symbol], set_global_symbol_type_check

    add [rb + identifier], 0, [rb - 1]
    arb -1
    call add_global_symbol
    add [rb - 3], 0, [rb + symbol]

    jz  0, set_global_symbol_type_have_symbol

set_global_symbol_type_check:
    # check for symbol already imported/exported
    add [rb + symbol], GLOBAL_TYPE, [ip + 1]
    add [0], 0, [rb + tmp]

    jz  [rb + tmp], set_global_symbol_type_have_symbol

    eq  [rb + tmp], [rb + type], [rb + tmp]
    jnz [rb + tmp], set_global_symbol_type_check_same

    add [rb + symbol], 0, [rb + 1]
    add err_symbol_symbol_type_mix, 0, [rb]
    call report_symbol_token_error

set_global_symbol_type_check_same:
    eq  [rb + type], 1, [rb + tmp]
    jnz [rb + tmp], set_global_symbol_type_error_imported
    eq  [rb + type], 2, [rb + tmp]
    jnz [rb + tmp], set_global_symbol_type_error_exported

    add [rb + symbol], 0, [rb + 1]
    add err_constant_already_defined, 0, [rb]
    call report_symbol_token_error

set_global_symbol_type_error_imported:
    add [rb + symbol], 0, [rb + 1]
    add err_symbol_already_imported, 0, [rb]
    call report_symbol_token_error

set_global_symbol_type_error_exported:
    add [rb + symbol], 0, [rb + 1]
    add err_symbol_already_exported, 0, [rb]
    call report_symbol_token_error

set_global_symbol_type_have_symbol:
    # set symbol type
    add [rb + symbol], GLOBAL_TYPE, [ip + 3]
    add [rb + type], 0, [0]

    arb 2
    ret 2
.ENDFRAME

##########
# globals

# head of the linked list of global symbols
global_head:
    db  0

# dummy symbol identifier used to store non-symbol related relocations
relocation_symbol:
    db  "", 0

##########
# error messages

err_duplicate_global_symbol:
    db  "Duplicate global symbol definition", 0
err_symbol_symbol_type_mix:
    db  "Redefining symbol type is not allowed", 0
err_symbol_already_imported:
    db  "Symbol is already imported", 0
err_symbol_already_exported:
    db  "Symbol is already exported", 0
err_constant_already_defined:
    db  "Constant symbol is already defined", 0

.EOF
