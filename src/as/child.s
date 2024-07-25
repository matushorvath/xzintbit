.EXPORT add_or_find_current_child_symbol
.EXPORT add_or_find_child_symbol
.EXPORT set_child_symbol_address

# from libxib/heap.s
.IMPORT alloc_blocks
.IMPORT free

# from libxib/string.s
.IMPORT strcmp

# from error.s
.IMPORT report_symbol_token_error

# TODO

##########
#find_child_symbol:
#.FRAME identifier; record, record_identifier, tmp
#    arb -3
#
#    add [global_head], 0, [rb + record]
#
#find_global_symbol_loop:
#    # are there any more records?
#    jz  [rb + record], find_global_symbol_done
#
#    # read identifier pointer from the record
#    add [rb + record], GLOBAL_IDENTIFIER_PTR, [ip + 1]
#    add [0], 0, [rb + record_identifier]
#
#    # treat two null identifiers as equal, see init_relocations for such identifier
#    add [rb + identifier], [rb + record_identifier], [rb + tmp]
#    jz  [rb + tmp], find_global_symbol_done
#
#    # skip strcmp if just one of the identifiers is null
#    jz  [rb + identifier], find_global_symbol_after_strcmp
#    jz  [rb + record_identifier], find_global_symbol_after_strcmp
#
#    # does this record contain the identifier?
#    add [rb + identifier], 0, [rb - 1]
#    add [rb + record_identifier], 0, [rb - 2]
#    arb -2
#    call strcmp
#
#    # if strcmp result is 0, we are done
#    jz  [rb - 4], find_global_symbol_done
#
#find_global_symbol_after_strcmp:
#    # move to next record
#    add [rb + record], GLOBAL_NEXT_PTR, [ip + 1]
#    add [0], 0, [rb + record]
#
#    jz  0, find_global_symbol_loop
#
#find_global_symbol_done:
#    arb 3
#    ret 1
#.ENDFRAME

##########
#add_child_symbol:
#.FRAME identifier; record
#    arb -1
#
#    # takes over ownership of identifier
#
#    # allocate a block
#    add GLOBAL_ALLOC_SIZE, 0, [rb - 1]
#    arb -1
#    call alloc_blocks
#    add [rb - 3], 0, [rb + record]
#
#    # set pointer to next symbol
#    add [rb + record], GLOBAL_NEXT_PTR, [ip + 3]
#    add [global_head], 0, [0]
#
#    # store the identifier pointer
#    add [rb + record], GLOBAL_IDENTIFIER_PTR, [ip + 3]
#    add [rb + identifier], 0, [0]
#
#    # set the symbol as local by default
#    add [rb + record], GLOBAL_TYPE, [ip + 3]
#    add 0, 0, [0]
#
#    # set address to -1, so we can detect when the address is set
#    add [rb + record], GLOBAL_ADDRESS, [ip + 3]
#    add -1, 0, [0]
#
#    # set fixup head to 0
#    add [rb + record], GLOBAL_FIXUPS_HEAD, [ip + 3]
#    add 0, 0, [0]
#
#    # set new symbol head
#    add [rb + record], 0, [global_head]
#
#    arb 1
#    ret 1
#.ENDFRAME

##########
add_or_find_current_child_symbol:
.FRAME identifier; child
    arb -1

    add [last_global_symbol], 0, [rb - 1]
    add [rb + identifier], 0, [rb - 2]
    arb -2
    call add_or_find_child_symbol
    add [rb - 4], 0, [rb + child]

    arb 1
    ret 1
.ENDFRAME

##########
add_or_find_child_symbol:
#.FRAME global, identifier; child
hlt # TODO
#    arb -1
#
#    # takes over ownership of identifier
#
#    # find the symbol record
#    add [rb + identifier], 0, [rb - 1]
#    arb -1
#    call find_global_symbol
#    add [rb - 3], 0, [rb + symbol]
#
#    # did we find the record?
#    jnz [rb + symbol], add_or_find_global_symbol_found
#
#    # no, add a new record
#    add [rb + identifier], 0, [rb - 1]
#    arb -1
#    call add_global_symbol
#    add [rb - 3], 0, [rb + symbol]
#
#    jz  0, add_or_find_global_symbol_done
#
#add_or_find_global_symbol_found:
#    # free the identifier, the symbol already exists so we don't need it
#    add [rb + identifier], 0, [rb - 1]
#    arb -1
#    call free
#
#add_or_find_global_symbol_done:
#    arb 1
#    ret 2
#.ENDFRAME

##########
set_child_symbol_address:
#.FRAME symbol, address; tmp
hlt # TODO
#    arb -1
#
#    # check for duplicate symbol definitions
#    add [rb + symbol], GLOBAL_ADDRESS, [ip + 1]
#    add [0], 0, [rb + tmp]
#
#    # does the symbol already have an address?
#    eq  [rb + tmp], -1, [rb + tmp]
#    jnz [rb + tmp], set_global_symbol_address_store
#
#    add [rb + symbol], 0, [rb + 1]
#    add err_duplicate_global_symbol, 0, [rb]
#    call report_symbol_token_error
#
#set_global_symbol_address_store:
#    # store the address of the symbol
#    add [rb + symbol], GLOBAL_ADDRESS, [ip + 3]
#    add [rb + address], 0, [0]
#
#    arb 1
#    ret 2
#.ENDFRAME

##########
# globals

# head of the linked list of global symbols
last_global_symbol:
    db  0

# TODO remember last global symbol that has an address set, from parse_symbol only I think
# TODO handle situation when there is no last global symbol yet (i.e. dot-identifier before first identifier)

##########
# error messages

#err_duplicate_global_symbol:
#    db  "Duplicate global symbol definition", 0
#err_symbol_symbol_type_mix:
#    db  "Redefining symbol type is not allowed", 0
#err_symbol_already_imported:
#    db  "Symbol is already imported", 0
#err_symbol_already_exported:
#    db  "Symbol is already exported", 0
#err_constant_already_defined:
#    db  "Constant symbol is already defined", 0

.EOF
