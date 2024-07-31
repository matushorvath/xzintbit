.EXPORT add_or_find_current_child_symbol
.EXPORT add_or_find_child_symbol
.EXPORT last_global_symbol

# from libxib/heap.s
.IMPORT alloc_blocks
.IMPORT free

# from libxib/string.s
.IMPORT strcmp

# from error.s
.IMPORT report_error

##########
find_child_symbol:
.FRAME parent, identifier; child
    arb -1

    add [rb + parent], GLOBAL_CHILDREN_HEAD, [ip + 1]
    add [0], 0, [rb + child]

.loop:
    # are there any more children?
    jz  [rb + child], .done

    # does this child contain the identifier?
    add [rb + identifier], 0, [rb - 1]
    add [rb + child], GLOBAL_IDENTIFIER_PTR, [ip + 1]
    add [0], 0, [rb - 2]
    arb -2
    call strcmp

    # if strcmp result is 0, we are done
    jz  [rb - 4], .done

    # move to next child
    add [rb + child], GLOBAL_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + child]

    jz  0, .loop

.done:
    arb 1
    ret 2
.ENDFRAME

##########
add_child_symbol:
.FRAME parent, identifier; child
    arb -1

    # takes over ownership of identifier

    # allocate a block
    add GLOBAL_ALLOC_SIZE, 0, [rb - 1]
    arb -1
    call alloc_blocks
    add [rb - 3], 0, [rb + child]

    # set pointer to next symbol
    add [rb + parent], GLOBAL_CHILDREN_HEAD, [ip + 5]
    add [rb + child], GLOBAL_NEXT_PTR, [ip + 3]
    add [0], 0, [0]

    # store the identifier pointer
    add [rb + child], GLOBAL_IDENTIFIER_PTR, [ip + 3]
    add [rb + identifier], 0, [0]

    # set the symbol type to "child"
    add [rb + child], GLOBAL_TYPE, [ip + 3]
    add 4, 0, [0]

    # set address to -1, so we can detect when the address is set
    add [rb + child], GLOBAL_ADDRESS, [ip + 3]
    add -1, 0, [0]

    # save parent of this child
    add [rb + child], GLOBAL_PARENT, [ip + 3]
    add [rb + parent], 0, [0]

    # set list of children to 0
    add [rb + child], GLOBAL_CHILDREN_HEAD, [ip + 3]
    add 0, 0, [0]

    # set fixup head to 0
    add [rb + child], GLOBAL_FIXUPS_HEAD, [ip + 3]
    add 0, 0, [0]

    # set new children head in the parent
    add [rb + parent], GLOBAL_CHILDREN_HEAD, [ip + 3]
    add [rb + child], 0, [0]

    arb 1
    ret 2
.ENDFRAME

##########
add_or_find_current_child_symbol:
.FRAME identifier; child
    arb -1

    # was there already a global symbol we can use as a parent?
    jnz [last_global_symbol], .have_parent

    add err_no_parent_symbol, 0, [rb]
    call report_error

.have_parent:
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
.FRAME parent, identifier; child
    arb -1

    # takes over ownership of identifier

    # find the child record
    add [rb + parent], 0, [rb - 1]
    add [rb + identifier], 0, [rb - 2]
    arb -2
    call find_child_symbol
    add [rb - 4], 0, [rb + child]

    # did we find the record?
    jnz [rb + child], .found

    # no, add a new record
    add [rb + parent], 0, [rb - 1]
    add [rb + identifier], 0, [rb - 2]
    arb -2
    call add_child_symbol
    add [rb - 4], 0, [rb + child]

    jz  0, .done

.found:
    # free the identifier, the child already exists so we don't need it
    add [rb + identifier], 0, [rb - 1]
    arb -1
    call free

.done:
    arb 1
    ret 2
.ENDFRAME

##########
# globals

# head of the linked list of global symbols
last_global_symbol:
    db  0

##########
# errors

err_no_parent_symbol:
    db  "Child symbol without a preceding parent symbol", 0

.EOF
