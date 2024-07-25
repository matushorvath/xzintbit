.EXPORT find_frame_symbol
.EXPORT add_frame_symbol
.EXPORT reset_frame
.EXPORT frame_head

# from libxib/heap.s
.IMPORT alloc_blocks
.IMPORT free

# from libxib/string.s
.IMPORT strcmp

##########
find_frame_symbol:
.FRAME identifier; record
    arb -1

    add [frame_head], 0, [rb + record]

.loop:
    # are there any more records?
    jz  [rb + record], .done

    # does this record contain the identifier?
    add [rb + identifier], 0, [rb - 1]
    add [rb + record], FRAME_IDENTIFIER_PTR, [ip + 1]
    add [0], 0, [rb - 2]
    arb -2
    call strcmp

    # if strcmp result is 0, we are done
    jz  [rb - 4], .done

    # move to next record
    add [rb + record], FRAME_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + record]

    jz  0, .loop

.done:
    arb 1
    ret 1
.ENDFRAME

##########
add_frame_symbol:
.FRAME identifier, block_index; record
    arb -1

    # takes over ownership of identifier

    # allocate a block
    add FRAME_ALLOC_SIZE, 0, [rb - 1]
    arb -1
    call alloc_blocks
    add [rb - 3], 0, [rb + record]

    # set pointer to next symbol
    add [rb + record], FRAME_NEXT_PTR, [ip + 3]
    add [frame_head], 0, [0]

    # store the identifier pointer
    add [rb + record], FRAME_IDENTIFIER_PTR, [ip + 3]
    add [rb + identifier], 0, [0]

    # set block index
    add [rb + record], FRAME_BLOCK, [ip + 3]
    add [rb + block_index], 0, [0]

    # set new symbol head
    add [rb + record], 0, [frame_head]

    arb 1
    ret 2
.ENDFRAME

##########
reset_frame:
.FRAME tmp, record
    arb -2

    add [frame_head], 0, [rb + record]

.loop:
    # are there any more records?
    jz  [rb + record], .done

    # save current record pointer
    add [rb + record], 0, [rb + tmp]

    # move to next record
    add [rb + record], FRAME_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + record]

    # free the identifier
    add [rb + tmp], FRAME_IDENTIFIER_PTR, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call free

    # free current record
    add [rb + tmp], 0, [rb - 1]
    arb -1
    call free

    jz  0, .loop

.done:
    # reset list head
    add 0, 0, [frame_head]

    arb 2
    ret 0
.ENDFRAME

##########
# globals

# head of the linked list of frame symbols
frame_head:
    db  0

.EOF
