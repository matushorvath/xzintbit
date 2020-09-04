# TODO
# - allocate less than MEM_BLOCK_SIZE

.EXPORT alloc
.EXPORT free

# from builtin
.IMPORT __heap_start

# from error.s
.IMPORT report_plain_error

# from print.s
.IMPORT print_str

##########
alloc:
.FRAME size; block, tmp
    arb -2

    # we only support certain block sizes
    lt  MEM_BLOCK_SIZE, [rb + size], [rb + tmp]
    jz  [rb + tmp], alloc_size_ok

    add err_allocation_size, 0, [rb]
    call report_plain_error

alloc_size_ok:
    # do we have any free blocks?
    jz  [free_head], alloc_create_block

    # yes, remove first block from the list and return it
    add [free_head], 0, [rb + block]

    add [free_head], 0, [ip + 1]
    add [0], 0, [free_head]

    jz  0, alloc_done

alloc_create_block:
    # there are no free blocks, create one
    add [heap_end], 0, [rb + block]
    add [heap_end], MEM_BLOCK_SIZE, [heap_end]

alloc_done:
    arb 2
    ret 1
.ENDFRAME

##########
free:
.FRAME block; tmp
    arb -1

    # set pointer to next free block in the block we are returning
    add [rb + block], 0, [ip + 3]
    add [free_head], 0, [0]

    # set new free block head
    add [rb + block], 0, [free_head]

    arb 1
    ret 1
.ENDFRAME

##########
# globals

# head of the linked list of free blocks
free_head:
    db  0

# start of unused memory
heap_end:
    db  __heap_start

##########
# error messages

err_allocation_size:
    db  "Unsupported allocation size", 0

.EOF
