.EXPORT set_mem
.EXPORT set_mem_str
.EXPORT inc_mem_at

# TODO these should be private
.EXPORT mem_head
.EXPORT mem_tail
.EXPORT mem_index

# from libxib/heap.s
.IMPORT alloc

# from util.s
.IMPORT report_error

##########
set_mem:
.FRAME byte; buffer, tmp
    arb -2

    add [mem_tail], 0, [rb + buffer]

    # do we have a buffer at all?
    jnz [rb + buffer], set_mem_have_buffer

    # no, create one
    add MEM_BLOCK_SIZE, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + buffer]

    # reset next buffer pointer
    add [rb + buffer], 0, [ip + 3]
    add 0, 0, [0]

    add [rb + buffer], 0, [mem_head]
    add [rb + buffer], 0, [mem_tail]
    add 1, 0, [mem_index]

    jz  0, set_mem_have_space

set_mem_have_buffer:
    # is there enough space for one more byte?
    lt  [mem_index], MEM_BLOCK_SIZE, [rb + tmp]
    jnz [rb + tmp], set_mem_have_space

    # no, create a new buffer
    add MEM_BLOCK_SIZE, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + buffer]

    # reset next buffer pointer
    add [rb + buffer], 0, [ip + 3]
    add 0, 0, [0]

    # add it to the tail of buffer linked list
    add [mem_tail], 0, [ip + 3]
    add [rb + buffer], 0, [0]

    add [rb + buffer], 0, [mem_tail]
    add 1, 0, [mem_index]

set_mem_have_space:
    add [mem_tail], [mem_index], [ip + 3]
    add [rb + byte], 0, [0]

    add [mem_index], 1, [mem_index]

    arb 2
    ret 1
.ENDFRAME

##########
set_mem_str:
.FRAME string; index, char
    arb -2

    add 0, 0, [rb + index]

set_mem_str_loop:
    add [rb + string], [rb + index], [ip + 1]
    add [0], 0, [rb + char]

    jz  [rb + char], set_mem_str_done

    add [rb + char], 0, [rb - 1]
    arb -1
    call set_mem

    add [rb + index], 1, [rb + index]
    jz  0, set_mem_str_loop

set_mem_str_done:
    arb 2
    ret 1
.ENDFRAME

##########
inc_mem_at:
.FRAME byte, index, offset; buffer
    arb -1

    # increase memory location in index-th memory block, location offset
    # assume this does not require creating new blocks

    add [mem_head], 0, [rb + buffer]

inc_mem_at_loop:
    # any more blocks?
    jnz [rb + buffer], inc_mem_at_have_block

    add err_invalid_fixup, 0, [rb]
    call report_error

inc_mem_at_have_block:
    # is this the block we need?
    jz  [rb + index], inc_mem_at_this_block
    add [rb + index], -1, [rb + index]

    # next block in linked list
    add [rb + buffer], 0, [ip + 1]
    add [0], 0, [rb + buffer]

    jz  0, inc_mem_at_loop

inc_mem_at_this_block:
    # set the value
    add [rb + buffer], [rb + offset], [inc_mem_at_ptr_in]
    add [rb + buffer], [rb + offset], [inc_mem_at_ptr_out]
+1 = inc_mem_at_ptr_in:
+3 = inc_mem_at_ptr_out:
    add [0], [rb + byte], [0]

    arb 1
    ret 3
.ENDFRAME

##########
# globals

# output memory buffer
mem_head:
    db 0
mem_tail:
    db 0

# index of next unused byte in last memory buffer
mem_index:
    db 0

##########
# error messages

err_invalid_fixup:
    db  "Invalid fixup location", 0

.EOF
