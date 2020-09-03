# TODO merge with ld/memory.s

.EXPORT set_mem
.EXPORT set_mem_str
.EXPORT inc_mem_at
.EXPORT calc_fixup
.EXPORT print_mem

# from libxib/heap.s
.IMPORT alloc

# from libxib/print.s
.IMPORT print_num

# from util.s
.IMPORT report_error

##########
set_mem:
.FRAME byte; buffer, tmp
    arb -2

    add mem_head, 0, [rb - 1]
    add mem_tail, 0, [rb - 2]
    add mem_index, 0, [rb - 3]
    add [rb + byte], 0, [rb - 4]
    arb -4
    call set_mem_internal

    arb 2
    ret 1
.ENDFRAME

##########
set_mem_internal:
.FRAME head_ptr, tail_ptr, tail_index_ptr, byte; buffer, tmp
    arb -2

    add [rb + tail_ptr], 0, [ip + 1]
    add [0], 0, [rb + buffer]

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

    add [rb + head_ptr], 0, [ip + 3]
    add [rb + buffer], 0, [0]

    add [rb + tail_ptr], 0, [ip + 3]
    add [rb + buffer], 0, [0]

    add [rb + tail_index_ptr], 0, [ip + 3]
    add 1, 0, [0]

    jz  0, set_mem_have_space

set_mem_have_buffer:
    # is there enough space for one more byte?
    add [rb + tail_index_ptr], 0, [ip + 1]
    lt  [0], MEM_BLOCK_SIZE, [rb + tmp]
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
    add [rb + tail_ptr], 0, [ip + 1]
    add [0], 0, [ip + 3]
    add [rb + buffer], 0, [0]

    add [rb + tail_ptr], 0, [ip + 3]
    add [rb + buffer], 0, [0]

    add [rb + tail_index_ptr], 0, [ip + 3]
    add 1, 0, [0]

set_mem_have_space:
    add [rb + tail_index_ptr], 0, [ip + 1]
    add [0], 0, [rb + tmp]

    add [rb + tail_ptr], 0, [ip + 1]
    add [0], [rb + tmp], [ip + 3]
    add [rb + byte], 0, [0]

    add [rb + tail_index_ptr], 0, [ip + 1]
    add [0], 0, [rb + tmp]
    add [rb + tail_index_ptr], 0, [ip + 3]
    add [rb + tmp], 1, [0]

    arb 2
    ret 4
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
calc_fixup:
.FRAME address; index, offset, tmp
    arb -3

    # calculate index = address / (MEM_BLOCK_SIZE - 1) ; offset = address % (MEM_BLOCK_SIZE - 1) + 1
    add 0, 0, [rb + index]
    add [rb + address], 0, [rb + offset]

calc_fixup_loop:
    lt  [rb + offset], MEM_BLOCK_SIZE - 1, [rb + tmp]
    jnz [rb + tmp], calc_fixup_done

    mul MEM_BLOCK_SIZE - 1, -1, [rb + tmp]
    add [rb + offset], [rb + tmp], [rb + offset]
    add [rb + index], 1, [rb + index]

    jz  0, calc_fixup_loop

calc_fixup_done:
    # data in memory blocks starts at offset 1
    add [rb + offset], 1, [rb + offset]

    arb 3
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
print_mem:
.FRAME tmp, buffer, limit, index, first
    arb -5

    # print .C
    out '.'
    out 'C'
    out 10

    add 1, 0, [rb + first]

    add [mem_head], 0, [rb + buffer]
    jz  [mem_head], print_mem_done

print_mem_block:
    add 1, 0, [rb + index]

    # maximum index within a block is MEM_BLOCK_SIZE, except for last block
    add MEM_BLOCK_SIZE, 0, [rb + limit]
    eq  [rb + buffer], [mem_tail], [rb + tmp]
    jz  [rb + tmp], print_mem_byte
    add [mem_index], 0, [rb + limit]

print_mem_byte:
    lt  [rb + index], [rb + limit], [rb + tmp]
    jz  [rb + tmp], print_mem_block_done

    # skip comma when printing first byte
    jnz [rb + first], print_mem_skip_comma
    out ','

print_mem_skip_comma:
    add 0, 0, [rb + first]

    add [rb + buffer], [rb + index], [ip + 1]
    add [0], 0, [rb + tmp]

    add [rb + tmp], 0, [rb - 1]
    arb -1
    call print_num

    add [rb + index], 1, [rb + index]
    jz  0, print_mem_byte

print_mem_block_done:
    # next block in linked list
    add [rb + buffer], 0, [ip + 1]
    add [0], 0, [rb + buffer]

    jnz [rb + buffer], print_mem_block

print_mem_done:
    out 10

    arb 5
    ret 0
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
