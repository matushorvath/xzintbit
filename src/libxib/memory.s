.EXPORT set_mem
.EXPORT inc_mem
.EXPORT print_mem

# from heap.s
.IMPORT alloc

# from print.s
.IMPORT print_num

# from error.s
.IMPORT report_plain_error

##########
set_mem:
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
inc_mem:
.FRAME head, tail, tail_index, address, increment; index, offset, tmp
    arb -3

    # find out which memory block should be updated
    add [rb + address], 0, [rb - 1]
    arb -1
    call calc_mem

    add [rb - 3], 0, [rb + index]
    add [rb - 4], 0, [rb + offset]

    # update the memory
    add [rb + head], 0, [rb - 1]
    add [rb + tail], 0, [rb - 2]
    add [rb + tail_index], 0, [rb - 3]
    add [rb + index], 0, [rb - 4]
    add [rb + offset], 0, [rb - 5]
    add [rb + increment], 0, [rb - 6]
    arb -6
    call inc_mem_internal

    arb 3
    ret 5
.ENDFRAME

##########
calc_mem:
.FRAME address; index, offset, tmp
    arb -3

    # calculate index = address / (MEM_BLOCK_SIZE - 1) ; offset = address % (MEM_BLOCK_SIZE - 1) + 1
    add 0, 0, [rb + index]
    add [rb + address], 0, [rb + offset]

calc_mem_loop:
    lt  [rb + offset], MEM_BLOCK_SIZE - 1, [rb + tmp]
    jnz [rb + tmp], calc_mem_done

    mul MEM_BLOCK_SIZE - 1, -1, [rb + tmp]
    add [rb + offset], [rb + tmp], [rb + offset]
    add [rb + index], 1, [rb + index]

    jz  0, calc_mem_loop

calc_mem_done:
    # data in memory blocks starts at offset 1
    add [rb + offset], 1, [rb + offset]

    arb 3
    ret 1
.ENDFRAME

##########
inc_mem_internal:
.FRAME head, tail, tail_index, index, offset, increment; buffer, tmp
    arb -2

    # increase memory location in index-th memory block, location offset
    # assume this does not require creating new blocks
    # TODO validate against tail and tail_index

    add [rb + head], 0, [rb + buffer]

inc_mem_internal_loop:
    # any more blocks?
    jnz [rb + buffer], inc_mem_internal_have_block

    add err_invalid_address, 0, [rb]
    call report_plain_error

inc_mem_internal_have_block:
    # is this the block we need?
    jz  [rb + index], inc_mem_internal_this_block
    add [rb + index], -1, [rb + index]

    # next block in linked list
    add [rb + buffer], 0, [ip + 1]
    add [0], 0, [rb + buffer]

    jz  0, inc_mem_internal_loop

inc_mem_internal_this_block:
    # set the value
    add [rb + buffer], [rb + offset], [ip + 1]
    add [0], 0, [rb + tmp]
    add [rb + buffer], [rb + offset], [ip + 3]
    add [rb + tmp], [rb + increment], [0]

    arb 2
    ret 6
.ENDFRAME

##########
print_mem:
.FRAME head, tail, tail_index; tmp, buffer, limit, index, first
    arb -5

    add 1, 0, [rb + first]

    add [rb + head], 0, [rb + buffer]
    jz  [rb + head], print_mem_done

print_mem_block:
    add 1, 0, [rb + index]

    # maximum index within a block is MEM_BLOCK_SIZE, except for last block
    add MEM_BLOCK_SIZE, 0, [rb + limit]
    eq  [rb + buffer], [rb + tail], [rb + tmp]
    jz  [rb + tmp], print_mem_byte
    add [rb + tail_index], 0, [rb + limit]

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
    arb 5
    ret 3
.ENDFRAME

##########
# globals

# allocation block size
.SYMBOL MEM_BLOCK_SIZE 50

##########
# error messages

err_invalid_address:
    db  "Invalid memory address", 0

.EOF
