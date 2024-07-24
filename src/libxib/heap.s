.EXPORT alloc_blocks
.EXPORT realloc_more_blocks
.EXPORT free
.EXPORT zeromem_blocks

.EXPORT neg_chunk_header_size
.EXPORT block_size

.EXPORT dump_heap

# from brk.s
.IMPORT sbrk
.IMPORT brk_addr

# from print.s
.IMPORT print_num
.IMPORT print_str

# from outside of this library
.IMPORT report_libxib_error

# from builtin
.IMPORT __heap_start

.SYMBOL CHUNK_BLOCKS                    0
.SYMBOL CHUNK_FREE                      1
.SYMBOL CHUNK_NEXT                      2
.SYMBOL CHUNK_PREV                      3

# used chunks just have the SIZE and FREE fields in their header
.SYMBOL USED_CHUNK_HEADER_SIZE          2
.SYMBOL FREE_CHUNK_HEADER_SIZE          4

.SYMBOL BLOCK_SIZE                      8
.SYMBOL MAX_SMALL_BLOCKS                8

# constants exported for users of this library 
# TODO use neg_chunk_header_size in this file instead of -2
neg_chunk_header_size:
    db  -2          # negative of USED_CHUNK_HEADER_SIZE, for subtracting
block_size:
    db  BLOCK_SIZE

# The amount of memory to allocate is not given in bytes, but in blocks of BLOCK_SIZE = 8 bytes.
# This is because we internally want to allocate amounts that are multiples of 8, but in intcode
# there is no efficient way to find the nearest multiple of eight for a given byte count.
#
# Since there is a USED_CHUNK_HEADER_SIZE = 2 byte overhead, the amount of bytes allocated is
# actually block_count * BLOCK_SIZE - USED_CHUNK_HEADER_SIZE = block_count * 8 - 2.

##########
alloc_blocks:
.FRAME block_count; ptr, bin, new_chunk, tmp                # returns ptr
    arb -4

    # we have small bins for block counts 0-MAX_SMALL_BLOCKS, bigger sizes are in the large bin
    lt  MAX_SMALL_BLOCKS, [rb + block_count], [rb + tmp]
    jnz [rb + tmp], alloc_large

    # start with the smallest bin that fits
    add alloc_small_table, [rb + block_count], [ip + 2]
    jz  0, [0]

alloc_small_table:
    db  alloc_small_0_blocks
    db  alloc_small_1_blocks
    db  alloc_small_2_blocks
    db  alloc_small_3_blocks
    db  alloc_small_4_blocks
    db  alloc_small_5_blocks
    db  alloc_small_6_blocks
    db  alloc_small_7_blocks
    db  alloc_small_8_blocks

alloc_small_0_blocks:
    # zero blocks, fail
    add err_zero_allocation, 0, [rb]
    call report_libxib_error

alloc_small_1_blocks:
    # if there is a chunk in current bin, use it
    add 1, 0, [rb + bin]
    jnz [small + 1], alloc_from_small
    # fall through

alloc_small_2_blocks:
    # if there is a chunk in current bin, use it
    add 2, 0, [rb + bin]
    jnz [small + 2], alloc_from_small
    # fall through

alloc_small_3_blocks:
    # if there is a chunk in current bin, use it
    add 3, 0, [rb + bin]
    jnz [small + 3], alloc_from_small
    # fall through

alloc_small_4_blocks:
    # if there is a chunk in current bin, use it
    add 4, 0, [rb + bin]
    jnz [small + 4], alloc_from_small
    # fall through

alloc_small_5_blocks:
    # if there is a chunk in current bin, use it
    add 5, 0, [rb + bin]
    jnz [small + 5], alloc_from_small
    # fall through

alloc_small_6_blocks:
    # if there is a chunk in current bin, use it
    add 6, 0, [rb + bin]
    jnz [small + 6], alloc_from_small
    # fall through

alloc_small_7_blocks:
    # if there is a chunk in current bin, use it
    add 7, 0, [rb + bin]
    jnz [small + 7], alloc_from_small
    # fall through

alloc_small_8_blocks:
    # if there is a chunk in current bin, use it
    add 8, 0, [rb + bin]
    jnz [small + 8], alloc_from_small
    # fall through

alloc_large:
    # try to find a fitting chunk in the large bin
    add [large], 0, [rb + ptr]

alloc_large_loop:
    # if we went through the whole large bin, allocate memory using sbrk
    # TODO try merging small and large chunks first
    jz  [rb + ptr], alloc_sbrk

    # is this chunk large enough?
    add [rb + ptr], CHUNK_BLOCKS, [ip + 1]
    lt  [0], [rb + block_count], [rb + tmp]
    jz  [rb + tmp], alloc_from_large

    # not large enough, try the next one
    add [rb + ptr], CHUNK_NEXT, [ip + 1]
    add [0], 0, [rb + ptr]

    jz  0, alloc_large_loop

alloc_sbrk:
    # last resort, call sbrk to enlarge the heap
    mul [rb + block_count], BLOCK_SIZE, [rb - 1]
    arb -1
    call sbrk
    add [rb - 3], 0, [rb + ptr]

    # set the SIZE field for the new chunk
    add [rb + ptr], CHUNK_BLOCKS, [ip + 3]
    add [rb + block_count], 0, [0]

    # return the new chunk
    jz  0, alloc_return_ptr

alloc_from_large:
    # use the chunk pointed to by ptr, from the large bin

    # if (ptr[CHUNK_NEXT])
    add [rb + ptr], CHUNK_NEXT, [ip + 1]
    jz  [0], alloc_from_large_after_next_prev

    # ptr[CHUNK_NEXT][CHUNK_PREV] = ptr[CHUNK_PREV]
    add [rb + ptr], CHUNK_NEXT, [ip + 1]
    add [0], CHUNK_PREV, [ip + 7]
    add [rb + ptr], CHUNK_PREV, [ip + 1]
    add [0], 0, [0]

alloc_from_large_after_next_prev:
    # if (ptr[CHUNK_PREV])
    add [rb + ptr], CHUNK_PREV, [ip + 1]
    jz  [0], alloc_from_large_replace_head

    # ptr[CHUNK_PREV][CHUNK_NEXT] = ptr[CHUNK_NEXT]
    add [rb + ptr], CHUNK_PREV, [ip + 1]
    add [0], CHUNK_NEXT, [ip + 7]
    add [rb + ptr], CHUNK_NEXT, [ip + 1]
    add [0], 0, [0]

    jz  0, alloc_cut_chunk

alloc_from_large_replace_head:
    # large = ptr[CHUNK_NEXT]
    add [rb + ptr], CHUNK_NEXT, [ip + 1]
    add [0], 0, [large]

    jz  0, alloc_cut_chunk

alloc_from_small:
    # use first chunk from the small bin we found
    add small, [rb + bin], [ip + 1]
    add [0], 0, [rb + ptr]

    # second chunk, if any, is now head of the bin
    # small[bin] = small[bin][CHUNK_NEXT] = ptr[CHUNK_NEXT]
    add [rb + ptr], CHUNK_NEXT, [ip + 5]
    add small, [rb + bin], [ip + 3]
    add [0], 0, [0]

    # does the bin still have a head?
    add small, [rb + bin], [ip + 1]
    jz  [0], alloc_cut_chunk

    # the bin has a head, set previous chunk of the new head to 0
    add small, [rb + bin], [ip + 1]
    add [0], CHUNK_PREV, [ip + 3]
    add 0, 0, [0]

alloc_cut_chunk:
    # is the chunk pointed to by [rb + ptr] larger than requested?
    # TODO perhaps only cut if it is much larger, e.g. don't cut small chunks?
    add [rb + ptr], CHUNK_BLOCKS, [ip + 1]
    eq  [0], [rb + block_count], [rb + tmp]
    jnz [rb + tmp], alloc_return_ptr

    # yes, split the chunk
    mul [rb + block_count], BLOCK_SIZE, [rb + tmp]
    add [rb + ptr], [rb + tmp], [rb + new_chunk]

    # new chunk blocks = ptr[CHUNK_BLOCKS] - block_count
    mul [rb + block_count], -1, [rb + tmp]
    add [rb + ptr], CHUNK_BLOCKS, [ip + 5]
    add [rb + new_chunk], CHUNK_BLOCKS, [ip + 3]
    add [0], [rb + tmp], [0]

    # resize the original chunk
    add [rb + ptr], CHUNK_BLOCKS, [ip + 3]
    add [rb + block_count], 0, [0]

    # return the new chunk to the heap (adjusting the pointer to point after the header, as free expects)
    add [rb + new_chunk], USED_CHUNK_HEADER_SIZE, [rb - 1]
    arb -1
    call free

    # fall through

alloc_return_ptr:
    # mark the chunk as not free
    add [rb + ptr], CHUNK_FREE, [ip + 3]
    add 0, 0, [0]

    # adjust the return value to point to the usable data buffer (after chunk header)
    add [rb + ptr], USED_CHUNK_HEADER_SIZE, [rb + ptr]

alloc_done:
    arb 4
    ret 1
.ENDFRAME

##########
realloc_more_blocks:
.FRAME old_ptr, new_block_count; new_ptr, old_size, new_size, old_chunk, tmp               # returns new_ptr
    arb -5

    # TODO support decreasing block count, for now only increasing is supported
    # TODO check if the chunk is already larger than requested; sometimes alloc returns a larger chunk than requested

    # adjust old_ptr to point to the chunk header by subtracting USED_CHUNK_HEADER_SIZE = 2
    add [rb + old_ptr], -2, [rb + old_chunk]

    # get current block size in bytes
    add [rb + old_chunk], CHUNK_BLOCKS, [ip + 1]
    mul [0], BLOCK_SIZE, [rb + old_size]

    # is this the last block before sbrk?
    add [rb + old_chunk], [rb + old_size], [rb + tmp]
    eq  [rb + tmp], [brk_addr], [rb + tmp]
    jnz [rb + tmp], realloc_blocks_sbrk

    # this is not the last block, allocate a new block and copy data
    # TODO first try merging this block with the following block, if it is free
    add [rb + new_block_count], 0, [rb - 1]
    arb -1
    call alloc_blocks
    add [rb - 3], 0, [rb + new_ptr]

    # old_size includes size of the chunk header, subtract it
    add [rb + old_size], [neg_chunk_header_size], [rb + old_size]

realloc_more_blocks_copy_loop:
    add [rb + old_size], -1, [rb + old_size]

    add [rb + old_ptr], [rb + old_size], [ip + 5]
    add [rb + new_ptr], [rb + old_size], [ip + 3]
    add [0], 0, [0]

    jnz [rb + old_size], realloc_more_blocks_copy_loop

    # free the original chunk
    add [rb + old_ptr], 0, [rb - 1]
    arb -1
    call free

    jz  0, realloc_more_blocks_done

realloc_blocks_sbrk:
    # allocate additional memory using sbrk
    mul [rb + new_block_count], BLOCK_SIZE, [rb + new_size]
    mul [rb + old_size], -1, [rb + tmp]
    add [rb + new_size], [rb + tmp], [rb - 1]
    arb -1
    call sbrk

    # update block size
    add [rb + old_chunk], CHUNK_BLOCKS, [ip + 3]
    add [rb + new_block_count], 0, [0]

    add [rb + old_ptr], 0, [rb + new_ptr]

realloc_more_blocks_done:
    arb 5
    ret 2
.ENDFRAME

##########
free:
.FRAME ptr; block_count, tmp
    arb -2

    # TODO when freeing at the end of heap (near brk), reduce brk instead of adding to bin

    # handle null pointers
    jz  [rb + ptr], free_done

    # adjust the pointer by USED_CHUNK_HEADER_SIZE = 2, so it points to the chunk header
    add [rb + ptr], -2, [rb + ptr]

    # mark the chunk as free
    add [rb + ptr], CHUNK_FREE, [ip + 3]
    add 1, 0, [0]

    # decide which bin to use for storing this chunk
    add [rb + ptr], CHUNK_BLOCKS, [ip + 1]
    add [0], 0, [rb + block_count]

    lt  MAX_SMALL_BLOCKS, [rb + block_count], [rb + tmp]
    jz  [rb + tmp], free_small

    # large chunk, add to the beginning of the large bin

    # if (large)
    jz  [large], free_large_after_prev

    # large[CHUNK_PREV] = ptr
    add [large], CHUNK_PREV, [ip + 3]
    add [rb + ptr], 0, [0]

free_large_after_prev:
    # ptr[CHUNK_NEXT] = large
    add [rb + ptr], CHUNK_NEXT, [ip + 3]
    add [large], 0, [0]

    # ptr[CHUNK_PREV] = 0
    add [rb + ptr], CHUNK_PREV, [ip + 3]
    add 0, 0, [0]

    # large = ptr
    add [rb + ptr], 0, [large]

    jz  0, free_done

free_small:
    # small chunk, return to small bin matching block_count

    # if (small[block_count])
    add small, [rb + block_count], [ip + 1]
    jz  [0], free_small_after_prev

    # small[block_count][CHUNK_PREV] = ptr
    add small, [rb + block_count], [ip + 1]
    add [0], CHUNK_PREV, [ip + 3]
    add [rb + ptr], 0, [0]

free_small_after_prev:
    # ptr[CHUNK_NEXT] = small[block_count]
    add small, [rb + block_count], [ip + 5]
    add [rb + ptr], CHUNK_NEXT, [ip + 3]
    add [0], 0, [0]

    # ptr[CHUNK_PREV] = 0
    add [rb + ptr], CHUNK_PREV, [ip + 3]
    add 0, 0, [0]

    # small[block_count] = ptr
    add small, [rb + block_count], [ip + 3]
    add [rb + ptr], 0, [0]

free_done:
    arb 2
    ret 1
.ENDFRAME

##########
zeromem_blocks:
.FRAME ptr, block_count; size, tmp
    arb -2

    # the block_count argument is in memory blocks, calculate size
    mul [rb + block_count], BLOCK_SIZE, [rb + size]
    # subtract chunk header size USED_CHUNK_HEADER_SIZE = 2
    add [rb + size], -2, [rb + size]

zeromem_loop:
    add [rb + size], -1, [rb + size]
    lt  [rb + size], 0, [rb + tmp]
    jnz [rb + tmp], zeromem_done

    add [rb + ptr], [rb + size], [ip + 3]
    add 0, 0, [0]

    jz  0, zeromem_loop

zeromem_done:
    arb 2
    ret 2
.ENDFRAME

##########
dump_heap:
.FRAME bin, chunk, tmp
    arb -3

    # dump brk
    add dump_heap_brk_msg, 0, [rb - 1]
    arb -1
    call print_str

    add 0, 0, [rb - 1]
    arb -1
    call sbrk

    add [rb - 3], 0, [rb - 1]
    arb -1
    call print_num

    out 10

    # dump all small bins, starting from 1 since 0 is a dummy bin
    add 1, 0, [rb + bin]

dump_heap_small_bin_loop:
    # there are MAX_SMALL_BLOCKS = 8 small bins, indexed from 1
    lt  MAX_SMALL_BLOCKS, [rb + bin], [rb + tmp]
    jnz [rb + tmp], dump_heap_large

    # get first chunk in this bin
    add small, [rb + bin], [ip + 1]
    add [0], 0, [rb + chunk]

    # if there are no chunks, don't dump this bin at all
    jz  [rb + chunk], dump_heap_small_next_bin

    # print bin header, then dump all chunks in the bin
    add dump_heap_small_bin_msg, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + bin], 0, [rb - 1]
    arb -1
    call print_num

    out ':'

dump_heap_small_chunk_loop:
    jz  [rb + chunk], dump_heap_small_chunk_done

    out ' '
    out '['
    out 'p'
    out ' '

    add [rb + chunk], 0, [rb - 1]
    arb -1
    call print_num

    out ','
    out ' '
    out 'b'
    out ' '

    add [rb + chunk], CHUNK_BLOCKS, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_num

    out ']'

    # next chunk
    add [rb + chunk], CHUNK_NEXT, [ip + 1]
    add [0], 0, [rb + chunk]

    jz  0, dump_heap_small_chunk_loop

dump_heap_small_chunk_done:
    # next bin
    out 10

dump_heap_small_next_bin:
    add [rb + bin], 1, [rb + bin]
    jz  0, dump_heap_small_bin_loop

dump_heap_large:
    # if there are no chunks, don't dump the large bin at all
    jz  [large], dump_heap_done

    add dump_heap_large_bin_msg, 0, [rb - 1]
    arb -1
    call print_str

    add [large], 0, [rb + chunk]

dump_heap_large_chunk_loop:
    jz  [rb + chunk], dump_heap_large_chunk_done

    out ' '
    out '['
    out 'p'
    out ' '

    add [rb + chunk], 0, [rb - 1]
    arb -1
    call print_num

    out ','
    out ' '
    out 'b'
    out ' '

    add [rb + chunk], CHUNK_BLOCKS, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_num

    out ']'

    # next chunk
    add [rb + chunk], CHUNK_NEXT, [ip + 1]
    add [0], 0, [rb + chunk]

    jz  0, dump_heap_large_chunk_loop

dump_heap_large_chunk_done:
    out 10

dump_heap_done:
    arb 3
    ret 0

dump_heap_brk_msg:
    db  "brk: ", 0
dump_heap_small_bin_msg:
    db  "small bin ", 0
dump_heap_large_bin_msg:
    db  "large bin:", 0
.ENDFRAME

##########
# globals

# small bins, for block counts 0-8, each is a doubly linked list
small:
    ds  9, 0

# large bin, for block counts >8, one doubly linked list
large:
    db  0

##########
# error messages

err_zero_allocation:
    db  "Zero block memory allocation is not allowed", 0

.EOF
