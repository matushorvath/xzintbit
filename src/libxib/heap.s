.EXPORT alloc
.EXPORT free
.EXPORT dump_heap

# from brk.s
.IMPORT sbrk

# from print.s
.IMPORT print_num
.IMPORT print_str

# from builtin
.IMPORT __heap_start

.SYMBOL CHUNK_SIZE                      0
.SYMBOL CHUNK_FREE                      1
.SYMBOL CHUNK_NEXT                      2
.SYMBOL CHUNK_PREV                      3

# used chunks just have the SIZE and FREE fields in their header
.SYMBOL USED_CHUNK_HEADER_SIZE          2
.SYMBOL FREE_CHUNK_HEADER_SIZE          4

.SYMBOL MIN_CHUNK_SIZE                  8
.SYMBOL MAX_SMALL_CHUNK_SIZE            64

##########
alloc:
.FRAME size; ptr, bin, new_chunk, tmp   # returns ptr
    arb -4

    # parameter validation
    lt  [rb + size], 1, [rb + tmp]
    jz  [rb + tmp], alloc_size_ok

    add 0, 0, [rb + ptr]
    jz  0, alloc_done

alloc_size_ok:
    # the chunk will use two additional bytes for the chunk header
    add [rb + size], USED_CHUNK_HEADER_SIZE, [rb + size]

    # make the size at least eight bytes
    lt  [rb + size], MIN_CHUNK_SIZE, [rb + tmp]
    jz  [rb + tmp], alloc_size_adjusted
    add 8, 0, [rb + size]

alloc_size_adjusted:
    # the smallest bin we have is for MIN_CHUNK_SIZE = 8 byte chunks
    add [rb + size], -8, [rb + bin]

alloc_small_loop:
    # there are MAX_SMALL_CHUNK_SIZE - MIN_CHUNK_SIZE = 64 - 8 = 56 small bins
    # once we go through them, try the large bin
    lt  [rb + bin], 56, [rb + tmp]
    jz  [rb + tmp], alloc_large

    # if there is a chunk in current bin, use it
    add small, [rb + bin], [ip + 1]
    jnz [0], alloc_small_have_chunk

    # otherwise, try the next bin
    add [rb + bin], 1, [rb + bin]
    jz  0, alloc_small_loop

alloc_small_have_chunk:
    # use the first chunk from bin bin
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

    jz  0, alloc_cut_chunk

alloc_large:
    # first try to find a fitting chunk in the large bin
    add [large], 0, [rb + ptr]

alloc_large_loop:
    # if we went through the whole large bin, try merging the small chunks next
    jz  [rb + ptr], alloc_merge_small

    # is this chunk large enough?
    add [rb + ptr], CHUNK_SIZE, [ip + 1]
    lt  [0], [rb + size], [rb + tmp]
    jz  [rb + tmp], alloc_large_have_chunk

    # not large enough, try the next one
    add [rb + ptr], CHUNK_NEXT, [ip + 1]
    add [0], 0, [rb + ptr]

    jz  0, alloc_large_loop

alloc_large_have_chunk:
    # remove the ptr chunk from the large bin

    # if (ptr[CHUNK_NEXT])
    add [rb + ptr], CHUNK_NEXT, [ip + 1]
    jz  [0], alloc_large_after_next_prev

    # ptr[CHUNK_NEXT][CHUNK_PREV] = ptr[CHUNK_PREV]
    add [rb + ptr], CHUNK_NEXT, [ip + 1]
    add [0], CHUNK_PREV, [ip + 7]
    add [rb + ptr], CHUNK_PREV, [ip + 1]
    add [0], 0, [0]

alloc_large_after_next_prev:
    # if (ptr[CHUNK_PREV])
    add [rb + ptr], CHUNK_PREV, [ip + 1]
    jz  [0], alloc_large_replace_head

    # ptr[CHUNK_PREV][CHUNK_NEXT] = ptr[CHUNK_NEXT]
    add [rb + ptr], CHUNK_PREV, [ip + 1]
    add [0], CHUNK_NEXT, [ip + 7]
    add [rb + ptr], CHUNK_NEXT, [ip + 1]
    add [0], 0, [0]

    jz  0, alloc_cut_chunk

alloc_large_replace_head:
    # large = ptr[CHUNK_NEXT]
    add [rb + ptr], CHUNK_NEXT, [ip + 1]
    add [0], 0, [large]

    jz  0, alloc_cut_chunk

alloc_merge_small:
    # try to merge the small bins until we build a chunk that fits
    add [rb + size], 0, [rb - 1]
    arb -1
    call merge_small_bins

    add [rb - 3], 0, [rb + ptr]
    jnz [rb + ptr], alloc_cut_chunk

alloc_merge_large:
    # try to merge the large bins until we build a chunk that fits
    add [rb + size], 0, [rb - 1]
    arb -1
    call merge_large_bins

    add [rb - 3], 0, [rb + ptr]
    jnz [rb + ptr], alloc_cut_chunk

alloc_sbrk:
    # last resort, call sbrk to enlarge the heap
    add [rb + size], 0, [rb - 1]
    arb -1
    call sbrk
    add [rb - 3], 0, [rb + ptr]

    # set the SIZE field for the new chunk
    add [rb + ptr], CHUNK_SIZE, [ip + 3]
    add [rb + size], 0, [0]

    # return the new chunk
    jz  0, alloc_return_ptr

alloc_cut_chunk:
    # minimum chunk size we will split is size + MIN_CHUNK_SIZE bytes
    add [rb + size], MIN_CHUNK_SIZE, [rb + tmp]

    # is the chunk pointed to by [rb + ptr] large enough to split?
    add [rb + ptr], CHUNK_SIZE, [ip + 1]
    lt  [0], [rb + tmp], [rb + tmp]
    jnz [rb + tmp], alloc_return_ptr

    # yes, split the chunk
    add [rb + ptr], [rb + size], [rb + new_chunk]

    # new chunk size = ptr[CHUNK_SIZE] - size
    mul [rb + size], -1, [rb + tmp]
    add [rb + ptr], CHUNK_SIZE, [ip + 5]
    add [rb + new_chunk], CHUNK_SIZE, [ip + 3]
    add [0], [rb + tmp], [0]

    # resize he original chunk
    add [rb + ptr], CHUNK_SIZE, [ip + 3]
    add [rb + size], 0, [0]

    # mark the new chunk as used and then free it, so it will return to one of the bins
    add [rb + new_chunk], CHUNK_FREE, [ip + 3]
    add 0, 0, [0]

    # free expects its argument to point after the chunk header, so add USED_CHUNK_HEADER_SIZE
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
merge_small_bins:
.FRAME size; tmp
    arb -1

    # TODO implement
    add 0, 0, [rb + tmp]

    arb 1
    ret 1
.ENDFRAME

##########
merge_large_bins:
.FRAME size; tmp
    arb -1

    # TODO implement
    add 0, 0, [rb + tmp]

    arb 1
    ret 1
.ENDFRAME

##########
free:
.FRAME ptr; size, bin, tmp
    arb -3

    # handle null pointers
    jz  [rb + ptr], free_done

    # adjust the pointer by USED_CHUNK_HEADER_SIZE = 2, so it points to the chunk header
    add [rb + ptr], -2, [rb + ptr]

    # mark the chunk as free
    add [rb + ptr], CHUNK_FREE, [ip + 3]
    add 1, 0, [0]

    # decide which bin to use for storing this chunk
    add [rb + ptr], CHUNK_SIZE, [ip + 1]
    add [0], 0, [rb + size]

    lt  MAX_SMALL_CHUNK_SIZE, [rb + size], [rb + tmp]
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
    # small chunk, calculate bin index
    add [rb + size], -8, [rb + bin]

    # if (small[bin])
    add small, [rb + bin], [ip + 1]
    jz  [0], free_small_after_prev

    # small[bin][CHUNK_PREV] = ptr
    add small, [rb + bin], [ip + 1]
    add [0], CHUNK_PREV, [ip + 3]
    add [rb + ptr], 0, [0]

free_small_after_prev:
    # ptr[CHUNK_NEXT] = small[bin]
    add small, [rb + bin], [ip + 5]
    add [rb + ptr], CHUNK_NEXT, [ip + 3]
    add [0], 0, [0]

    # ptr[CHUNK_PREV] = 0
    add [rb + ptr], CHUNK_PREV, [ip + 3]
    add 0, 0, [0]

    # small[bin] = ptr
    add small, [rb + bin], [ip + 3]
    add [rb + ptr], 0, [0]

free_done:
    arb 3
    ret 1
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

    # dump all small bins
    add 0, 0, [rb + bin]

dump_heap_small_bin_loop:
    # there are MAX_SMALL_CHUNK_SIZE - MIN_CHUNK_SIZE = 64 - 8 = 56 small bins
    lt  [rb + bin], 56, [rb + tmp]
    jz  [rb + tmp], dump_heap_large

    # get first chunk in this bin
    add small, [rb + bin], [ip + 1]
    add [0], 0, [rb + chunk]

    # if there are no chunks, don't dump this bin at all
    jz  [rb + chunk], dump_heap_small_next_bin

    # print bin header, then dump all chunks in the bin
    add dump_heap_small_bin_msg, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + bin], MIN_CHUNK_SIZE, [rb - 1]
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
    out 's'
    out ' '

    add [rb + chunk], CHUNK_SIZE, [ip + 1]
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
    out 's'
    out ' '

    add [rb + chunk], CHUNK_SIZE, [ip + 1]
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

# small bins, for sizes 8 to 64, each is a doubly linked list
small:
    ds  56, 0

# large bin, for sizes >64, one doubly linked list
large:
    db  0

.EOF
