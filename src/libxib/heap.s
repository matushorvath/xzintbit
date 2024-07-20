.EXPORT alloc
.EXPORT free

# from brk.s
.IMPORT sbrk

# from builtin
.IMPORT __heap_start

# TODO logging for alloc/free

##########
alloc:
.FRAME size; ptr, tmp                   # returns ptr
    arb -X

    # parameter validation
    lt  [rb + size], 1, [rb + tmp]
    jz  [rb + tmp], alloc_size_ok

    add 0, 0, [rb + ptr]
    jz  0, alloc_done

alloc_size_ok:
    # the chunk will use 2 additional bytes for the chunk header
    add [rb + size], 2, [rb + size]

    # select the large allocator for requests >64 bytes
    lt  64, [rb + size], [rb + tmp]
    jnz [rb + tmp], alloc_large_calc_size

    # allocate a small chunk, size rounded up to nearest multiple of 8
    add multiple_8, [rb + size], [ip + 1]
    add [0], 0, [rb + size]

    # start from the smallest bin that fits
    add alloc_small_table, [rb + size], [ip + 2]
    jz  0, [0]

alloc_small_table:
    db  0
    ds  8, alloc_small_8
    ds  8, alloc_small_16
    ds  8, alloc_small_24
    ds  8, alloc_small_32
    ds  8, alloc_small_40
    ds  8, alloc_small_48
    ds  8, alloc_small_56
    ds  8, alloc_small_64

alloc_small_8:
    jz  [small_next + 0], alloc_small_16

    # use the first chunk from this bin
    add [small_next + 0], 2, [rb + ptr]

    # second chunk is now head of the list
    add [small_next + 0], 2, [ip + 1]
    add [0], 0, [small_next + 0]

    # previous chunk of the new head chunk, if any, is now empty
    jz  [small_next + 0], alloc_cut_chunk
    add [small_next + 0], 3, [ip + 3]
    add 0, 0, [0]

    jz  0, alloc_cut_chunk

alloc_small_16:
    jz  [small_next + 1], alloc_small_24

    # use the first chunk from this bin
    add [small_next + 1], 2, [rb + ptr]

    # second chunk is now head of the list
    add [small_next + 1], 2, [ip + 1]
    add [0], 0, [small_next + 1]

    # previous chunk of the new head chunk, if any, is now empty
    jz  [small_next + 1], alloc_cut_chunk
    add [small_next + 1], 3, [ip + 3]
    add 0, 0, [0]

    jz  0, alloc_cut_chunk

alloc_small_24:
    jz  [small_next + 2], alloc_small_32

    # use the first chunk from this bin
    add [small_next + 2], 2, [rb + ptr]

    # second chunk is now head of the list
    add [small_next + 2], 2, [ip + 1]
    add [0], 0, [small_next + 2]

    # previous chunk of the new head chunk, if any, is now empty
    jz  [small_next + 2], alloc_cut_chunk
    add [small_next + 2], 3, [ip + 3]
    add 0, 0, [0]

    jz  0, alloc_cut_chunk

alloc_small_32:
    jz  [small_next + 3], alloc_small_40

    # use the first chunk from this bin
    add [small_next + 3], 2, [rb + ptr]

    # second chunk is now head of the list
    add [small_next + 3], 2, [ip + 1]
    add [0], 0, [small_next + 3]

    # previous chunk of the new head chunk, if any, is now empty
    jz  [small_next + 3], alloc_cut_chunk
    add [small_next + 3], 3, [ip + 3]
    add 0, 0, [0]

    jz  0, alloc_cut_chunk

alloc_small_40:
    jz  [small_next + 4], alloc_small_48

    # use the first chunk from this bin
    add [small_next + 4], 2, [rb + ptr]

    # second chunk is now head of the list
    add [small_next + 4], 2, [ip + 1]
    add [0], 0, [small_next + 4]

    # previous chunk of the new head chunk, if any, is now empty
    jz  [small_next + 4], alloc_cut_chunk
    add [small_next + 4], 3, [ip + 3]
    add 0, 0, [0]

    jz  0, alloc_cut_chunk

alloc_small_48:
    jz  [small_next + 5], alloc_small_56

    # use the first chunk from this bin
    add [small_next + 5], 2, [rb + ptr]

    # second chunk is now head of the list
    add [small_next + 5], 2, [ip + 1]
    add [0], 0, [small_next + 5]

    # previous chunk of the new head chunk, if any, is now empty
    jz  [small_next + 5], alloc_cut_chunk
    add [small_next + 5], 3, [ip + 3]
    add 0, 0, [0]

    jz  0, alloc_cut_chunk

alloc_small_56:
    jz  [small_next + 6], alloc_small_64

    # use the first chunk from this bin
    add [small_next + 6], 2, [rb + ptr]

    # second chunk is now head of the list
    add [small_next + 6], 2, [ip + 1]
    add [0], 0, [small_next + 6]

    # previous chunk of the new head chunk, if any, is now empty
    jz  [small_next + 6], alloc_cut_chunk
    add [small_next + 6], 3, [ip + 3]
    add 0, 0, [0]

    jz  0, alloc_cut_chunk

alloc_small_64:
    jz  [small_next + 7], alloc_large

    # use the first chunk from this bin
    add [small_next + 7], 2, [rb + ptr]

    # second chunk is now head of the list
    add [small_next + 7], 2, [ip + 1]
    add [0], 0, [small_next + 7]

    # previous chunk of the new head chunk, if any, is now empty
    jz  [small_next + 7], alloc_cut_chunk
    add [small_next + 7], 3, [ip + 3]
    add 0, 0, [0]

    jz  0, alloc_cut_chunk

alloc_large_calc_size:
    # allocate a large chunk, size rounded up to nearest multiple of 8
    add [rb + size], 0, [rb - 1]
    arb -1
    call calc_multiple_8
    add [rb - 3], 0, [rb + size]

alloc_large:
    # first try to find a fitting chunk in the large bin
    # TODO also needs size
    add large_next, 0, [rb - 1]
    add large_prev, 0, [rb - 2]
    arb -2
    call alloc_large_bin
    jz  [rb - 3], alloc_merge_small

    add [rb - 3], 2, [rb + ptr]
    jz  0, alloc_cut_chunk

alloc_merge_small:
    # now try to merge the small bins until we build a chunk that fits
    # TODO also needs size
    add large_next, 0, [rb - 1]
    add large_prev, 0, [rb - 2]
    arb -2
    call merge_small_bins
    jz  [rb - 3], alloc_sbrk

    add [rb - 3], 2, [rb + ptr]
    jz  0, alloc_cut_chunk

alloc_sbrk:
    # last resort, call sbrk to enlarge the heap
    add [rb + size], 0, [rb - 1]
    arb -1
    call sbrk

    # create a header for the new chunk
    add [rb - 3], 0, [ip + 3]
    add [rb + size], 0, [0]
    add [rb - 3], 1, [ip + 3]
    add 0, 0, [0]

    # no need to cut this chunk, just return it
    add [rb - 3], 2, [rb + ptr]
    jz  0, alloc_done

alloc_cut_chunk:
    # the chunk pointed to by [rb + ptr] - 2 needs to be split into size and the rest
    # the first part will be returned in [rb + ptr], the rest if any needs to be stored in the correct bin

    # TODO

alloc_done:
    arb X
    ret 1
.ENDFRAME

##########
calc_multiple_8:
.FRAME input; output                    # returns output
    arb -X

    # TODO

n = 8a + b
k = 8a - b

n - k = 2b


    arb X
    ret 1
.ENDFRAME



alloc_large_bin
# TODO iterate whole list, merge chunks

merge_small_bins
# TODO each small bin, merge until you find a matching size, if smaller than needed store in correct bin


.EOF


    # we only support certain block sizes
    lt  MEM_BLOCK_SIZE, [rb + size], [rb + tmp]
    jz  [rb + tmp], alloc_size_ok

    add err_allocation_size, 0, [rb]
    call report_libxib_error

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

# small bins, for sizes 8, 16, 24, 32, 40, 48, 56 and 64, each is a doubly linked list
small_next:
    db  0
    db  0
    db  0
    db  0
    db  0
    db  0
    db  0
    db  0
small_prev:
    db  0
    db  0
    db  0
    db  0
    db  0
    db  0
    db  0
    db  0

# large bin, for sizes >64, a doubly linked list
large_next:
    db  0
small_next:
    db  0

# table of nearest multiples of 8
multiple_8:
    db   0
    db   8,  8,  8,  8,  8,  8,  8,  8
    db  16, 16, 16, 16, 16, 16, 16, 16
    db  24, 24, 24, 24, 24, 24, 24, 24
    db  32, 32, 32, 32, 32, 32, 32, 32
    db  40, 40, 40, 40, 40, 40, 40, 40
    db  48, 48, 48, 48, 48, 48, 48, 48
    db  56, 56, 56, 56, 56, 56, 56, 56
    db  64, 64, 64, 64, 64, 64, 64, 64

.EOF
