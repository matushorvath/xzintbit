# Real heap allocator, work in progress
# https://gist.github.com/apsun/caa3c5552dce7b13b898b70569b1f239

# .EXPORT alloc
# .EXPORT free

# from builtin
.IMPORT __heap_start

# # from print.s
# .IMPORT print_str
# 
# from outside of this library
# .IMPORT report_libxib_error
# 

# memory block header size
.SYMBOL HEADER_SIZE                     2

# data alignment size
.SYMBOL ALIGN_SIZE                      2

# heap size increment                   1000
.SYMBOL HEAP_INCREMENT_SIZE             4096

# /*
#  * Masks for extracting the size and flags out of the
#  * header info fields.
#  */
# #define MYA_MASK_SIZE ~0x7
# #define MYA_FLAG_USED 0x1

# /*
#  * Rounds up to a multiple of the specified alignment.
#  * This may overflow if x is very large.
#  */
# #define mya_round_up(x, align) (((x) + (align) - 1) & -(align))

# /*
#  * Macros to make type-casts more readable.
#  */
# #define mya_as_bytes(x) ((char *)(x))
# #define mya_as_header(x) ((mya_header_t *)(x))

# /*
#  * Macros for moving between the header and user data.
#  */
# #define mya_data_to_header(d) (mya_as_header(mya_as_bytes(d) - HEADER_SIZE))
# #define mya_header_to_data(h) (mya_as_bytes(h) + HEADER_SIZE)

# /*
#  * Macros for accessing the header size and flags independently.
#  *
#  * w = which info field to access, either 'prev' or 'curr'
#  * h = pointer to header
#  * v = new value
#  */
# #define mya_info(w, h) (h->w##_info)
# #define mya_size(w, h) (mya_info(w, h) & MYA_MASK_SIZE)
# #define mya_used(w, h) (mya_info(w, h) & MYA_FLAG_USED)
# #define mya_set_size(w, h, v) (mya_info(w, h) = (mya_info(w, h) & ~MYA_MASK_SIZE) | (v))
# #define mya_set_used(w, h, v) (mya_info(w, h) = (mya_info(w, h) & ~MYA_FLAG_USED) | (v))
# #define mya_is_sentinel(w, h) (mya_size(w, h) == 0)

# /*
#  * Macros for moving to the previous and next adjacent block headers.
#  */
# #define mya_next(h) (mya_as_header(mya_header_to_data(h) + mya_size(curr, h)))
# #define mya_prev(h) (mya_data_to_header(mya_as_bytes(h) - mya_size(prev, h)))

# block structure layout
.SYMBOL MODULE_NEXT_PTR             0
.SYMBOL MODULE_CODE_HEAD            1
.SYMBOL MODULE_CODE_TAIL            2
.SYMBOL MODULE_CODE_INDEX           3
.SYMBOL MODULE_CODE_LENGTH          4
.SYMBOL MODULE_RELOC_HEAD           5
.SYMBOL MODULE_RELOC_TAIL           6
.SYMBOL MODULE_RELOC_INDEX          7
.SYMBOL MODULE_IMPORTS_HEAD         8
.SYMBOL MODULE_EXPORTS_HEAD         9
.SYMBOL MODULE_NEEDED               10          # 0 = not needed, 1 = needed
.SYMBOL MODULE_INCLUDED             11          # 0 = not included, 1 = included
.SYMBOL MODULE_ADDRESS              12
.SYMBOL MODULE_SIZE                 13

typedef struct mya_header {
    /*
     * Holds the user data size in bytes and status flags of
     * the previous adjacent block.
     */
    size_t prev_info;

    /*
     * Holds the user data size in bytes and status flags of
     * the current block.
     */
    size_t curr_info;

    /*
     * If the current block is not in use, holds a pointer to
     * the previous block in the free list.
     */
    struct mya_header *prev_free;

    /*
     * If the current block is not in use, holds a pointer to
     * the next block in the free list.
     */
    struct mya_header *next_free;
} mya_header_t;

# head of the linked list of free blocks
free_head:
    db  0

# start of unused memory
heap_end:
    db  __heap_start

# detect when we need to initialize heap
initialized:
    db  0

##########
# add_free_block:
.FRAME header; tmp
    arb -1

    # set pointer to next free block in the block we are returning
    add [rb + block], 0, [ip + 3]
    add [free_head], 0, [0]

    # set new free block head
    add [rb + block], 0, [free_head]

    arb 1
    ret 1
.ENDFRAME

# /*
#  * Adds a block to the free list. Upon entry,
#  * prev_free and next_free may be in an invalid state.
#  */
# static void
# add_free_block(mya_header_t *header)
# {
#     header->prev_free = NULL;
#     header->next_free = mya_free_list;
#     if (mya_free_list != NULL) {
#         mya_free_list->prev_free = header;
#     }
#     mya_free_list = header;
# }



# /*
#  * Removes a block from the free list. Upon exit,
#  * prev_free and next_free may be in an invalid state.
#  */
# static void
# mya_remove_free_list(mya_header_t *header)
# {
#     mya_header_t *prev_free = header->prev_free;
#     mya_header_t *next_free = header->next_free;
# 
#     if (mya_free_list == header) {
#         mya_free_list = next_free;
#     }
# 
#     if (prev_free != NULL) {
#         prev_free->next_free = next_free;
#     }
# 
#     if (next_free != NULL) {
#         next_free->prev_free = prev_free;
#     }
# }
# 
# /*
#  * Attempts to coalesce a block with the next adjacent block.
#  * This will never invalidate the original block. The block
#  * may be allocated. Returns whether the block was coalesced.
#  */
# static bool
# mya_coalesce_next(mya_header_t *header)
# {
#     /* Can't coalesce if the next adjacent block is allocated */
#     mya_header_t *next_adj = mya_next(header);
#     if (mya_used(curr, next_adj)) {
#         return false;
#     }
# 
#     /* Remove next adjacent block from free list */
#     mya_remove_free_list(next_adj);
# 
#     /* New size equals combined size of two blocks plus header overhead */
#     size_t new_size = mya_size(curr, header) + HEADER_SIZE + mya_size(curr, next_adj);
# 
#     /* Update previous size and usage flag in next next adjacent block */
#     mya_header_t *next_next_adj = mya_next(next_adj);
#     mya_set_used(prev, next_next_adj, mya_used(curr, header));
#     mya_set_size(prev, next_next_adj, new_size);
# 
#     /* Update size of the current block */
#     mya_set_size(curr, header, new_size);
# 
#     return true;
# }
# 
# /*
#  * Attempts to coalesce a free block with the previous adjacent block.
#  * If coalescing occurs, the original block will be invalidated.
#  */
# static mya_header_t *
# mya_coalesce_prev(mya_header_t *header)
# {
#     if (!mya_used(prev, header)) {
#         header = mya_prev(header);
#         mya_coalesce_next(header);
#     }
# 
#     return header;
# }
# 
# /*
#  * Attempts to coalesce a free block in both directions.
#  * Returns a pointer to the new, possibly merged free block.
#  * The original block will be invalidated if backwards
#  * coalescing occurs.
#  */
# static mya_header_t *
# mya_coalesce(mya_header_t *header)
# {
#     /* Coalesce forwards */
#     mya_coalesce_next(header);
# 
#     /* Coalesce backwards */
#     return mya_coalesce_prev(header);
# }
# 
# /*
#  * Wrapper for sbrk that checks for overflow.
#  * Note that delta is unsigned, since we don't support
#  * shrinking the data break on free yet.
#  */
# static bool
# mya_sbrk(size_t delta, void **orig_brk, void **new_brk)
# {
#     /* If allocation would overflow, fail fast */
#     if ((size_t)heap_end + delta < (size_t)heap_end) {
#         return false;
#     }
# 
#     /* We know the allocation is safe, call sbrk */
#     void *last_brk;
#     if ((last_brk = sbrk(delta)) == (void *)-1) {
#         return false;
#     }
# 
#     /* Update cached brk value */
#     heap_end = (void *)((char *)last_brk + delta);
# 
#     *orig_brk = last_brk;
#     *new_brk = heap_end;
#     return true;
# }
# 
# /*
#  * Initializes the global allocator state. After this function
#  * returns true, it must not be called again.
#  */
# static bool
# mya_initialize(void)
# {
#     /*
#      * This function sets up the sentinel headers, one at the
#      * start of the data, one at the end. Notice that the sentinel
#      * at the end is only halfway allocated; only the info fields
#      * lie within a valid page.
#      *         ____________________________________________
#      *        | | size = 0 | used = 1 |            ^
#      *        | | size = X | used = 0 |___         |
#      * header | |   prev_free = NULL  | ^          |
#      *        |_|___next_free = NULL__| |          |
#      *          |          .          | X    HEAP_INCREMENT_SIZE
#      *          |          .          | |          |
#      *         _|_____________________|_v_         |
#      *        | | size = X | used = 0 |            |
#      * header | | size = 0 | used = 1 |____________v_______
#      */
# 
#     /* Initialize cached brk value */
#     heap_end = sbrk(0);
#     if (heap_end == (void *)-1) {
#         return false;
#     }
# 
#     /* Allocate some starting memory */
#     void *orig_brk, *new_brk;
#     if (!mya_sbrk(HEAP_INCREMENT_SIZE, &orig_brk, &new_brk)) {
#         return false;
#     }
# 
#     /* Set up the sentinel blocks */
#     mya_header_t *bottom = mya_as_header(orig_brk);
#     mya_set_size(prev, bottom, 0);
#     mya_set_used(prev, bottom, 1);
# 
#     mya_header_t *top = mya_data_to_header(new_brk);
#     mya_set_size(curr, top, 0);
#     mya_set_used(curr, top, 1);
# 
#     /* Initialize the initial free block */
#     mya_set_size(curr, bottom, HEAP_INCREMENT_SIZE - 2 * HEADER_SIZE);
#     mya_set_used(curr, bottom, 0);
# 
#     mya_set_size(prev, top, HEAP_INCREMENT_SIZE - 2 * HEADER_SIZE);
#     mya_set_used(prev, top, 0);
# 
#     /* Add free block to free list */
#     add_free_block(bottom);
# 
#     initialized = true;
#     return true;
# }
# 
# /*
#  * Finds a block that is large enough to fit an allocation
#  * with the specified user data size, or NULL if there are
#  * no available blocks that are large enough.
#  */
# static mya_header_t *
# mya_find_free_block(size_t aligned_size)
# {
#     mya_header_t *header = mya_free_list;
#     while (header != NULL) {
#         if (mya_size(curr, header) >= aligned_size) {
#             return header;
#         }
#         header = header->next_free;
#     }
#     return NULL;
# }
# 
# /*
#  * Allocates a new block from the system with at least the specified
#  * size. If there is no more available memory, returns NULL. Otherwise,
#  * returns the header of the top-most block (which might not be the
#  * new block due to coalescing, if the original top-most block was free).
#  */
# static mya_header_t *
# mya_sbrk_new_block(size_t aligned_size)
# {
#     /* Round to a multiple of the page size */
#     size_t page_size = mya_round_up(aligned_size + HEADER_SIZE, HEAP_INCREMENT_SIZE);
# 
#     /* Request some more memory from the kernel */
#     void *orig_brk, *new_brk;
#     if (!mya_sbrk(page_size, &orig_brk, &new_brk)) {
#         return NULL;
#     }
# 
#     /* New block starts right before the data break */
#     mya_header_t *header = mya_data_to_header(orig_brk);
# 
#     /* Convert sentinel to a normal block */
#     mya_set_size(curr, header, page_size - HEADER_SIZE);
#     mya_set_used(curr, header, 0);
# 
#     /* Initialize new sentinel block */
#     mya_header_t *sentinel = mya_data_to_header(new_brk);
#     mya_set_size(prev, sentinel, page_size - HEADER_SIZE);
#     mya_set_used(prev, sentinel, 0);
#     mya_set_size(curr, sentinel, 0);
#     mya_set_used(curr, sentinel, 1);
# 
#     /* Add new block to free list */
#     add_free_block(header);
# 
#     /* Coalesce with the previous block if it's free */
#     return mya_coalesce_prev(header);
# }
# 
# /*
#  * Attempts to split a block into two smaller blocks, with the first
#  * having size >= aligned_size. If the block is too small, returns NULL.
#  * Otherwise, returns a pointer to the second block, which will be unused.
#  * The second block may be coalesced with the next adjacent block.
#  */
# static mya_header_t *
# mya_split_block(mya_header_t *header, size_t aligned_size)
# {
#     size_t curr_size = mya_size(curr, header);
# 
#     /* Only split if we have enough space for another allocation */
#     if (curr_size < aligned_size + HEADER_SIZE + ALIGN_SIZE) {
#         return NULL;
#     }
# 
#     /* This will be the size of our split block */
#     size_t split_size = curr_size - aligned_size - HEADER_SIZE;
#     
#     /* Update the previous field of the next adjacent block */
#     mya_header_t *next_header = mya_next(header);
#     mya_set_size(prev, next_header, split_size);
#     mya_set_used(prev, next_header, 0);
# 
#     /* Update the original header with the new size */
#     mya_set_size(curr, header, aligned_size);
# 
#     /* Now find out where our split header is */
#     mya_header_t *split_header = mya_next(header);
# 
#     /* Initialize prev field of split header */
#     mya_set_size(prev, split_header, aligned_size);
#     mya_set_used(prev, split_header, mya_used(curr, header));
# 
#     /* Initialize curr field of split header */
#     mya_set_size(curr, split_header, split_size);
#     mya_set_used(curr, split_header, 0);
# 
#     /* Add new block to free list */
#     add_free_block(split_header);
# 
#     /* Try to coalesce new block with the next adjacent block */
#     mya_coalesce_next(split_header);
# 
#     return split_header;
# }
# 
# /*
#  * Allocates the specified number of bytes and returns a pointer
#  * to the allocated memory. If size equals 0 or there is no
#  * more memory available, NULL is returned.
#  */
# void *
# mya_malloc(size_t size)
# {
#     /* malloc(0) always returns NULL */
#     if (size == 0) {
#         return NULL;
#     }
# 
#     /* Initialize global state on first run */
#     if (!initialized && !mya_initialize()) {
#         return NULL;
#     }
# 
#     /* Round allocation size up to an appropriate alignment */
#     size_t aligned_size = mya_round_up(size, ALIGN_SIZE);
#     if (aligned_size == 0) {
#         return NULL;
#     }
# 
#     /*
#      * First try to find an existing free block.
#      * If that fails, try to allocate a new block.
#      * If that also fails, we've run out of memory.
#      */
#     mya_header_t *header = mya_find_free_block(aligned_size);
#     if (header == NULL) {
#         header = mya_sbrk_new_block(aligned_size);
#         if (header == NULL) {
#             return NULL;
#         }
#     }
# 
#     /* Split the block as necessary */
#     mya_split_block(header, aligned_size);
# 
#     /* Remove block from the free list */
#     mya_remove_free_list(header);
# 
#     /* Mark block as allocated */
#     mya_set_used(curr, header, 1);
#     mya_set_used(prev, mya_next(header), 1);
# 
#     /* Return pointer to the user data */
#     return mya_header_to_data(header);
# }
# 
# /*
#  * Frees a block of memory originally allocated by malloc,
#  * calloc, or realloc. Calling free on NULL is a no-op.
#  */
# void
# mya_free(void *ptr)
# {
#     /* free(NULL) is a no-op */
#     if (ptr == NULL) {
#         return;
#     }
# 
#     /* Find header for the user data */
#     mya_header_t *header = mya_data_to_header(ptr);
# 
#     /* Mark block as free */
#     mya_set_used(curr, header, 0);
#     mya_set_used(prev, mya_next(header), 0);
# 
#     /* Add block into the free list */
#     add_free_block(header);
# 
#     /* Coalesce with any neighboring free blocks */
#     mya_coalesce(header);
# }
# 
# /*
#  * Allocates a 0-initialized block of memory of with size
#  * equal to num * size. If num * size would overflow, or
#  * num and/or size equal 0, or there is no more available
#  * memory, NULL is returned.
#  */
# void *
# mya_calloc(size_t num, size_t size)
# {
#     /* calloc(num, 0) and calloc(0, size) always returns NULL */
#     if (size == 0 || num == 0) {
#         return NULL;
#     }
# 
#     /* Prevent overflow when computing num * size */
#     if (num > SIZE_MAX / size) {
#         return NULL;
#     }
# 
#     /* Otherwise, simply use malloc() followed by memset() */
#     void *ptr = mya_malloc(num * size);
#     if (ptr != NULL) {
#         memset(ptr, 0, num * size);
#     }
#     return ptr;
# }
# 
# /*
#  * Changes the size of a previously allocated memory block.
#  * The contents will be unchanged up to the previous size of
#  * the block, and any additional memory will not be initialized.
#  * If size equals 0, this is equivalent to calling free(ptr). If
#  * ptr is NULL, this is equivalent to calling malloc(size).
#  */
# void *
# mya_realloc(void *ptr, size_t size)
# {
#     /* realloc(NULL, size) is the same as malloc(size) */
#     if (ptr == NULL) {
#         return mya_malloc(size);
#     }
# 
#     /* realloc(ptr, 0) is the same as free(ptr) */
#     if (size == 0) {
#         free(ptr);
#         return NULL;
#     }
# 
#     /* Round allocation size up to an appropriate alignment */
#     size_t aligned_size = mya_round_up(size, ALIGN_SIZE);
# 
#     /* Find header for the user data */
#     mya_header_t *header = mya_data_to_header(ptr);
# 
#     /* Save original size of block */
#     size_t orig_size = mya_size(curr, header);
# 
#     /*
#      * If we're shrinking the block, try to split it
#      * and return the same block. No copying required.
#      */
#     if (aligned_size <= orig_size) {
#         mya_split_block(header, aligned_size);
#         return ptr;
#     }
# 
#     /* Try coalescing with the next block */
#     if (mya_coalesce_next(header)) {
#         if (aligned_size <= mya_size(curr, header)) {
#             mya_split_block(header, aligned_size);
#             return ptr;
#         }
#     }
# 
#     /* If this is the last block, just sbrk some more memory */
#     if (mya_is_sentinel(curr, mya_next(header))) {
#         size_t sbrk_size = aligned_size - mya_size(curr, header);
#         mya_header_t *next_alloc = mya_sbrk_new_block(sbrk_size);
#         if (next_alloc != NULL) {
#             mya_coalesce_next(header);
#             mya_split_block(header, aligned_size);
#             return ptr;
#         }
#     }
# 
#     /*
#      * We tried everything but we still can't resize it in-place,
#      * fall back to malloc() followed by memcpy().
#      */
#     void *new_ptr = mya_malloc(size);
#     if (new_ptr != NULL) {
#         memcpy(new_ptr, ptr, orig_size);
#         mya_free(ptr);
#     }
#     return new_ptr;
# }
# 
# /*
#  * Some basic allocation correctness tests.
#  */
# #include <assert.h>
# #include <stdio.h>
# #include <time.h>
# #include <signal.h>
# 
# #define SMALL_SIZE_MIN 0
# #define SMALL_SIZE_MAX 64
# #define LARGE_SIZE_MIN 512
# #define LARGE_SIZE_MAX 1000000
# #define ITERATION_COUNT 10000
# #define RAND_RANGE(a, b) ((a) + rand() % ((b) - (a)))
# #define RAND_SIZE() \
#     ((rand() & 1) \
#        ? RAND_RANGE(SMALL_SIZE_MIN, SMALL_SIZE_MAX) \
#        : RAND_RANGE(LARGE_SIZE_MIN, LARGE_SIZE_MAX))
# 
# /* Saves the rand seed so we can reproduce crashes */
# static unsigned int seed;
# 
# static void
# handle_segfault(int signum)
# {
#     (void)signum;
#     printf("FAIL! Seed: %u\n", seed);
#     exit(1);
# }
# 
# int
# main(void)
# {
# #if MYA_REPLACE_STD
#     printf("Using custom allocator functions\n");
# #else
#     printf("Using stdlib allocator functions\n");
# #endif
# 
#     /* 0-sized allocation checks */
#     (void)malloc(0);
#     void *_ = realloc(NULL, 0);
#     (void)_;
#     (void)calloc(1, 0);
# 
#     /* Overflow checks */
#     assert(malloc(SIZE_MAX) == NULL);
#     assert(realloc(NULL, SIZE_MAX) == NULL);
#     assert(calloc(1, SIZE_MAX) == NULL);
#     assert(calloc(SIZE_MAX, SIZE_MAX) == NULL);
# 
#     /* Stuff to store our checks */
#     char *ptrs[ITERATION_COUNT];
#     int sizes[ITERATION_COUNT];
#     char chrs[ITERATION_COUNT];
# 
#     /* Handle segfaults for debugging */
#     struct sigaction sa;
#     sa.sa_handler = handle_segfault;
#     sa.sa_flags = SA_RESTART;
#     sigemptyset(&sa.sa_mask);
#     sigaction(SIGSEGV, &sa, NULL);
# 
#     /* I can haz randomness? */
#     seed = (unsigned int)time(NULL);
#     srand(seed);
# 
#     /* Start benchmark */
#     clock_t start = clock();
# 
#     /* malloc some randomly sized blocks */
#     for (int i = 0; i < ITERATION_COUNT; ++i) {
#         size_t sz = RAND_SIZE();
#         sizes[i] = 0;
#         ptrs[i] = malloc(sz);
#         if (ptrs[i] != NULL) {
#             sizes[i] = sz;
#             chrs[i] = (char)RAND_RANGE(0, 256);
#             for (size_t j = 0; j < sz; ++j) {
#                 ptrs[i][j] = chrs[i];
#             }
#         }
#     }
# 
#     /* free some of the pointers */
#     for (int i = 0; i < ITERATION_COUNT / 2; ++i) {
#         int index = rand() % ITERATION_COUNT;
#         free(ptrs[index]);
#         ptrs[index] = NULL;
#         sizes[index] = 0;
#         chrs[index] = '\0';
#     }
# 
#     /* realloc some of the pointers */
#     for (int i = 0; i < ITERATION_COUNT / 2; ++i) {
#         int index = rand() % ITERATION_COUNT;
#         size_t sz = RAND_SIZE();
#         void *x = realloc(ptrs[index], sz);
# 
#         if (sz == 0 || x != NULL) {
#             ptrs[index] = x;
#             sizes[index] = sz;
#             chrs[index] = (char)RAND_RANGE(0, 256);
#             for (size_t j = 0; j < sz; ++j) {
#                 ptrs[index][j] = chrs[index];
#             }
#         }
#     }
# 
#     /* Make sure our data is still intact */
#     for (int i = 0; i < ITERATION_COUNT; ++i) {
#         for (int j = 0; j < sizes[i]; ++j) {
#             assert(ptrs[i][j] == chrs[i]);
#         }
#     }
# 
#     /* Clean up our mess */
#     for (int i = 0; i < ITERATION_COUNT; ++i) {
#         free(ptrs[i]);
#         ptrs[i] = NULL;
#         sizes[i] = 0;
#         chrs[i] = '\0';
#     }
# 
#     /* End benchmark */
#     clock_t stop = clock();
#     double total_time = (double)(stop - start) / CLOCKS_PER_SEC;
#     printf("PASS! Time: %.02fs\n", total_time);
#     return 0;
# }




# ##########
# alloc:
# .FRAME size; block, tmp
#     arb -2
# 
#     # we only support certain block sizes
#     lt  MEM_BLOCK_SIZE, [rb + size], [rb + tmp]
#     jz  [rb + tmp], alloc_size_ok
# 
#     add err_allocation_size, 0, [rb]
#     call report_libxib_error
# 
# alloc_size_ok:
#     # do we have any free blocks?
#     jz  [free_head], alloc_create_block
# 
#     # yes, remove first block from the list and return it
#     add [free_head], 0, [rb + block]
# 
#     add [free_head], 0, [ip + 1]
#     add [0], 0, [free_head]
# 
#     jz  0, alloc_done
# 
# alloc_create_block:
#     # there are no free blocks, create one
#     add [heap_end], 0, [rb + block]
#     add [heap_end], MEM_BLOCK_SIZE, [heap_end]
# 
# alloc_done:
#     arb 2
#     ret 1
# .ENDFRAME
# 
# ##########
# free:
# .FRAME block; tmp
#     arb -1
# 
#     # set pointer to next free block in the block we are returning
#     add [rb + block], 0, [ip + 3]
#     add [free_head], 0, [0]
# 
#     # set new free block head
#     add [rb + block], 0, [free_head]
# 
#     arb 1
#     ret 1
# .ENDFRAME
# 
# ##########
# # globals
# 

# ##########
# # error messages
# 
# err_allocation_size:
#     db  "Unsupported allocation size", 0

.EOF
