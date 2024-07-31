.EXPORT report_libxib_error

.IMPORT alloc_blocks
.IMPORT free
.IMPORT dump_heap

.IMPORT brk

.IMPORT print_num
.IMPORT print_str

    arb stack
    call main
    hlt

    ds  1000, 0
stack:

main:
.FRAME
    # make brk predictable
    add 1000000, 0, [rb - 1]
    arb -1
    call brk

    ##########
    add 0, 0, [rb - 1]
    arb -1
    call print_phase

    # phase 0: allocate 1 1 1 3 1 12 3 blocks, all from brk
    add 1, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p0_1a]

    add 1, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p0_1b]

    add 1, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p0_1c]

    add 3, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p0_3a]

    add 1, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p0_1d]

    add 12, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p0_12]

    add 3, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p0_3b]

    # phase 0: free all allocated pointers
    add [p0_0], 0, [rb - 1]
    arb -1
    call test_free

    add [p0_1a], 0, [rb - 1]
    arb -1
    call test_free

    add [p0_1b], 0, [rb - 1]
    arb -1
    call test_free

    add [p0_1c], 0, [rb - 1]
    arb -1
    call test_free

    add [p0_3a], 0, [rb - 1]
    arb -1
    call test_free

    add [p0_1d], 0, [rb - 1]
    arb -1
    call test_free

    add [p0_12], 0, [rb - 1]
    arb -1
    call test_free

    add [p0_3b], 0, [rb - 1]
    arb -1
    call test_free

    # heap state after phase 0: [1: 4], [3: 2], [12: 1]

    ##########
    add 1, 0, [rb - 1]
    arb -1
    call print_phase

    # phase 1: allocate 1 1 1 1 (all use bin 1, no split)
    add 1, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p1_1a]

    add 1, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p1_1b]

    add 1, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p1_1c]

    add 1, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p1_1d]

    # phase 1: allocate 1 (uses bin 3, split off to bin 2), 2 (uses bin 2, no split)
    add 1, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p1_1e]

    add 2, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p1_2]

    # phase 1: allocate 13 (brk), 12 (uses bin 12), 12 (brk)
    add 13, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p1_13]

    add 12, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p1_12a]

    add 12, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p1_12b]

    # heap state after phase 1: [3: 1]

    ##########
    add 2, 0, [rb - 1]
    arb -1
    call print_phase

    # phase 2: allocate 5 (brk), free 5 (heap state: [3: 1], [5: 1])
    add 5, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p2_5]

    add [p2_5], 0, [rb - 1]
    arb -1
    call test_free

    # phase 2: allocate 4 (uses bin 5, split off to bin 1)
    add 4, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p2_4]

    # phase 2: allocate 3 (uses bin 3)
    add 3, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p2_3]

    # phase 2: allocate 1 (uses bin 1)
    add 1, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p2_1]

    # heap state after phase 2: empty

    ##########
    add 3, 0, [rb - 1]
    arb -1
    call print_phase

    # phase 3: free p1_12b (heap state: [12: 1])
    add [p1_12b], 0, [rb - 1]
    arb -1
    call test_free

    # phase 3: allocate 1 (uses bin 12, split off 11 to large bin)
    add 1, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p3_1]

    # phase 3: allocate 4 (uses bin 11, split off to small bin 7)
    add 4, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p3_4]

    # phase 3: allocate 7 (uses small bin 7)
    add 7, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p3_7]

    # phase 3: allocate 200 (brk)
    add 200, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p3_200]

    # heap state after phase 3: empty

    ret 0
.ENDFRAME

# allocated pointers
p0_0:
    db  0
p0_1a:
    db  0
p0_1b:
    db  0
p0_1c:
    db  0
p0_3a:
    db  0
p0_1d:
    db  0
p0_12:
    db  0
p0_3b:
    db  0

p1_1a:
    db  0
p1_1b:
    db  0
p1_1c:
    db  0
p1_1d:
    db  0
p1_1e:
    db  0
p1_2:
    db  0
p1_13:
    db  0
p1_12a:
    db  0
p1_12b:
    db  0
p2_5:
    db  0

p2_4:
    db  0
p2_3:
    db  0
p2_1:
    db  0

p3_1:
    db  0
p3_4:
    db  0
p3_7:
    db  0
p3_200:
    db  0

print_phase:
.FRAME phase;
    add .msg, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + phase], 0, [rb - 1]
    arb -1
    call print_num

    out 10

    ret 1

.msg:
    db  "==========", 10, "PHASE ", 0
.ENDFRAME

test_alloc:
.FRAME block_count; ptr                 # returns ptr
    arb -1

    # print test description and size to alloc
    add .desc_msg, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + block_count], 0, [rb - 1]
    arb -1
    call print_num

    out ':'
    out ' '

    # call alloc
    add [rb + block_count], 0, [rb - 1]
    arb -1
    call alloc_blocks
    add [rb - 3], 0, [rb + ptr]

    # print the pointer returned
    add .ptr_msg, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + ptr], 0, [rb - 1]
    arb -1
    call print_num

    # print returned chunk size, if any
    jz  [rb + ptr], .after_size

    out ','
    out ' '

    add .size_msg, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + ptr], -2, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_num

.after_size:
    # dump heap information
    out 10
    call dump_heap

    arb 1
    ret 1

.desc_msg:
    db  "----------", 10, "alloc ", 0
.ptr_msg:
    db  "ptr ", 0
.size_msg:
    db  "blocks ", 0
.ENDFRAME

test_free:
.FRAME ptr;
    # print test description and pointer to free
    add .desc_msg, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + ptr], 0, [rb - 1]
    arb -1
    call print_num

    # print freed chunk size, if any
    jz  [rb + ptr], .after_size

    out ':'
    out ' '

    add .size_msg, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + ptr], -2, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_num

.after_size:
    # call free
    add [rb + ptr], 0, [rb - 1]
    arb -1
    call free

    # dump heap information
    out 10
    call dump_heap

    ret 1

.desc_msg:
    db  "----------", 10, "free ", 0
.size_msg:
    db  "blocks ", 0
.ENDFRAME

report_libxib_error:
.FRAME
    add .error_msg, 0, [rb - 1]
    arb -1
    call print_str

    hlt

.error_msg:
    db  "libxib error", 10, 0
.ENDFRAME

.EOF
