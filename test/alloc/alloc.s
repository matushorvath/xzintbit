.EXPORT report_libxib_error

.IMPORT alloc
.IMPORT free
.IMPORT dump_heap

.IMPORT brk

.IMPORT print_num
.IMPORT print_str

    arb stack
    call main
    hlt

    ds  50, 0
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

    # phase 0: allocate 0 1 5 6 17 5 90 17 bytes, all from brk
    add 0, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p0_0]

    add 1, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p0_1]

    add 5, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p0_5a]

    add 6, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p0_6]

    add 17, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p0_17a]

    add 5, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p0_5b]

    add 90, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p0_90]

    add 17, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p0_17b]

    # phase 0: free all allocated pointers
    add [p0_0], 0, [rb - 1]
    arb -1
    call test_free

    add [p0_1], 0, [rb - 1]
    arb -1
    call test_free

    add [p0_5a], 0, [rb - 1]
    arb -1
    call test_free

    add [p0_6], 0, [rb - 1]
    arb -1
    call test_free

    add [p0_17a], 0, [rb - 1]
    arb -1
    call test_free

    add [p0_5b], 0, [rb - 1]
    arb -1
    call test_free

    add [p0_90], 0, [rb - 1]
    arb -1
    call test_free

    add [p0_17b], 0, [rb - 1]
    arb -1
    call test_free

    # heap state after phase 0:
    # 8: 4, 19: 2, 92: 1

    ##########
    add 1, 0, [rb - 1]
    arb -1
    call print_phase

    # phase 1: allocate 5 6 1 5 (all use 8, no split)
    add 5, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p1_5a]

    add 6, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p1_6]

    add 1, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p1_1]

    add 5, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p1_5b]

    # phase 1: allocate 3 (uses 19, split off 11), 4 (uses 11, no split)
    add 3, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p1_3]

    add 4, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p1_4]

    # phase 1: allocate 91(brk), 89(uses 92), 90(brk)
    add 91, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p1_91]

    add 89, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p1_89]

    add 90, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p1_90]

    # phase 1: allocate 29(brk)
    add 29, 0, [rb - 1]
    arb -1
    call test_alloc
    add [rb - 3], 0, [p1_29]

    # heap state after phase 1:
    # 19: 1

    ret 0
.ENDFRAME

# allocated pointers
p0_0:
    db  0
p0_1:
    db  0
p0_5a:
    db  0
p0_6:
    db  0
p0_17a:
    db  0
p0_5b:
    db  0
p0_90:
    db  0
p0_17b:
    db  0

p1_5a:
    db  0
p1_6:
    db  0
p1_1:
    db  0
p1_5b:
    db  0
p1_3:
    db  0
p1_4:
    db  0
p1_91:
    db  0
p1_89:
    db  0
p1_90:
    db  0
p1_29:
    db  0

print_phase:
.FRAME phase;
    add print_phase_msg, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + phase], 0, [rb - 1]
    arb -1
    call print_num

    out 10

    ret 1

print_phase_msg:
    db  "==========", 10, "PHASE ", 0
.ENDFRAME

test_alloc:
.FRAME size; ptr                        # returns ptr
    arb -1

    # print test description and size to alloc
    add test_alloc_desc_msg, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + size], 0, [rb - 1]
    arb -1
    call print_num

    out ':'
    out ' '

    # call alloc
    add [rb + size], 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + ptr]

    # print the pointer returned
    add test_alloc_ptr_msg, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + ptr], 0, [rb - 1]
    arb -1
    call print_num

    # print returned chunk size, if any
    jz  [rb + ptr], test_alloc_after_size

    out ','
    out ' '

    add test_alloc_size_msg, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + ptr], -2, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_num

test_alloc_after_size:
    # dump heap information
    out 10
    call dump_heap

    arb 1
    ret 1

test_alloc_desc_msg:
    db  "----------", 10, "alloc ", 0
test_alloc_ptr_msg:
    db  "ptr ", 0
test_alloc_size_msg:
    db  "size ", 0
.ENDFRAME

test_free:
.FRAME ptr;
    # print test description and pointer to free
    add test_free_desc_msg, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + ptr], 0, [rb - 1]
    arb -1
    call print_num

    # print freed chunk size, if any
    jz  [rb + ptr], test_free_after_size

    out ':'
    out ' '

    add test_free_size_msg, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + ptr], -2, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_num

test_free_after_size:
    # call free
    add [rb + ptr], 0, [rb - 1]
    arb -1
    call free

    # dump heap information
    out 10
    call dump_heap

    ret 1

test_free_desc_msg:
    db  "----------", 10, "free ", 0
test_free_size_msg:
    db  "size ", 0
.ENDFRAME

report_libxib_error:
.FRAME
    add report_libxib_error_msg, 0, [rb - 1]
    arb -1
    call print_str

    ret 0

report_libxib_error_msg:
    db  "libxib error", 10, 0
.ENDFRAME

.EOF
