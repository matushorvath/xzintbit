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

    add 0, 0, [rb - 1]
    arb -1
    call test_alloc

    add 1, 0, [rb - 1]
    arb -1
    call test_alloc

    add 5, 0, [rb - 1]
    arb -1
    call test_alloc

    add 6, 0, [rb - 1]
    arb -1
    call test_alloc

    add 7, 0, [rb - 1]
    arb -1
    call test_alloc

    add 17, 0, [rb - 1]
    arb -1
    call test_alloc

    add 61, 0, [rb - 1]
    arb -1
    call test_alloc

    add 62, 0, [rb - 1]
    arb -1
    call test_alloc

    add 63, 0, [rb - 1]
    arb -1
    call test_alloc

    add 500, 0, [rb - 1]
    arb -1
    call test_alloc

    ret 0
.ENDFRAME

test_alloc:
.FRAME size; ptr
    arb -1

    # print separator and size to alloc
    add test_alloc_separator_msg, 0, [rb - 1]
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

test_alloc_separator_msg:
    db  "----------", 10, 0
test_alloc_ptr_msg:
    db  "ptr ", 0
test_alloc_size_msg:
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
