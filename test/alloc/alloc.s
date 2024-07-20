.EXPORT report_libxib_error

.IMPORT alloc
.IMPORT free
.IMPORT dump_heap
.IMPORT brk

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

    # zero allocation
    call separator
    out '0'
    out ' '

    add 0, 0, [rb - 1]
    arb -1
    call alloc

    jnz [rb - 3], fail
    call ok

    # allocate 1 byte
    call separator
    out '1'
    out ' '

    add 1, 0, [rb - 1]
    arb -1
    call alloc

    jz  [rb - 3], fail
    call ok

    # allocate 5 bytes
    call separator
    out '5'
    out ' '

    add 5, 0, [rb - 1]
    arb -1
    call alloc

    jz  [rb - 3], fail
    call ok

    # allocate 6 bytes
    call separator
    out '6'
    out ' '

    add 6, 0, [rb - 1]
    arb -1
    call alloc

    jz  [rb - 3], fail
    call ok

    # allocate 7 bytes
    call separator
    out '7'
    out ' '

    add 7, 0, [rb - 1]
    arb -1
    call alloc

    jz  [rb - 3], fail
    call ok

    # allocate 17 bytes
    call separator
    out '1'
    out '7'
    out ' '

    add 17, 0, [rb - 1]
    arb -1
    call alloc

    jz  [rb - 3], fail
    call ok

    # allocate 61 bytes
    call separator
    out '6'
    out '1'
    out ' '

    add 61, 0, [rb - 1]
    arb -1
    call alloc

    jz  [rb - 3], fail
    call ok

    # allocate 62 bytes
    call separator
    out '6'
    out '2'
    out ' '

    add 62, 0, [rb - 1]
    arb -1
    call alloc

    jz  [rb - 3], fail
    call ok

    # allocate 63 bytes
    call separator
    out '6'
    out '3'
    out ' '

    add 65, 0, [rb - 1]
    arb -1
    call alloc

    jz  [rb - 3], fail
    call ok

    # allocate 500 bytes
    call separator
    out '5'
    out '0'
    out '0'
    out ' '

    add 500, 0, [rb - 1]
    arb -1
    call alloc

    jz  [rb - 3], fail
    call ok

    jz  0, done

fail:
    out 'f'
    out 'a'
    out 'i'
    out 'l'
    out 10

    call dump_heap

done:
    ret 0
.ENDFRAME

separator:
.FRAME
    out '-'
    out '-'
    out '-'
    out '-'
    out '-'
    out '-'
    out '-'
    out '-'
    out '-'
    out '-'
    out 10

    ret 0
.ENDFRAME

ok:
.FRAME
    out 'o'
    out 'k'
    out 10

    call dump_heap

    ret 0
.ENDFRAME

report_libxib_error:
.FRAME
    out 'l'
    out 'i'
    out 'b'
    out 'x'
    out 'i'
    out 'b'
    out ' '
    out 'e'
    out 'r'
    out 'r'
    out 'o'
    out 'r'
    out 10

    call dump_heap

    ret 0
.ENDFRAME

.EOF
