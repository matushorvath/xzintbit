.EXPORT is_bit

##########
is_bit:
.FRAME byte, bit; tmp, idx
    arb -2

    add 7, 0, [rb + idx]

is_bit_loop:
    # find if idx-th bit is set
    add is_bit_consts, [rb + idx], [ip + 2]
    lt  [rb + byte], [0], [rb + tmp]
    jnz [rb + tmp], is_bit_loop_not_set

    # idx-th bit is set; if this is the one we want, return 1 in tmp
    eq  [rb + idx], [rb + bit], [rb + tmp]
    jnz [rb + tmp], is_bit_end

    # remove idx-th bit from byte
    add is_bit_consts, [rb + idx], [ip + 1]
    mul [0], -1, [rb + tmp]
    add [rb + byte], [rb + tmp], [rb + byte]

    # next bit
    add [rb + idx], -1, [rb + idx]
    jz  0, is_bit_loop

is_bit_loop_not_set:
    # idx-th bit is not set, if this is the one we want, return 0 in tmp
    eq  [rb + idx], [rb + bit], [rb + tmp]
    eq  [rb + tmp], 0, [rb + tmp]
    jz  [rb + tmp], is_bit_end

    # next bit
    add [rb + idx], -1, [rb + idx]
    jz  0, is_bit_loop

is_bit_end:
    arb 2
    ret 2

is_bit_consts:
    db  1
    db  2
    db  4
    db  8
    db  16
    db  32
    db  64
    db  128
.ENDFRAME

.EOF
