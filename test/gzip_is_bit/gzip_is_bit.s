.IMPORT is_bit

    arb stack

    # test set bits

    # 00000001, bit 0
    add 1, 0, [rb - 1]
    add 0, 0, [rb - 2]
    arb -2
    call is_bit
    add '0', [rb - 4], [rb - 4]
    out [rb - 4]

    # 00000010, bit 1
    add 2, 0, [rb - 1]
    add 1, 0, [rb - 2]
    arb -2
    call is_bit
    add '0', [rb - 4], [rb - 4]
    out [rb - 4]

    # 01010101, bit 2
    add 125, 0, [rb - 1]
    add 2, 0, [rb - 2]
    arb -2
    call is_bit
    add '0', [rb - 4], [rb - 4]
    out [rb - 4]

    # 10101010, bit 3
    add 170, 0, [rb - 1]
    add 3, 0, [rb - 2]
    arb -2
    call is_bit
    add '0', [rb - 4], [rb - 4]
    out [rb - 4]

    # 00010000, bit 4
    add 16, 0, [rb - 1]
    add 4, 0, [rb - 2]
    arb -2
    call is_bit
    add '0', [rb - 4], [rb - 4]
    out [rb - 4]

    # 00100000, bit 5
    add 32, 0, [rb - 1]
    add 5, 0, [rb - 2]
    arb -2
    call is_bit
    add '0', [rb - 4], [rb - 4]
    out [rb - 4]

    # 01000000, bit 6
    add 64, 0, [rb - 1]
    add 6, 0, [rb - 2]
    arb -2
    call is_bit
    add '0', [rb - 4], [rb - 4]
    out [rb - 4]

    # 10000000, bit 7
    add 128, 0, [rb - 1]
    add 7, 0, [rb - 2]
    arb -2
    call is_bit
    add '0', [rb - 4], [rb - 4]
    out [rb - 4]

    # test unset bits

    # 00000010, bit 0
    add 2, 0, [rb - 1]
    add 0, 0, [rb - 2]
    arb -2
    call is_bit
    add '0', [rb - 4], [rb - 4]
    out [rb - 4]

    # 11111101, bit 1
    add 253, 0, [rb - 1]
    add 1, 0, [rb - 2]
    arb -2
    call is_bit
    add '0', [rb - 4], [rb - 4]
    out [rb - 4]

    # 11111011, bit 2
    add 251, 0, [rb - 1]
    add 2, 0, [rb - 2]
    arb -2
    call is_bit
    add '0', [rb - 4], [rb - 4]
    out [rb - 4]

    # 11110111, bit 3
    add 247, 0, [rb - 1]
    add 3, 0, [rb - 2]
    arb -2
    call is_bit
    add '0', [rb - 4], [rb - 4]
    out [rb - 4]

    # 11101111, bit 4
    add 239, 0, [rb - 1]
    add 4, 0, [rb - 2]
    arb -2
    call is_bit
    add '0', [rb - 4], [rb - 4]
    out [rb - 4]

    # 11011111, bit 5
    add 223, 0, [rb - 1]
    add 5, 0, [rb - 2]
    arb -2
    call is_bit
    add '0', [rb - 4], [rb - 4]
    out [rb - 4]

    # 10111111, bit 6
    add 191, 0, [rb - 1]
    add 6, 0, [rb - 2]
    arb -2
    call is_bit
    add '0', [rb - 4], [rb - 4]
    out [rb - 4]

    # 01111111, bit 7
    add 127, 0, [rb - 1]
    add 7, 0, [rb - 2]
    arb -2
    call is_bit
    add '0', [rb - 4], [rb - 4]
    out [rb - 4]

    out 10
    hlt

    ds  50, 0
stack:

.EOF
