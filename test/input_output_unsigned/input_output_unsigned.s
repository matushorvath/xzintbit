    arb data

    # Test input and output of characters >= 128, to catch incorrect handling
    # of signed/unsigned bytes in the VM

    out 127
    out 128
    out 129
    out 253
    out 254
    out 255

    in  [rb + 0]
    eq  [rb + 0], 255, [rb + 1]
    jz  [rb + 1], fail
    out '='

    in  [rb + 0]
    eq  [rb + 0], 254, [rb + 1]
    jz  [rb + 1], fail
    out '='

    in  [rb + 0]
    eq  [rb + 0], 253, [rb + 1]
    jz  [rb + 1], fail
    out '='

    in  [rb + 0]
    eq  [rb + 0], 129, [rb + 1]
    jz  [rb + 1], fail
    out '='

    in  [rb + 0]
    eq  [rb + 0], 128, [rb + 1]
    jz  [rb + 1], fail
    out '='

    in  [rb + 0]
    eq  [rb + 0], 127, [rb + 1]
    jz  [rb + 1], fail
    out '='

    hlt

fail:
    out 'X'
    hlt

data:
    ds  1, 0

.EOF
