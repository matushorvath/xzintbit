.IMPORT print

    arb stack

    add message, 0, [rb - 1]
    arb -1
    call print

    out 10

    hlt

message:
    db  "Hello world!", 0

    ds  20, 0
stack:

.EOF
