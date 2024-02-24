    arb stack
    call caller
    hlt

    ds 10, 0
stack:

caller:
.FRAME address
    arb -1

    add callee, 0, [rb + address]

    # While processing the call, the assembler needs to adjust the parameter:
    # add [ip + 5], 0, [rb - 1]     # [ip + 5] is the return address
    # arb -1                        # here we adjust rb, which breaks the following jz
    # jz  0, [rb + address + 1]     # here [rb + address + 1] needs to compensate for the adjusted rb

    call [rb + address]

    arb 1
    ret 0
.ENDFRAME

callee:
.FRAME address
    arb -0

    out 65
    out 10

    arb 0
    ret 0
.ENDFRAME

.EOF
