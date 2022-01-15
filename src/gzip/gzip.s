##########
# entry point
    arb stack

    call main
    hlt

##########
main:
.FRAME
    arb -0

    out 'H'
    out 'W'
    out 10

    arb 0
    ret 0
.ENDFRAME

##########
    ds  50, 0
stack:

.EOF
