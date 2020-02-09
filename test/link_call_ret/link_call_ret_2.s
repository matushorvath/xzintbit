# Test correct relocation of the return address in a function call

call function

function:
.FRAME param; var1, var2
    arb -2
    add 0, 0, [0]
    arb 2
    ret 1
.ENDFRAME

.EOF
