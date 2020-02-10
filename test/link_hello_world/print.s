.EXPORT print

##########
print:
.FRAME string; index, char
    arb -2

    add 0, 0, [rb + index]

print_loop:
    add [rb + string], [rb + index], [ip + 1]
    add [0], 0, [rb + char]

    jz  [rb + char], print_done

    out [rb + char]

    add [rb + index], 1, [rb + index]
    jz  0, print_loop

print_done:
    arb 2
    ret 1
.ENDFRAME

.EOF
