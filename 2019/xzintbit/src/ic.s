next_digit:
    in  [digit]
    eq  [digit], 44, [flag]
    jnz [flag], finish_byte
    eq  [digit], 10, [flag]
    jnz [flag], finish_prog
    mul [rb + mem], 10, [rb + mem]
    add [rb + mem], [digit], [rb + mem]
    jz  0, next_digit

finish_byte:
    add [size], 1, [size]
    arb 1
    jz  0, next_digit

finish_prog:
    out 'L'
    out 'i'
    out 's'
    out 't'
    out ':'

    mul -1, [size], [size]
    arb [size]

    jz  [size], finish_print
print_byte:
    out [rb + mem]
    jz  [size], finish_print
    out 44
    arb 1
    add 1, [size], [size]
    jz  0, print_byte

finish_print:
    out 10
    hlt

size:
    db  0
digit:
    db  0
flag:
    db  0

mem: