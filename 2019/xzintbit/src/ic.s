    arb stack

next_digit:
    in  [digit]
    eq  [digit], 44, [flag]
    jnz [flag], finish_byte
    eq  [digit], 10, [flag]
    jnz [flag], finish_byte
    mul [byte], 10, [byte]
    add [byte], [digit], [byte]
    jz  0, next_digit

finish_byte:
    add [byte], 0, [mem]
    add 0, 0, [byte]
    add [size], 1, [size]
    add [finish_byte + 3], 1, [finish_byte + 3]

    eq  [digit], 10, [flag]
    jz  [flag], next_digit

finish_prog:
    out 'L'
    out 'i'
    out 's'
    out 't'
    out ':'
    out 10

    jz  [size], finish_print
    mul -1, [size], [size]

print_byte:
    out [mem]
    jz  [size], finish_print
    out 44
    add [size], 1, [size]
    add [print_byte + 1], 1, [print_byte + 1]
    jz  0, print_byte

finish_print:
    out 10
    hlt

size:
    db  0
digit:
    db  0
byte:
    db  0
flag:
    db  0

stack:
    ds  50, 0

mem: