    arb stack

next_digit:
    in  [digit]
    eq  [digit], 44, [flag]
    jnz [flag], finish_byte
    eq  [digit], 10, [flag]
    jnz [flag], finish_byte
    add [digit], -48, [digit]
    mul [byte], 10, [byte]
    add [byte], [digit], [byte]
    jz  0, next_digit

finish_byte:
+3 = size_in:
    add [byte], 0, [mem]
    add 0, 0, [byte]
    add [size], 1, [size]
    add [size_in], 1, [size_in]

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
    add [size], 1, [size]

print_byte:
+1 = size_out:
    add [mem], '0', [byte]
    out [byte]
    jz  [size], finish_print
    out 44
    add [size], 1, [size]
    add [size_out], 1, [size_out]
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