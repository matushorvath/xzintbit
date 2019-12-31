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

    add [size_in], -1, [size_in]
    jz  [size_in], finish_print

print_byte:
+1 = size_out:
    add [mem], '0', [byte]
    out [byte]

    eq  [size_in], [size_out], [flag]
    jnz [flag], finish_print
    add [size_out], 1, [size_out]

    out 44
    jz  0, print_byte

finish_print:
    out 10
    hlt

digit:
    db  0
byte:
    db  0
flag:
    db  0

stack:
    ds  50, 0

mem: