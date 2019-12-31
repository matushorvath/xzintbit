    arb stack

##########
main:
    out '?'
    out 10

    cal read
    cal print
    hlt

##########
read:
    # digit, byte, flag
    arb -3

next_digit:
    in  [rb - 0]
    eq  [rb - 0], 44, [rb - 2]
    jnz [rb - 2], finish_byte
    eq  [rb - 0], 10, [rb - 2]
    jnz [rb - 2], finish_byte
    add [rb - 0], -48, [rb - 0]
    mul [rb - 1], 10, [rb - 1]
    add [rb - 1], [rb - 0], [rb - 1]
    jz  0, next_digit

finish_byte:
+3 = size_in:
    add [rb - 1], 0, [mem]
    add 0, 0, [rb - 1]
    add [size_in], 1, [size_in]

    eq  [rb - 0], 10, [rb - 2]
    jz  [rb - 2], next_digit

    add [size_in], -1, [size]

    arb 3
    ret 0

##########
print:
    # byte, flag
    arb -2

    out 'L'
    out 'i'
    out 's'
    out 't'
    out ':'
    out 10

    jz  [size], finish_print

print_byte:
+1 = size_out:
    add [mem], '0', [rb - 0]
    out [rb - 0]

    eq  [size], [size_out], [rb - 1]
    jnz [rb - 1], finish_print
    add [size_out], 1, [size_out]

    out 44
    jz  0, print_byte

finish_print:
    out 10
    arb 2
    ret 0

##########
# globals
size:
    db  0

##########
    ds  50, 0
stack:

mem:
