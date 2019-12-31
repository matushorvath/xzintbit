    arb stack

##########
main:
.FRAME
    out '?'
    out 10

    cal read
    cal print

    hlt
.ENDFRAME

##########
read:
.FRAME digit, byte, flag
    arb -3

read_next_digit:
    in  [rb + 0]
    eq  [rb + 0], 44, [rb + 2]
    jnz [rb + 2], read_finish_byte
    eq  [rb + 0], 10, [rb + 2]
    jnz [rb + 2], read_finish_byte
    add [rb + 0], -48, [rb + 0]
    mul [rb + 1], 10, [rb + 1]
    add [rb + 1], [rb + 0], [rb + 1]
    jz  0, read_next_digit

read_finish_byte:
+3 = read_size:
    add [rb + 1], 0, [mem]
    add 0, 0, [rb + 1]
    add [read_size], 1, [read_size]

    eq  [rb + 0], 10, [rb + 2]
    jz  [rb + 2], read_next_digit

    add [read_size], -1, [size]

    add mem, 0, [read_size]
    arb 3
    ret 0
.ENDFRAME

##########
print:
.FRAME byte, flag
    arb -2

    out 'L'
    out 'i'
    out 's'
    out 't'
    out ':'
    out 10

    jz  [size], print_finish

print_byte:
#+1 = print_size:
#    add [mem], '0', [rb + 0]
#    out [rb + 0]

+1 = print_size:
    add [mem], 0, [rb - 1]
    arb -1
    cal print_num

    eq  [size], [print_size], [rb + 1]
    jnz [rb + 1], print_finish
    add [print_size], 1, [print_size]

    out 44
    jz  0, print_byte

print_finish:
    out 10

    add -1, 0, [print_size]
    arb 2
    ret 0
.ENDFRAME

##########
# convert number to string
print_num:
.FRAME tmp, order, digit; num
    # also digit[] is on stack (overlaps next stack frame)
    arb -3

    # determine highest power of 10
    add 1, 0, [rb + 1]

print_num_next_order:
    mul [rb + 1], 10, [rb + 0]
    lt  [rb + 0], [rb + 4], [rb + 0]
    jz  [rb + 0], print_num_next_digit
    mul [rb + 1], 10, [rb + 1]
    jz  0, print_num_next_order

print_num_next_digit:
    out 'N'
    out [rb + 4]
    out 'O'
    out [rb + 1]
    out 10

#    add 0, 0, [rb + 2]
#
#print_num_next_order:
#    add [rb + 2], 1, [rb + 2]
#    mul [rb + 2], [rb + 1], [rb + 0]
#    lt  [rb + 0], [rb + 1], [rb + 0]
#    jnz [rb + 0], print_num_next_order
#
#+3 = print_num_digit_index:
#    add [rb + 2], -1, [rb - 1]
#    add [print_num_digit_index], -1, [print_num_digit_index]
#    j

    arb 3
    ret 1
.ENDFRAME

##########
# globals
size:
    db  0

##########
    ds  50, 0
stack:

mem:
