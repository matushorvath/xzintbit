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
    in  [rb + digit]
    eq  [rb + digit], 44, [rb + flag]
    jnz [rb + flag], read_finish_byte
    eq  [rb + digit], 10, [rb + flag]
    jnz [rb + flag], read_finish_byte
    add [rb + digit], -48, [rb + digit]
    mul [rb + byte], 10, [rb + byte]
    add [rb + byte], [rb + digit], [rb + byte]
    jz  0, read_next_digit

read_finish_byte:
+3 = read_size:
    add [rb + byte], 0, [mem]
    add 0, 0, [rb + byte]
    add [read_size], 1, [read_size]

    eq  [rb + digit], 10, [rb + flag]
    jz  [rb + flag], read_next_digit

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
#    add [mem], '0', [rb + byte]
#    out [rb + byte]

+1 = print_size:
    add [mem], 0, [rb - 1]
    arb -1
    cal print_num

    eq  [size], [print_size], [rb + flag]
    jnz [rb + flag], print_finish
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
.FRAME num; flag, order, digit
    # digit[] overlaps next stack frame
    arb -3

    # determine highest power of 10
    add 1, 0, [rb + order]

print_num_next_order:
+3 = print_num_digit_ptr:
    add [rb + order], 0, [rb + digit]
    add [print_num_digit_ptr], -1, [print_num_digit_ptr]

    mul [rb + order], 10, [rb + order]
    lt  [rb + order], [rb + num], [rb + flag]
    jnz [rb + flag], print_num_next_order

print_num_finish_order:
    out 'N'
    out [rb + num]
    out 'O'
    out [rb + order]
    out '0'
    out [rb + digit]
    out '1'
    out [rb + digit - 1]
    out '2'
    out [rb + digit - 2]
    out '3'
    out [rb + digit - 3]
    out '4'
    out [rb + digit - 4]
    out '5'
    out [rb + digit - 5]
    out 10

#    add 0, 0, [rb + digit]
#
#print_num_next_order:
#    add [rb + digit], 1, [rb + digit]
#    mul [rb + digit], [rb + order], [rb + flag]
#    lt  [rb + flag], [rb + order], [rb + flag]
#    jnz [rb + flag], print_num_next_order
#
#+3 = print_num_digit_index:
#    add [rb + digit], -1, [rb - 1]
#    add [print_num_digit_index], -1, [print_num_digit_index]
#    j

    add digit, 0, [print_num_digit_ptr]
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
