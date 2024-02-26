.EXPORT print_num
.EXPORT print_num_radix
.EXPORT print_str

##########
# convert number to string
print_num:
.FRAME num;
    arb -0

    add [rb + num], 0, [rb - 1]
    add 10, 0, [rb - 2]
    arb -2
    call print_num_radix

    arb 0
    ret 1
.ENDFRAME

##########
# convert number to string using a radix
print_num_radix:
.FRAME num, radix; tmp, order, digit; digits
    arb -3

    # determine highest power of radix
    add 1, 0, [rb + order]

    # handle sign if negative
    lt  [rb + num], 0, [rb + tmp]
    jz  [rb + tmp], print_num_radix_next_order
    out '-'
    mul [rb + num], -1, [rb + num]

print_num_radix_next_order:
+3 = print_num_radix_digit_ptr_1:
    add [rb + order], 0, [rb + digits]
    add [print_num_radix_digit_ptr_1], -1, [print_num_radix_digit_ptr_1]

    mul [rb + order], [rb + radix], [rb + order]
    lt  [rb + num], [rb + order], [rb + tmp]
    jz  [rb + tmp], print_num_radix_next_order

print_num_radix_finish_order:
    add [print_num_radix_digit_ptr_1], 1, [print_num_radix_digit_ptr_2]

print_num_radix_next_digit:
+1 = print_num_radix_digit_ptr_2:
    add [rb + digits], 0, [rb + order]
    add -1, 0, [rb + digit]

print_num_radix_increase:
    add [rb + digit], 1, [rb + digit]
    mul [rb + order], -1, [rb + tmp]
    add [rb + num], [rb + tmp], [rb + num]
    lt  [rb + num], 0, [rb + tmp]
    jz  [rb + tmp], print_num_radix_increase

    add [rb + num], [rb + order], [rb + num]
    add print_num_radix_digits, [rb + digit], [ip + 1]
    out [0]

    eq  [rb + order], 1, [rb + tmp]
    jnz [rb + tmp], print_num_radix_finish

    add [print_num_radix_digit_ptr_2], 1, [print_num_radix_digit_ptr_2]
    jz  0, print_num_radix_next_digit

print_num_radix_finish:
    add digits, 0, [print_num_radix_digit_ptr_2]
    add digits, 0, [print_num_radix_digit_ptr_1]

    arb 3
    ret 2

print_num_radix_digits:
    db  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
.ENDFRAME

##########
print_str:
.FRAME str; index, tmp, char
    arb -3

    add 0, 0, [rb + index]

print_str_loop:
    add [rb + str], [rb + index], [ip + 1]
    add [0], 0, [rb + char]

    jz  [rb + char], print_str_done

    out [rb + char]

    add [rb + index], 1, [rb + index]
    jz  0, print_str_loop

print_str_done:
    arb 3
    ret 1
.ENDFRAME

.EOF
