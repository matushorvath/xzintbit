.EXPORT print_num
.EXPORT print_num_2
.EXPORT print_num_16
.EXPORT print_num_radix

.EXPORT print_str
.EXPORT print_str_as_mem

##########
# convert number to string
print_num:
.FRAME num;
    arb -0

    add [rb + num], 0, [rb - 1]
    add 10, 0, [rb - 2]
    add 0, 0, [rb - 3]
    arb -3
    call print_num_radix

    arb 0
    ret 1
.ENDFRAME

##########
# convert number to binary string
print_num_2:
.FRAME num;
    arb -0

    add [rb + num], 0, [rb - 1]
    add 2, 0, [rb - 2]
    add 0, 0, [rb - 3]
    arb -3
    call print_num_radix

    arb 0
    ret 1
.ENDFRAME

##########
# convert number to hexadecimal string
print_num_16:
.FRAME num;
    arb -0

    add [rb + num], 0, [rb - 1]
    add 16, 0, [rb - 2]
    add 0, 0, [rb - 3]
    arb -3
    call print_num_radix

    arb 0
    ret 1
.ENDFRAME

##########
# convert number to string using a radix
print_num_radix:
.FRAME num, radix, width; tmp, order, digit; digits
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

    # Calculate padding amount
    mul digits, -1, [rb + tmp]
    add [print_num_radix_digit_ptr_1], [rb + tmp], [rb + tmp]
    add [rb + width], [rb + tmp], [rb + width]

print_num_radix_padding_loop:
    lt  0, [rb + width], [rb + tmp]
    jz [rb + tmp], print_num_radix_next_digit

    add [rb + width], -1, [rb + width]
    out '0'

    jz  0, print_num_radix_padding_loop

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
    ret 3

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

##########
print_str_as_mem:
.FRAME str; index, tmp, char
    arb -3

    add 0, 0, [rb + index]

print_str_as_mem_loop:
    add [rb + str], [rb + index], [ip + 1]
    add [0], 0, [rb + char]

    jz  [rb + char], print_str_as_mem_done

    jz  [rb + index], print_str_as_mem_skip_comma
    out ','

print_str_as_mem_skip_comma:
    add [rb + char], 0, [rb - 1]
    add 10, 0, [rb - 2]
    add 0, 0, [rb - 3]
    arb -3
    call print_num_radix

    add [rb + index], 1, [rb + index]
    jz  0, print_str_as_mem_loop

print_str_as_mem_done:
    arb 3
    ret 1
.ENDFRAME

.EOF
