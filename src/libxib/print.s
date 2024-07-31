.EXPORT print_num
.EXPORT print_num_2
.EXPORT print_num_2_b
.EXPORT print_num_2_w
.EXPORT print_num_16
.EXPORT print_num_16_b
.EXPORT print_num_16_w
.EXPORT print_num_radix

.EXPORT print_str
.EXPORT print_str_as_mem

##########
# convert number to string
print_num:
.FRAME num;
    add [rb + num], 0, [rb - 1]
    add 10, 0, [rb - 2]
    add 0, 0, [rb - 3]
    arb -3
    call print_num_radix

    ret 1
.ENDFRAME

##########
# convert number to binary string
print_num_2:
.FRAME num;
    add [rb + num], 0, [rb - 1]
    add 2, 0, [rb - 2]
    add 0, 0, [rb - 3]
    arb -3
    call print_num_radix

    ret 1
.ENDFRAME

##########
# convert number to binary string, print 8 bits
print_num_2_b:
.FRAME num;
    add [rb + num], 0, [rb - 1]
    add 2, 0, [rb - 2]
    add 8, 0, [rb - 3]
    arb -3
    call print_num_radix

    ret 1
.ENDFRAME

##########
# convert number to binary string, print 16 bits
print_num_2_w:
.FRAME num;
    add [rb + num], 0, [rb - 1]
    add 2, 0, [rb - 2]
    add 16, 0, [rb - 3]
    arb -3
    call print_num_radix

    ret 1
.ENDFRAME

##########
# convert number to hexadecimal string
print_num_16:
.FRAME num;
    add [rb + num], 0, [rb - 1]
    add 16, 0, [rb - 2]
    add 0, 0, [rb - 3]
    arb -3
    call print_num_radix

    ret 1
.ENDFRAME

##########
# convert number to hexadecimal string, print 2 digits
print_num_16_b:
.FRAME num;
    add [rb + num], 0, [rb - 1]
    add 16, 0, [rb - 2]
    add 2, 0, [rb - 3]
    arb -3
    call print_num_radix

    ret 1
.ENDFRAME

##########
# convert number to hexadecimal string, print 4 digits
print_num_16_w:
.FRAME num;
    add [rb + num], 0, [rb - 1]
    add 16, 0, [rb - 2]
    add 4, 0, [rb - 3]
    arb -3
    call print_num_radix

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
    jz  [rb + tmp], .next_order
    out '-'
    mul [rb + num], -1, [rb + num]

.next_order:
+3 = .digit_ptr_1:
    add [rb + order], 0, [rb + digits]
    add [.digit_ptr_1], -1, [.digit_ptr_1]

    mul [rb + order], [rb + radix], [rb + order]
    lt  [rb + num], [rb + order], [rb + tmp]
    jz  [rb + tmp], .next_order

.finish_order:
    add [.digit_ptr_1], 1, [.digit_ptr_2]

    # Calculate padding amount
    mul digits, -1, [rb + tmp]
    add [.digit_ptr_1], [rb + tmp], [rb + tmp]
    add [rb + width], [rb + tmp], [rb + width]

.padding_loop:
    lt  0, [rb + width], [rb + tmp]
    jz [rb + tmp], .next_digit

    add [rb + width], -1, [rb + width]
    out '0'

    jz  0, .padding_loop

.next_digit:
+1 = .digit_ptr_2:
    add [rb + digits], 0, [rb + order]
    add -1, 0, [rb + digit]

.increase:
    add [rb + digit], 1, [rb + digit]
    mul [rb + order], -1, [rb + tmp]
    add [rb + num], [rb + tmp], [rb + num]
    lt  [rb + num], 0, [rb + tmp]
    jz  [rb + tmp], .increase

    add [rb + num], [rb + order], [rb + num]
    add .digits, [rb + digit], [ip + 1]
    out [0]

    eq  [rb + order], 1, [rb + tmp]
    jnz [rb + tmp], .finish

    add [.digit_ptr_2], 1, [.digit_ptr_2]
    jz  0, .next_digit

.finish:
    add digits, 0, [.digit_ptr_2]
    add digits, 0, [.digit_ptr_1]

    arb 3
    ret 3

.digits:
    db  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
.ENDFRAME

##########
print_str:
.FRAME str; index, tmp, char
    arb -3

    add 0, 0, [rb + index]
    jz  0, .start

.loop:
    out [rb + char]
    add [rb + index], 1, [rb + index]

.start:
    add [rb + str], [rb + index], [ip + 1]
    add [0], 0, [rb + char]

    jnz [rb + char], .loop

    arb 3
    ret 1
.ENDFRAME

##########
print_str_as_mem:
.FRAME str; index, tmp, char
    arb -3

    add 0, 0, [rb + index]

.loop:
    add [rb + str], [rb + index], [ip + 1]
    add [0], 0, [rb + char]

    jz  [rb + char], .done

    jz  [rb + index], .skip_comma
    out ','

.skip_comma:
    add [rb + char], 0, [rb - 1]
    add 10, 0, [rb - 2]
    add 0, 0, [rb - 3]
    arb -3
    call print_num_radix

    add [rb + index], 1, [rb + index]
    jz  0, .loop

.done:
    arb 3
    ret 1
.ENDFRAME

.EOF
