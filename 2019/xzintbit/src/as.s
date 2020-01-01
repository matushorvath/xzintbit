    arb stack

##########
main:
.FRAME
    #out '?'
    #out 10

main_loop:
    cal get_token
    out [rb - 2]
    out 10
    jz  0, main_loop

    #cal print

    hlt
.ENDFRAME

##########
# A add; T arb; C cal; B db; S ds; E eq; H hlt; I in;
# J jnz; Z jz; L lt; M mul; O out; P rb; R ret
# F .FRAME; D .ENDFRAME; c ,; $ EOL; + - = : ; [ ]
# n [0-9]+ i [a-zA-Z_][a-zA-Z0-9_]* s ' '
get_token:
.FRAME tmp, char
    arb -2

get_token_loop:
    in  [rb + char]

    # skip whitespace
    eq  [rb + char], ' ', [rb + tmp]
    jnz [rb + tmp], get_token_loop
    eq  [rb + char], 7, [rb + tmp]
    jnz [rb + tmp], get_token_loop

    # comments
    eq  [rb + char], 35, [rb + tmp]
    jnz [rb + tmp], get_token_eat_comment

    eq  [rb + char], 10, [rb + tmp]
    jnz [rb + tmp], get_token_eol

    # keywords
    eq  [rb + char], 'a', [rb + tmp]
    jnz [rb + tmp], get_token_a
    eq  [rb + char], 'c', [rb + tmp]
    jnz [rb + tmp], get_token_c
    eq  [rb + char], 'd', [rb + tmp]
    jnz [rb + tmp], get_token_d
    eq  [rb + char], 'e', [rb + tmp]
    jnz [rb + tmp], get_token_e
    eq  [rb + char], 'h', [rb + tmp]
    jnz [rb + tmp], get_token_h
    eq  [rb + char], 'i', [rb + tmp]
    jnz [rb + tmp], get_token_i
    eq  [rb + char], 'j', [rb + tmp]
    jnz [rb + tmp], get_token_j
    eq  [rb + char], 'l', [rb + tmp]
    jnz [rb + tmp], get_token_l
    eq  [rb + char], 'm', [rb + tmp]
    jnz [rb + tmp], get_token_m
    eq  [rb + char], 'o', [rb + tmp]
    jnz [rb + tmp], get_token_o
    eq  [rb + char], 'r', [rb + tmp]
    jnz [rb + tmp], get_token_r

    # directives
    eq  [rb + char], '.', [rb + tmp]
    jnz [rb + tmp], get_token_dir

    # symbols
    eq  [rb + char], '+', [rb + tmp]
    jnz [rb + tmp], get_token_plus
    eq  [rb + char], '-', [rb + tmp]
    jnz [rb + tmp], get_token_minus
    eq  [rb + char], '=', [rb + tmp]
    jnz [rb + tmp], get_token_equal
    eq  [rb + char], ':', [rb + tmp]
    jnz [rb + tmp], get_token_colon
    eq  [rb + char], ',', [rb + tmp]
    jnz [rb + tmp], get_token_comma
    eq  [rb + char], ';', [rb + tmp]
    jnz [rb + tmp], get_token_semicolon
    eq  [rb + char], '[', [rb + tmp]
    jnz [rb + tmp], get_token_open_bracket
    eq  [rb + char], ']', [rb + tmp]
    jnz [rb + tmp], get_token_close_bracket

    # strings
    eq  [rb + char], ''', [rb + tmp]
    jnz [rb + tmp], get_token_string

    # numbers and identifiers
    lt  '9', [rb + char], [rb + tmp]                        # if char > 9, not a digit
    jnz [rb + tmp], get_token_try_identifier
    lt  [rb + char], '0', [rb + tmp]                        # if char < 0, not a digit
    jz  [rb + tmp], get_token_number

get_token_try_identifier:
    lt  'z', [rb + char], [rb + tmp]                        # if char > z, not lowercase
    jnz [rb + tmp], get_token_not_lowercase
    lt  [rb + char], 'a', [rb + tmp]                        # if char < a, not lowercase
    jz  [rb + tmp], get_token_identifier

get_token_not_lowercase:
    lt  'Z', [rb + char], [rb + tmp]                        # if char > Z, not uppercase
    jnz [rb + tmp], get_token_not_uppercase
    lt  [rb + char], 'A', [rb + tmp]                        # if char < A, not uppercase
    jz  [rb + tmp], get_token_identifier

get_token_not_uppercase:
    eq  [rb + char], '_', [rb + tmp]
    jnz  [rb + tmp], get_token_identifier

    hlt

get_token_a:
    in  [rb + char]

    eq  [rb + char], 'd', [rb + tmp]
    jnz [rb + tmp], get_token_ad
    eq  [rb + char], 'r', [rb + tmp]
    jnz [rb + tmp], get_token_ar

    jz  0, get_token_try_identifier

get_token_ad:
    in  [rb + char]

    eq  [rb + char], 'd', [rb + tmp]
    jnz [rb + tmp], get_token_add

    # TODO store previous chars
    jz  0, get_token_try_identifier

get_token_add:
    add 'A', 0, [rb + tmp]
    arb 2
    ret 0

get_token_ar:
    in  [rb + char]

    eq  [rb + char], 'b', [rb + tmp]
    jnz [rb + tmp], get_token_arb

    # TODO store previous chars
    jz  0, get_token_try_identifier

get_token_arb:
    add 'T', 0, [rb + tmp]
    arb 2
    ret 0

get_token_c:
    in  [rb + char]

    eq  [rb + char], 'a', [rb + tmp]
    jnz [rb + tmp], get_token_ca

    # TODO store previous chars
    jz  0, get_token_try_identifier

get_token_ca:
    in  [rb + char]

    eq  [rb + char], 'l', [rb + tmp]
    jnz [rb + tmp], get_token_cal

    # TODO store previous chars
    jz  0, get_token_try_identifier

get_token_cal:
    add 'C', 0, [rb + tmp]
    arb 2
    ret 0

get_token_d:
    in  [rb + char]

    eq  [rb + char], 'b', [rb + tmp]
    jnz [rb + tmp], get_token_db
    eq  [rb + char], 's', [rb + tmp]
    jnz [rb + tmp], get_token_ds

    # TODO store previous chars
    jz  0, get_token_try_identifier

get_token_db:
    add 'B', 0, [rb + tmp]
    arb 2
    ret 0

get_token_ds:
    add 'S', 0, [rb + tmp]
    arb 2
    ret 0

get_token_e:
    in  [rb + char]

    eq  [rb + char], 'q', [rb + tmp]
    jnz [rb + tmp], get_token_eq

    # TODO store previous chars
    jz  0, get_token_try_identifier

get_token_eq:
    add 'E', 0, [rb + tmp]
    arb 2
    ret 0

get_token_h:
    in  [rb + char]

    eq  [rb + char], 'l', [rb + tmp]
    jnz [rb + tmp], get_token_hl

    # TODO store previous chars
    jz  0, get_token_try_identifier

get_token_hl:
    in  [rb + char]

    eq  [rb + char], 't', [rb + tmp]
    jnz [rb + tmp], get_token_hlt

    # TODO store previous chars
    jz  0, get_token_try_identifier

get_token_hlt:
    add 'H', 0, [rb + tmp]
    arb 2
    ret 0

get_token_i:
    in  [rb + char]

    eq  [rb + char], 'n', [rb + tmp]
    jnz [rb + tmp], get_token_in

    # TODO store previous chars
    jz  0, get_token_try_identifier

get_token_in:
    add 'I', 0, [rb + tmp]
    arb 2
    ret 0

get_token_j:
    in  [rb + char]

    eq  [rb + char], 'n', [rb + tmp]
    jnz [rb + tmp], get_token_jn
    eq  [rb + char], 'z', [rb + tmp]
    jnz [rb + tmp], get_token_jz

    # TODO store previous chars
    jz  0, get_token_try_identifier

get_token_jn:
    in  [rb + char]

    eq  [rb + char], 'z', [rb + tmp]
    jnz [rb + tmp], get_token_jnz

    # TODO store previous chars
    jz  0, get_token_try_identifier

get_token_jnz:
    add 'J', 0, [rb + tmp]
    arb 2
    ret 0

get_token_jz:
    add 'Z', 0, [rb + tmp]
    arb 2
    ret 0

get_token_l:
    in  [rb + char]

    eq  [rb + char], 't', [rb + tmp]
    jnz [rb + tmp], get_token_lt

    # TODO store previous chars
    jz  0, get_token_try_identifier

get_token_lt:
    add 'L', 0, [rb + tmp]
    arb 2
    ret 0

get_token_m:
    in  [rb + char]

    eq  [rb + char], 'u', [rb + tmp]
    jnz [rb + tmp], get_token_mu

    # TODO store previous chars
    jz  0, get_token_try_identifier

get_token_mu:
    in  [rb + char]

    eq  [rb + char], 'l', [rb + tmp]
    jnz [rb + tmp], get_token_mul

    # TODO store previous chars
    jz  0, get_token_try_identifier

get_token_mul:
    add 'M', 0, [rb + tmp]
    arb 2
    ret 0

get_token_o:
    in  [rb + char]

    eq  [rb + char], 'u', [rb + tmp]
    jnz [rb + tmp], get_token_ou

    # TODO store previous chars
    jz  0, get_token_try_identifier

get_token_ou:
    in  [rb + char]

    eq  [rb + char], 't', [rb + tmp]
    jnz [rb + tmp], get_token_out

    # TODO store previous chars
    jz  0, get_token_try_identifier

get_token_out:
    add 'O', 0, [rb + tmp]
    arb 2
    ret 0

get_token_r:
    in  [rb + char]

    eq  [rb + char], 'b', [rb + tmp]
    jnz [rb + tmp], get_token_rb
    eq  [rb + char], 'e', [rb + tmp]
    jnz [rb + tmp], get_token_re

    # TODO store previous chars
    jz  0, get_token_try_identifier

get_token_rb:
    add 'P', 0, [rb + tmp]
    arb 2
    ret 0

get_token_re:
    in  [rb + char]

    eq  [rb + char], 't', [rb + tmp]
    jnz [rb + tmp], get_token_ret

    # TODO store previous chars
    jz  0, get_token_try_identifier

get_token_ret:
    add 'R', 0, [rb + tmp]
    arb 2
    ret 0

get_token_dir:
    in  [rb + char]

    eq  [rb + char], 'E', [rb + tmp]
    jnz [rb + tmp], get_token_dir_E
    eq  [rb + char], 'F', [rb + tmp]
    jnz [rb + tmp], get_token_dir_F

    hlt

get_token_dir_F:
    in  [rb + char]
    eq  [rb + char], 'R', [rb + tmp]
    jz [rb + tmp], get_token_dir_F_fail

    in  [rb + char]
    eq  [rb + char], 'A', [rb + tmp]
    jz [rb + tmp], get_token_dir_F_fail

    in  [rb + char]
    eq  [rb + char], 'M', [rb + tmp]
    jz [rb + tmp], get_token_dir_F_fail

    in  [rb + char]
    eq  [rb + char], 'E', [rb + tmp]
    jz [rb + tmp], get_token_dir_F_fail

    add 'F', 0, [rb + tmp]
    arb 2
    ret 0

get_token_dir_F_fail:
    hlt

get_token_dir_E:
    in  [rb + char]
    eq  [rb + char], 'N', [rb + tmp]
    jz [rb + tmp], get_token_dir_E_fail

    in  [rb + char]
    eq  [rb + char], 'D', [rb + tmp]
    jz [rb + tmp], get_token_dir_E_fail

    in  [rb + char]
    eq  [rb + char], 'F', [rb + tmp]
    jz [rb + tmp], get_token_dir_E_fail

    in  [rb + char]
    eq  [rb + char], 'R', [rb + tmp]
    jz [rb + tmp], get_token_dir_E_fail

    in  [rb + char]
    eq  [rb + char], 'A', [rb + tmp]
    jz [rb + tmp], get_token_dir_E_fail

    in  [rb + char]
    eq  [rb + char], 'M', [rb + tmp]
    jz [rb + tmp], get_token_dir_E_fail

    in  [rb + char]
    eq  [rb + char], 'E', [rb + tmp]
    jz [rb + tmp], get_token_dir_E_fail

    add 'D', 0, [rb + tmp]
    arb 2
    ret 0

get_token_dir_E_fail:
    hlt

get_token_eol:
    add '$', 0, [rb + tmp]
    arb 2
    ret 0

get_token_plus:
    add '+', 0, [rb + tmp]
    arb 2
    ret 0

get_token_minus:
    add '-', 0, [rb + tmp]
    arb 2
    ret 0

get_token_equal:
    add '=', 0, [rb + tmp]
    arb 2
    ret 0

get_token_colon:
    add ':', 0, [rb + tmp]
    arb 2
    ret 0

get_token_comma:
    add 'c', 0, [rb + tmp]
    arb 2
    ret 0

get_token_semicolon:
    add ';', 0, [rb + tmp]
    arb 2
    ret 0

get_token_open_bracket:
    add '[', 0, [rb + tmp]
    arb 2
    ret 0

get_token_close_bracket:
    add ']', 0, [rb + tmp]
    arb 2
    ret 0

get_token_eat_comment:
    # TODO eat the comment
    hlt

get_token_string:
    # TODO store the string
get_token_string_loop:
    in  [rb + char]
    eq  [rb + char], ''', [rb + tmp]
    jz  [rb + tmp], get_token_string_loop

    add 's', 0, [rb + tmp]
    arb 2
    ret 0

get_token_number:
    # TODO
    add 'n', 0, [rb + tmp]
    arb 2
    ret 0

get_token_identifier:
    # TODO
    add 'i', 0, [rb + tmp]
    arb 2
    ret 0

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
.FRAME num; tmp, order, digit; digits
    arb -3

    # determine highest power of 10
    add 1, 0, [rb + order]

print_num_next_order:
+3 = print_num_digit_ptr_1:
    add [rb + order], 0, [rb + digits]
    add [print_num_digit_ptr_1], -1, [print_num_digit_ptr_1]

    mul [rb + order], 10, [rb + order]
    lt  [rb + num], [rb + order], [rb + tmp]
    jz  [rb + tmp], print_num_next_order

print_num_finish_order:
    add [print_num_digit_ptr_1], 1, [print_num_digit_ptr_2]

print_num_next_digit:
+1 = print_num_digit_ptr_2:
    add [rb + digits], 0, [rb + order]
    add -1, 0, [rb + digit]

print_num_increase:
    add [rb + digit], 1, [rb + digit]
    mul [rb + order], -1, [rb + tmp]
    add [rb + num], [rb + tmp], [rb + num]
    lt  [rb + num], 0, [rb + tmp]
    jz  [rb + tmp], print_num_increase

    add [rb + num], [rb + order], [rb + num]
    add [rb + digit], '0', [rb + digit]
    out [rb + digit]

    eq  [rb + order], 1, [rb + tmp]
    jnz [rb + tmp], print_num_finish

    add [print_num_digit_ptr_2], 1, [print_num_digit_ptr_2]
    jz  0, print_num_next_digit

print_num_finish:
    add digits, 0, [print_num_digit_ptr_2]
    add digits, 0, [print_num_digit_ptr_1]

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
