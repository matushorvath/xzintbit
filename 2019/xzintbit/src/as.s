    arb stack

##########
main:
.FRAME

main_loop:
    cal get_token
    out [rb - 2]
    out 10
    jz  0, main_loop

    hlt
.ENDFRAME

##########
get_input:
.FRAME tmp
    arb -1

    # we need to be able to "unget" an unused char and then get it again later
    # to unget a char, we just use one instruction: add [rb + char], 0, [get_input_buffer]

    jnz [get_input_buffer], get_input_from_buffer
    in  [rb + tmp]
    arb 1
    ret 0

get_input_from_buffer:
    add [get_input_buffer], 0, [rb + tmp]
    add 0, 0, [get_input_buffer]
    arb 1
    ret 0

get_input_buffer:
    db  0
.ENDFRAME

##########
get_token:
.FRAME tmp, char
    arb -2

# A add; T arb; C cal; B db; S ds; E eq; H hlt; I in;
# J jnz; Z jz; L lt; M mul; O out; P rb; R ret
# F .FRAME; D .ENDFRAME; c ,; $ EOL; + - = : ; [ ]
# n [0-9]+ i [a-zA-Z_][a-zA-Z0-9_]* s ' '

get_token_loop:
    cal get_input
    add [rb - 2], 0, [rb + char]

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

    # numbers
    add [rb + char], 0, [rb - 1]
    arb -1
    cal is_digit
    jnz [rb - 3], get_token_number

    # identifiers
    eq  [rb + char], '_', [rb + tmp]
    jnz [rb + tmp], get_token_identifier

    add [rb + char], 0, [rb - 1]
    arb -1
    cal is_alpha
    jnz [rb - 3], get_token_identifier

    hlt

get_token_a:
    cal get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 'd', [rb + tmp]
    jnz [rb + tmp], get_token_ad
    eq  [rb + char], 'r', [rb + tmp]
    jnz [rb + tmp], get_token_ar

    jz  0, get_token_identifier

get_token_ad:
    cal get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 'd', [rb + tmp]
    jnz [rb + tmp], get_token_add

    # TODO store previous chars
    jz  0, get_token_identifier

get_token_add:
    add 'A', 0, [rb + tmp]
    arb 2
    ret 0

get_token_ar:
    cal get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 'b', [rb + tmp]
    jnz [rb + tmp], get_token_arb

    # TODO store previous chars
    jz  0, get_token_identifier

get_token_arb:
    add 'T', 0, [rb + tmp]
    arb 2
    ret 0

get_token_c:
    cal get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 'a', [rb + tmp]
    jnz [rb + tmp], get_token_ca

    # TODO store previous chars
    jz  0, get_token_identifier

get_token_ca:
    cal get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 'l', [rb + tmp]
    jnz [rb + tmp], get_token_cal

    # TODO store previous chars
    jz  0, get_token_identifier

get_token_cal:
    add 'C', 0, [rb + tmp]
    arb 2
    ret 0

get_token_d:
    cal get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 'b', [rb + tmp]
    jnz [rb + tmp], get_token_db
    eq  [rb + char], 's', [rb + tmp]
    jnz [rb + tmp], get_token_ds

    # TODO store previous chars
    jz  0, get_token_identifier

get_token_db:
    add 'B', 0, [rb + tmp]
    arb 2
    ret 0

get_token_ds:
    add 'S', 0, [rb + tmp]
    arb 2
    ret 0

get_token_e:
    cal get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 'q', [rb + tmp]
    jnz [rb + tmp], get_token_eq

    # TODO store previous chars
    jz  0, get_token_identifier

get_token_eq:
    add 'E', 0, [rb + tmp]
    arb 2
    ret 0

get_token_h:
    cal get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 'l', [rb + tmp]
    jnz [rb + tmp], get_token_hl

    # TODO store previous chars
    jz  0, get_token_identifier

get_token_hl:
    cal get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 't', [rb + tmp]
    jnz [rb + tmp], get_token_hlt

    # TODO store previous chars
    jz  0, get_token_identifier

get_token_hlt:
    add 'H', 0, [rb + tmp]
    arb 2
    ret 0

get_token_i:
    cal get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 'n', [rb + tmp]
    jnz [rb + tmp], get_token_in

    # TODO store previous chars
    jz  0, get_token_identifier

get_token_in:
    add 'I', 0, [rb + tmp]
    arb 2
    ret 0

get_token_j:
    cal get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 'n', [rb + tmp]
    jnz [rb + tmp], get_token_jn
    eq  [rb + char], 'z', [rb + tmp]
    jnz [rb + tmp], get_token_jz

    # TODO store previous chars
    jz  0, get_token_identifier

get_token_jn:
    cal get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 'z', [rb + tmp]
    jnz [rb + tmp], get_token_jnz

    # TODO store previous chars
    jz  0, get_token_identifier

get_token_jnz:
    add 'J', 0, [rb + tmp]
    arb 2
    ret 0

get_token_jz:
    add 'Z', 0, [rb + tmp]
    arb 2
    ret 0

get_token_l:
    cal get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 't', [rb + tmp]
    jnz [rb + tmp], get_token_lt

    # TODO store previous chars
    jz  0, get_token_identifier

get_token_lt:
    add 'L', 0, [rb + tmp]
    arb 2
    ret 0

get_token_m:
    cal get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 'u', [rb + tmp]
    jnz [rb + tmp], get_token_mu

    # TODO store previous chars
    jz  0, get_token_identifier

get_token_mu:
    cal get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 'l', [rb + tmp]
    jnz [rb + tmp], get_token_mul

    # TODO store previous chars
    jz  0, get_token_identifier

get_token_mul:
    add 'M', 0, [rb + tmp]
    arb 2
    ret 0

get_token_o:
    cal get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 'u', [rb + tmp]
    jnz [rb + tmp], get_token_ou

    # TODO store previous chars
    jz  0, get_token_identifier

get_token_ou:
    cal get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 't', [rb + tmp]
    jnz [rb + tmp], get_token_out

    # TODO store previous chars
    jz  0, get_token_identifier

get_token_out:
    add 'O', 0, [rb + tmp]
    arb 2
    ret 0

get_token_r:
    cal get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 'b', [rb + tmp]
    jnz [rb + tmp], get_token_rb
    eq  [rb + char], 'e', [rb + tmp]
    jnz [rb + tmp], get_token_re

    # TODO store previous chars
    jz  0, get_token_identifier

get_token_rb:
    add 'P', 0, [rb + tmp]
    arb 2
    ret 0

get_token_re:
    cal get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 't', [rb + tmp]
    jnz [rb + tmp], get_token_ret

    # TODO store previous chars
    jz  0, get_token_identifier

get_token_ret:
    add 'R', 0, [rb + tmp]
    arb 2
    ret 0

get_token_dir:
    cal get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], 'E', [rb + tmp]
    jnz [rb + tmp], get_token_dir_E
    eq  [rb + char], 'F', [rb + tmp]
    jnz [rb + tmp], get_token_dir_F

    hlt

get_token_dir_F:
    cal get_input
    add [rb - 2], 0, [rb + char]
    eq  [rb + char], 'R', [rb + tmp]
    jz  [rb + tmp], get_token_dir_F_fail

    cal get_input
    add [rb - 2], 0, [rb + char]
    eq  [rb + char], 'A', [rb + tmp]
    jz  [rb + tmp], get_token_dir_F_fail

    cal get_input
    add [rb - 2], 0, [rb + char]
    eq  [rb + char], 'M', [rb + tmp]
    jz  [rb + tmp], get_token_dir_F_fail

    cal get_input
    add [rb - 2], 0, [rb + char]
    eq  [rb + char], 'E', [rb + tmp]
    jz  [rb + tmp], get_token_dir_F_fail

    add 'F', 0, [rb + tmp]
    arb 2
    ret 0

get_token_dir_F_fail:
    hlt

get_token_dir_E:
    cal get_input
    add [rb - 2], 0, [rb + char]
    eq  [rb + char], 'N', [rb + tmp]
    jz  [rb + tmp], get_token_dir_E_fail

    cal get_input
    add [rb - 2], 0, [rb + char]
    eq  [rb + char], 'D', [rb + tmp]
    jz  [rb + tmp], get_token_dir_E_fail

    cal get_input
    add [rb - 2], 0, [rb + char]
    eq  [rb + char], 'F', [rb + tmp]
    jz  [rb + tmp], get_token_dir_E_fail

    cal get_input
    add [rb - 2], 0, [rb + char]
    eq  [rb + char], 'R', [rb + tmp]
    jz  [rb + tmp], get_token_dir_E_fail

    cal get_input
    add [rb - 2], 0, [rb + char]
    eq  [rb + char], 'A', [rb + tmp]
    jz  [rb + tmp], get_token_dir_E_fail

    cal get_input
    add [rb - 2], 0, [rb + char]
    eq  [rb + char], 'M', [rb + tmp]
    jz  [rb + tmp], get_token_dir_E_fail

    cal get_input
    add [rb - 2], 0, [rb + char]
    eq  [rb + char], 'E', [rb + tmp]
    jz  [rb + tmp], get_token_dir_E_fail

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
    cal get_input
    add [rb - 2], 0, [rb + char]
    eq  [rb + char], ''', [rb + tmp]
    jz  [rb + tmp], get_token_string_loop

    add 's', 0, [rb + tmp]
    arb 2
    ret 0

get_token_number:
    # first digit of the number was already processed
    # TODO store the number

get_token_number_loop:
    cal get_input
    add [rb - 2], 0, [rb + char]

    add [rb + char], 0, [rb - 1]
    arb -1
    cal is_digit
    jnz [rb - 3], get_token_number_loop

    # unget last char
    add [rb + char], 0, [get_input_buffer]

    add 'n', 0, [rb + tmp]
    arb 2
    ret 0

get_token_identifier:
    # at least one character of the identifier was already processed
    # TODO store the identifier

get_token_identifier_loop:
    cal get_input
    add [rb - 2], 0, [rb + char]

    add [rb + char], 0, [rb - 1]
    arb -1
    cal is_alphanum
    jnz [rb - 3], get_token_identifier_loop

    # unget last char
    add [rb + char], 0, [get_input_buffer]

    add 'i', 0, [rb + tmp]
    arb 2
    ret 0
.ENDFRAME

##########
is_digit:
.FRAME char; tmp
    arb -1

    # check if 0 <= char <= 9
    lt  '9', [rb + char], [rb + tmp]                        # if char > 9, not a digit
    jnz [rb + tmp], is_digit_end
    lt  [rb + char], '0', [rb + tmp]                        # if char < 0, not a digit

is_digit_end:
    # the result is a logical negation of [rb + tmp]
    eq  [rb + tmp], 0, [rb + tmp]

    arb 1
    ret 1
.ENDFRAME

##########
is_alpha:
.FRAME char; tmp
    arb -1

    # check if a <= char <= z
    lt  'z', [rb + char], [rb + tmp]                        # if char > z, not a letter
    jnz [rb + tmp], is_alpha_end
    lt  [rb + char], 'a', [rb + tmp]                        # if !(char < a), is a letter
    jz  [rb + tmp], is_alpha_end

    # check if A <= char <= Z
    lt  'Z', [rb + char], [rb + tmp]                        # if char > Z, not a letter
    jnz [rb + tmp], is_alpha_end
    lt  [rb + char], 'A', [rb + tmp]                        # if !(char < A), is a letter

is_alpha_end:
    # the result is a logical negation of [rb + tmp]
    eq  [rb + tmp], 0, [rb + tmp]

    arb 1
    ret 1
.ENDFRAME

##########
is_alphanum:
.FRAME char; tmp
    arb -1

    add [rb + char], 0, [rb - 1]
    arb -1
    cal is_alpha
    add [rb - 3], 0, [rb + tmp]
    jnz [rb + tmp], is_alphanum_end

    add [rb + char], 0, [rb - 1]
    arb -1
    cal is_digit
    add [rb - 3], 0, [rb + tmp]
    jnz [rb + tmp], is_alphanum_end

    eq  [rb + char], '_', [rb + tmp]

is_alphanum_end:
    arb 1
    ret 1
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
