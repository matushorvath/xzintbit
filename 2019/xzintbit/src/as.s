    arb stack

# token types:
# 1 add; 9 arb; 8 eq; 99 hlt; 3 in; 5 jnz; 6 jz; 7 lt; 2 mul; 4 out
# C cal; R ret; B db; S ds
# F .FRAME; D .ENDFRAME; N EOF; $ EOL; P rb
# + - = , : ; [ ]
# n [0-9]+ i [a-zA-Z_][a-zA-Z0-9_]* c '.'

##########
parse:
.FRAME tmp
    arb -1

parse_loop:
    cal get_token

    # skip empty lines
    eq  [token], '$', [rb + tmp]
    jnz [rb + tmp], parse_loop

    # instructions
    eq  [token], 1, [rb + tmp]
    jnz [rb + tmp], parse_call_add_mul_lt_eq
    eq  [token], 2, [rb + tmp]
    jnz [rb + tmp], parse_call_add_mul_lt_eq
    eq  [token], 7, [rb + tmp]
    jnz [rb + tmp], parse_call_add_mul_lt_eq
    eq  [token], 8, [rb + tmp]
    jnz [rb + tmp], parse_call_add_mul_lt_eq

    eq  [token], 5, [rb + tmp]
    jnz [rb + tmp], parse_call_jnz_jz
    eq  [token], 6, [rb + tmp]
    jnz [rb + tmp], parse_call_jnz_jz

    eq  [token], 9, [rb + tmp]
    jnz [rb + tmp], parse_call_arb_out
    eq  [token], 4, [rb + tmp]
    jnz [rb + tmp], parse_call_arb_out

    eq  [token], 3, [rb + tmp]
    jnz [rb + tmp], parse_call_in
    eq  [token], 99, [rb + tmp]
    jnz [rb + tmp], parse_call_hlt

    # pseudo-instructions
    eq  [token], 'C', [rb + tmp]
    jnz [rb + tmp], parse_call_cal
    eq  [token], 'R', [rb + tmp]
    jnz [rb + tmp], parse_call_ret
    eq  [token], 'B', [rb + tmp]
    jnz [rb + tmp], parse_call_db
    eq  [token], 'S', [rb + tmp]
    jnz [rb + tmp], parse_call_ds

    # symbols
    eq  [token], 'i', [rb + tmp]
    jnz [rb + tmp], parse_call_symbol
    eq  [token], '+', [rb + tmp]
    jnz [rb + tmp], parse_call_symbol

    # directives
    eq  [token], 'F', [rb + tmp]
    jnz [rb + tmp], parse_call_directive_frame
    eq  [token], 'E', [rb + tmp]
    jnz [rb + tmp], parse_call_directive_endframe
    eq  [token], 'N', [rb + tmp]
    jnz [rb + tmp], parse_call_directive_eof

    cal parse_error

parse_call_add_mul_lt_eq:
    cal parse_add_mul_lt_eq
    jz  0, parse_loop

parse_call_jnz_jz:
    cal parse_jnz_jz
    jz  0, parse_loop

parse_call_arb_out:
    cal parse_arb_out
    jz  0, parse_loop

parse_call_in:
    cal parse_in
    jz  0, parse_loop

parse_call_hlt:
    cal parse_hlt
    jz  0, parse_loop

parse_call_cal:
    cal parse_cal
    jz  0, parse_loop

parse_call_ret:
    cal parse_ret
    jz  0, parse_loop

parse_call_db:
    cal parse_db
    jz  0, parse_loop

parse_call_ds:
    cal parse_ds
    jz  0, parse_loop

parse_call_symbol:
    cal parse_symbol
    jz  0, parse_loop

parse_call_directive_frame:
    cal parse_directive_frame
    jz  0, parse_loop

parse_call_directive_endframe:
    cal parse_directive_endframe
    jz  0, parse_loop

parse_call_directive_eof:
    cal parse_directive_eof
    jz  0, parse_loop
.ENDFRAME

##########
parse_error:
.FRAME
    # TODO use strings once available
    out 'P'
    out 'a'
    out 'r'
    out 's'
    out 'e'
    out ' '
    out 'e'
    out 'r'
    out 'r'
    out 'o'
    out 'r'
    out 10

    hlt
.ENDFRAME

##########
parse_add_mul_lt_eq:
.FRAME tmp, op, param0, param1, param2
    arb -5

    # token type is conveniently also the op code
    add [token], 0, [rb + op]
    cal get_token

    add [ip], 1, [ip]
    cal parse_in_param

    # update opcode and param 0 value
    mul [rb - 3], 100, [rb - 3]
    add [rb + op], [rb - 3], [rb + op]
    add [rb - 2], 0, [rb + param0]

    eq  [token], ',', [rb + tmp]
    cal get_token
    jnz [rb + tmp], parse_add_mul_lt_eq_param1
    cal parse_error

parse_add_mul_lt_eq_param1:
    add [ip], 1, [ip]
    cal parse_in_param

    # update opcode and param 1 value
    mul [rb - 3], 1000, [rb - 3]
    add [rb + op], [rb - 3], [rb + op]
    add [rb - 2], 0, [rb + param1]

    eq  [token], ',', [rb + tmp]
    cal get_token
    jnz [rb + tmp], parse_add_mul_lt_eq_param2
    cal parse_error

parse_add_mul_lt_eq_param2:
    add [ip], 1, [ip]
    cal parse_out_param

    # update opcode and param 2 value
    mul [rb - 3], 10000, [rb - 3]
    add [rb + op], [rb - 3], [rb + op]
    add [rb - 2], 0, [rb + param2]

    eq  [token], '$', [rb + tmp]
    jnz [rb + tmp], parse_add_mul_lt_eq_done
    cal parse_error

parse_add_mul_lt_eq_done:
    add [ip], 1, [ip]

    out [rb + op]
    out [rb + param0]
    out [rb + param1]
    out [rb + param2]
    out 10

    arb 5
    ret 0
.ENDFRAME

##########
parse_jnz_jz:
.FRAME tmp
    arb -1

    arb 1
    ret 0
.ENDFRAME

##########
parse_arb_out:
.FRAME tmp
    arb -1

    arb 1
    ret 0
.ENDFRAME

##########
parse_in:
.FRAME tmp
    arb -1

    arb 1
    ret 0
.ENDFRAME

##########
parse_hlt:
.FRAME tmp
    arb -1

    arb 1
    ret 0
.ENDFRAME

##########
parse_cal:
.FRAME tmp
    arb -1

    arb 1
    ret 0
.ENDFRAME

##########
parse_ret:
.FRAME tmp
    arb -1

    arb 1
    ret 0
.ENDFRAME

##########
parse_db:
.FRAME tmp
    arb -1

    arb 1
    ret 0
.ENDFRAME

##########
parse_ds:
.FRAME tmp
    arb -1

    arb 1
    ret 0
.ENDFRAME

##########
parse_symbol:
.FRAME tmp
    arb -1

    arb 1
    ret 0
.ENDFRAME

##########
parse_directive_frame:
.FRAME tmp
    arb -1

    arb 1
    ret 0
.ENDFRAME

##########
parse_directive_endframe:
.FRAME tmp
    arb -1

    arb 1
    ret 0
.ENDFRAME

##########
parse_directive_eof:
.FRAME tmp
    arb -1

    arb 1
    ret 0
.ENDFRAME

##########
parse_out_param:
.FRAME result, mode, sign, tmp
    arb -4

    # default is position mode, unless we see a 'rb'
    add 0, 0, [rb + mode]

    eq  [token], '[', [rb + tmp]
    cal get_token
    jnz [rb + tmp], parse_out_param_try_rb
    cal parse_error

parse_out_param_try_rb:
    eq  [token], 'P', [rb + tmp]
    jz  [rb + tmp], parse_out_param_after_rb
    cal get_token

    eq  [token], '+', [rb + tmp]
    jnz [rb + tmp], parse_out_param_rb_plus
    eq  [token], '-', [rb + tmp]
    jnz [rb + tmp], parse_out_param_rb_minus

    cal parse_error

parse_out_param_rb_plus:
    # relative mode, sign is +
    add 2, 0, [rb + mode]
    add 1, 0, [rb + sign]

    cal get_token
    jz  0, parse_out_param_after_rb

parse_out_param_rb_minus:
    # relative mode, sign is -
    add 2, 0, [rb + mode]
    add -1, 0, [rb + sign]

    cal get_token
    jz  0, parse_out_param_after_rb

parse_out_param_after_rb:
    cal parse_value
    mul [rb - 2], [rb + sign], [rb + result]

    # we don't support 'rb - symbol', the fixup is always positive
    eq  [rb - 3], 1, [rb + tmp]
    jz  [rb + tmp], parse_out_param_skip_neg_symbol_check

    eq  [rb + sign], -1, [rb + tmp]
    jnz [rb + tmp], parse_error

parse_out_param_skip_neg_symbol_check:
    eq  [token], ']', [rb + tmp]
    cal get_token
    jnz [rb + tmp], parse_out_param_done
    cal parse_error

parse_out_param_done:
    arb 4
    ret 0
.ENDFRAME

##########
parse_in_param:
.FRAME result, mode, tmp
    arb -3

    # position and relative are handled same as out_param
    eq  [token], '[', [rb + tmp]
    jz  [rb + tmp], parse_in_param_immediate

    cal parse_out_param
    add [rb - 2], 0, [rb + result]
    add [rb - 3], 0, [rb + mode]
    jz  0, parse_in_param_done

parse_in_param_immediate:
    cal parse_value

    # return the value and immediate mode
    add [rb - 2], 0, [rb + result]
    add 1, 0, [rb + mode]

parse_in_param_done:
    arb 3
    ret 0
.ENDFRAME

##########
parse_value:
.FRAME result, has_symbol, sign, tmp
    arb -4

    add 0, 0, [rb + has_symbol]

    eq  [token], 'n', [rb + tmp]
    jnz [rb + tmp], parse_value_number_or_char_1
    eq  [token], 'c', [rb + tmp]
    jnz [rb + tmp], parse_value_number_or_char_1
    eq  [token], 'i', [rb + tmp]
    jnz [rb + tmp], parse_value_identifier

    cal parse_error

parse_value_number_or_char_1:
    # return the number/char value
    add [value], 0, [rb + result]

    cal get_token
    jz  0, parse_value_done

parse_value_identifier:
    # TODO support frame symbols, resolve them immediately
    add 1, 0, [rb + has_symbol]

    # add a fixup for this identifier
    add [value], 0, [rb - 1]
    add [ip], 0, [rb - 2]
    arb -2
    cal add_fixup

    # optionally followed by + or - and a number or char
    cal get_token
    eq  [token], '+', [rb + tmp]
    jnz [rb + tmp], parse_value_identifier_plus
    eq  [token], '-', [rb + tmp]
    jnz [rb + tmp], parse_value_identifier_minus
    jz  0, parse_value_done

parse_value_identifier_plus:
    add 1, 0, [rb + sign]
    cal get_token
    jz  0, parse_value_identifier_after_sign

parse_value_identifier_minus:
    add -1, 0, [rb + sign]
    cal get_token
    jz  0, parse_value_identifier_after_sign

parse_value_identifier_after_sign:
    eq  [token], 'n', [rb + tmp]
    jnz [rb + tmp], parse_value_number_or_char_2
    eq  [token], 'c', [rb + tmp]
    jnz [rb + tmp], parse_value_number_or_char_2
    cal parse_error

parse_value_number_or_char_2:
    # return the number/char value
    mul [value], [rb + sign], [rb + result]

    cal get_token
    jz  0, parse_value_done

parse_value_done:
    arb 4
    ret 0
.ENDFRAME

##########
dump_token:
.FRAME token, data; tmp
    arb -1

    # print token type
    out [rb + token]

    # print token value if relevant
    eq  [rb + token], 'n', [rb + tmp]
    jnz [rb + tmp], dump_token_print_n
    eq  [rb + token], 'c', [rb + tmp]
    jnz [rb + tmp], dump_token_print_c
    eq  [rb + token], 'i', [rb + tmp]
    jnz [rb + tmp], dump_token_print_i
    jz  0, dump_token_finish

dump_token_print_n:
    out ' '
    add [rb + data], 0, [rb - 1]
    arb -1
    cal print_num
    jz  0, dump_token_finish

dump_token_print_c:
    out ' '
    out [rb - 3]
    jz  0, dump_token_finish

dump_token_print_i:
    out ' '
    add [rb + data], 0, [rb - 1]
    arb -1
    cal print_str
    jz  0, dump_token_finish

dump_token_finish:
    out 10

    arb 1
    ret 2
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

    # end of line
    eq  [rb + char], 10, [rb + tmp]
    jnz [rb + tmp], get_token_eol

    # identifiers and keywords
    eq  [rb + char], '_', [rb + tmp]
    jnz [rb + tmp], get_token_identifier

    add [rb + char], 0, [rb - 1]
    arb -1
    cal is_alpha
    jnz [rb - 3], get_token_identifier

    # directives
    eq  [rb + char], '.', [rb + tmp]
    jnz [rb + tmp], get_token_directive

    # symbols
    add [rb + char], 0, [rb - 1]
    arb -1
    cal is_symbol
    jnz [rb - 3], get_token_symbol

    # character literals
    eq  [rb + char], ''', [rb + tmp]
    jnz [rb + tmp], get_token_char

    # number literals
    add [rb + char], 0, [rb - 1]
    arb -1
    cal is_digit
    jnz [rb - 3], get_token_number

    cal token_error

get_token_eat_comment:
    # eat everything until end of line

get_token_eat_comment_loop:
    cal get_input
    eq  [rb - 2], 10, [rb + tmp]
    jz  [rb + tmp], get_token_eat_comment_loop

    # FALLTHROUGH: last char was EOL, so return it as a token

get_token_eol:
    add '$', 0, [token]
    arb 2
    ret 0

get_token_identifier:
    # unget last char, read_identifier will get it again
    add [rb + char], 0, [get_input_buffer]

    # return read identifier pointer in [rb + char]
    # this memory needs to be freed by caller of get_token
    cal read_identifier_or_keyword
    add [rb - 2], 0, [token]
    add [rb - 3], 0, [value]

    arb 2
    ret 0

get_token_directive:
    cal read_directive
    add [rb - 2], 0, [token]

    arb 2
    ret 0

get_token_symbol:
    add [rb + char], 0, [token]
    arb 2
    ret 0

get_token_char:
    # get one character and return it as token value
    cal get_input
    add [rb - 2], 0, [value]

    # get closing quote
    cal get_input
    eq  [rb - 2], ''', [rb + tmp]
    jnz [rb + tmp], get_token_char_done

    # error, missing closing quote
    cal token_error

get_token_char_done:
    add 'c', 0, [token]
    arb 2
    ret 0

get_token_number:
    # unget last char, read_number will get it again
    add [rb + char], 0, [get_input_buffer]

    # return read number in [rb + char]
    cal read_number
    add [rb - 2], 0, [value]

    add 'n', 0, [token]
    arb 2
    ret 0
.ENDFRAME

##########
token_error:
.FRAME
    # TODO use strings once available
    out 'T'
    out 'o'
    out 'k'
    out 'e'
    out 'n'
    out ' '
    out 'e'
    out 'r'
    out 'r'
    out 'o'
    out 'r'
    out 10

    hlt
.ENDFRAME

##########
is_symbol:
.FRAME char; tmp
    arb -1

    eq  [rb + char], '+', [rb + tmp]
    jnz [rb + tmp], is_symbol_end
    eq  [rb + char], '-', [rb + tmp]
    jnz [rb + tmp], is_symbol_end
    eq  [rb + char], '=', [rb + tmp]
    jnz [rb + tmp], is_symbol_end
    eq  [rb + char], ',', [rb + tmp]
    jnz [rb + tmp], is_symbol_end
    eq  [rb + char], ':', [rb + tmp]
    jnz [rb + tmp], is_symbol_end
    eq  [rb + char], ';', [rb + tmp]
    jnz [rb + tmp], is_symbol_end
    eq  [rb + char], '[', [rb + tmp]
    jnz [rb + tmp], is_symbol_end
    eq  [rb + char], ']', [rb + tmp]

is_symbol_end:
    arb 1
    ret 1
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
read_number:
.FRAME byte, digit, tmp
    arb -3

    add 0, 0, [rb + byte]

read_number_loop:
    # get next character
    cal get_input
    add [rb - 2], 0, [rb + digit]

    # if it is not a digit, end
    add [rb + digit], 0, [rb - 1]
    arb -1
    cal is_digit
    jz  [rb - 3], read_number_end

    # convert ASCII to a number
    add [rb + digit], -48, [rb + digit]

    # byte = byte * 10 + digit
    mul [rb + byte], 10, [rb + byte]
    add [rb + byte], [rb + digit], [rb + byte]

    jz  0, read_number_loop

read_number_end:
    # unget last char
    add [rb + digit], 0, [get_input_buffer]

    arb 3
    ret 0
.ENDFRAME

##########
read_identifier_or_keyword:
.FRAME token, buffer, tmp
    arb -3

    # read the identifier into a buffer
    cal read_identifier
    add [rb - 2], 0, [rb + buffer]

    # check if the identifier is actually a keyword
    # reuse buffer [rb - 2] and length [rb - 3] as parameters
    arb -3
    cal detect_keyword
    arb 1
    add [rb - 5], 0, [rb + token]

    # if it is a keyword, free the buffer, we don't need it
    eq  [rb + token], 'i', [rb + tmp]
    jnz [rb + tmp], read_identifier_or_keyword_skip_free

    add [rb + buffer], 0, [rb - 1]
    arb -1
    cal free
    add 0, 0, [rb + buffer]

read_identifier_or_keyword_skip_free:
    arb 3
    ret 0
.ENDFRAME
##########

##########
read_identifier:
.FRAME buffer, index, char, tmp
    arb -4

    # we will store the identifier in dynamic memory that needs to be freed by caller
    add 50, 0, [rb - 1]
    arb -1
    cal alloc
    add [rb - 3], 0, [rb + buffer]

    add 0, 0, [rb + index]

read_identifier_loop:
    cal get_input
    add [rb - 2], 0, [rb + char]

    # when we find first non-alphanumeric character, we are done
    add [rb + char], 0, [rb - 1]
    arb -1
    cal is_alphanum
    jz  [rb - 3], read_identifier_done

    # store the character in buffer
    add [rb + buffer], [rb + index], [read_identifier_buffer_char_ptr]
+3 = read_identifier_buffer_char_ptr:
    add [rb + char], 0, [0]

    # increase index and check for maximum identifier length (50 - 3)
    add [rb + index], 1, [rb + index]
    lt  [rb + index], 47, [rb + tmp]
    jnz [rb + tmp], read_identifier_loop

    cal token_error

read_identifier_done:
    # zero terminate
    add [rb + buffer], [rb + index], [read_identifier_buffer_zero_ptr]
+3 = read_identifier_buffer_zero_ptr:
    add 0, 0, [0]

    # unget last char
    add [rb + char], 0, [get_input_buffer]

    arb 4
    ret 0
.ENDFRAME

##########
detect_keyword:
.FRAME string, length; tmp, char0, char1
    arb -3

    # this uses a perfect hash function, generated using gperf and a list of keywords
    # gperf < src/gperf.in

    # check string length
    lt  3, [rb + length], [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not
    lt  [rb + length], 2, [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not

    # read first character, check that it is a lowercase letter
    add [rb + string], 0, [detect_keyword_char0_ptr]
+1 = detect_keyword_char0_ptr:
    add [0], 0, [rb + char0]

    lt  [rb + char0], 'a', [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not
    lt  'z', [rb + char0], [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not

    # read second character, check that it is a lowercase letter
    add [rb + string], 1, [detect_keyword_char1_ptr]
+1 = detect_keyword_char1_ptr:
    add [0], 0, [rb + char1]

    lt  [rb + char1], 'a', [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not
    lt  'z', [rb + char1], [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not

    # calculate indexes into the asso_values table
    add [rb + char0], -97, [rb + char0]
    add [rb + char1], -97, [rb + char1]

    # look up the hash value (store to char0)
    add detect_keyword_asso_values, [rb + char0], [detect_keyword_asso_values_ptr0]
+1 = detect_keyword_asso_values_ptr0:
    add [0], [rb + length], [rb + char0]

    add detect_keyword_asso_values, [rb + char1], [detect_keyword_asso_values_ptr1]
+1 = detect_keyword_asso_values_ptr1:
    add [0], [rb + char0], [rb + char0]

    # check hash limit
    lt  28, [rb + char0], [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not

    # find candidate keyword, compare input string with the candidate
    mul [rb + char0], 4, [rb + tmp]
    add detect_keyword_wordlist, [rb + tmp], [rb - 1]
    add [rb + string], 0, [rb - 2]
    arb -2
    cal strcmp

    eq  [rb - 4], 0, [rb + tmp]
    jz  [rb + tmp], detect_keyword_is_not

    # find token id, return it
    add detect_keyword_tokens, [rb + char0], [detect_keyword_tokens_ptr]
+1 = detect_keyword_tokens_ptr:
    add [0], 0, [rb + tmp]

    arb 3
    ret 2

detect_keyword_is_not:
    # not a keyword, so it is an identifier
    add 'i', 0, [rb + tmp]

    arb 3
    ret 2

detect_keyword_asso_values:
    ds  2, 0
    db  3
    db  5
    db  10
    ds  2, 29
    db  10
    db  4
    db  10
    db  29
    db  15
    db  3
    ds  2, 10
    db  29
    db  5
    db  0
    db  5
    db  10
    db  5
    ds  4, 29
    db  10

detect_keyword_wordlist:
    # TODO use strings once supported
    ds  8, 0
    db  'r'
    db  'b'
    ds  2, 0
    db  'a'
    db  'r'
    db  'b'
    ds  9, 0
    db  'c'
    db  'a'
    db  'l'
    db  0
    db  'd'
    db  'b'
    ds  2, 0
    db  'a'
    db  'd'
    db  'd'
    ds  9, 0
    db  'm'
    db  'u'
    db  'l'
    db  0
    db  'd'
    db  's'
    ds  2, 0
    db  'r'
    db  'e'
    db  't'
    ds  9, 0
    db  'i'
    db  'n'
    ds  2, 0
    db  'e'
    db  'q'
    ds  2, 0
    db  'o'
    db  'u'
    db  't'
    ds  13, 0
    db  'j'
    db  'z'
    ds  2, 0
    db  'j'
    db  'n'
    db  'z'
    ds  13, 0
    db  'l'
    db  't'
    ds  2, 0
    db  'h'
    db  'l'
    db  't'
    db  0

detect_keyword_tokens:
    ds  2, 0
    db  'P'
    db  9
    ds  2, 0
    db  'C'
    db  'B'
    db  1
    ds  2, 0
    db  2
    db  'S'
    db  'R'
    ds  2, 0
    db  3
    db  8
    db  4
    ds  3, 0
    db  6
    db  5
    ds  3, 0
    db  7
    db  99
.ENDFRAME

##########
read_directive:
.FRAME tmp
    arb -1

    # TODO consider reading the whole string and doing a gperf
    # TODO perhaps use the same gperf code as above, just parametrized

    cal get_input

    eq  [rb - 2], 'E', [rb + tmp]
    jnz [rb + tmp], read_directive_e
    eq  [rb - 2], 'F', [rb + tmp]
    jnz [rb + tmp], read_directive_frame

    jz  0, read_directive_fail

read_directive_e:
    cal get_input

    eq  [rb - 2], 'N', [rb + tmp]
    jnz [rb + tmp], read_directive_endframe
    eq  [rb - 2], 'O', [rb + tmp]
    jnz [rb + tmp], read_directive_eof

    jz  0, read_directive_fail

read_directive_endframe:
    cal get_input
    eq  [rb - 2], 'D', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    cal get_input
    eq  [rb - 2], 'F', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    cal get_input
    eq  [rb - 2], 'R', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    cal get_input
    eq  [rb - 2], 'A', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    cal get_input
    eq  [rb - 2], 'M', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    cal get_input
    eq  [rb - 2], 'E', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    add 'D', 0, [rb + tmp]
    arb 1
    ret 0

read_directive_eof:
    cal get_input
    eq  [rb - 2], 'F', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    add 'N', 0, [rb + tmp]
    arb 1
    ret 0

read_directive_frame:
    cal get_input
    eq  [rb - 2], 'R', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    cal get_input
    eq  [rb - 2], 'A', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    cal get_input
    eq  [rb - 2], 'M', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    cal get_input
    eq  [rb - 2], 'E', [rb + tmp]
    jz  [rb + tmp], read_directive_fail

    add 'F', 0, [rb + tmp]
    arb 1
    ret 0

read_directive_fail:
    cal token_error
.ENDFRAME

##########
#print_mem:
#.FRAME byte, flag
#    arb -2
#
#    jz  [size], print_mem_finish
#
#print_mem_byte:
#+1 = print_size:
#    add [mem], 0, [rb - 1]
#    arb -1
#    cal print_num
#
#    eq  [size], [print_size], [rb + flag]
#    jnz [rb + flag], print_mem_finish
#    add [print_size], 1, [print_size]
#
#    out 44
#    jz  0, print_mem_byte
#
#print_mem_finish:
#    out 10
#
#    add -1, 0, [print_size]
#    arb 2
#    ret 0
#.ENDFRAME

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
alloc:
.FRAME size; block, tmp
    arb -2

    # we only support certain block sizes
    lt  50, [rb + size], [rb + tmp]
    jnz [rb + tmp], alloc_error

    # do we have any free blocks?
    eq  [free_head], 0, [rb + tmp]
    jnz [rb + tmp], alloc_create_block

    # yes, remove first block from the list and return it
    add [free_head], 0, [rb + block]

    add [free_head], 0, [alloc_next_block]
+1 = alloc_next_block:
    add [0], 0, [free_head]

    arb 2
    ret 1

alloc_create_block:
    # there is no block, create one
    add [heap_end], 0, [rb + block]
    add [heap_end], 50, [heap_end]

    arb 2
    ret 1

alloc_error:
    out 'A'
    out 'l'
    out 'l'
    out 'o'
    out 'c'
    out ' '
    out 'e'
    out 'r'
    out 'r'
    out 'o'
    out 'r'
    out 10

    hlt
.ENDFRAME

##########
free:
.FRAME block; tmp
    arb -1

    # set pointer to next free block in the block we are returning
    add [rb + block], 0, [free_next_block]
+3 = free_next_block:
    add [free_head], 0, [0]

    # set new free block head
    add [rb + block], 0, [free_head]

    arb 1
    ret 1
.ENDFRAME

##########
find_symbol:
.FRAME identifier; record, tmp
    arb -2

    add [symbol_head], 0, [rb + record]

find_symbol_loop:
    # are there any more records?
    eq  [rb + record], 0, [rb + tmp]
    jnz [rb + tmp], find_symbol_done

    # does this record contain the identifier?
    add [rb + identifier], 0, [rb - 1]
    add [rb + record], 1, [rb - 2]
    arb -2
    cal strcmp

    # if strcmp result is 0, we are done
    eq  [rb - 4], 0, [rb + tmp]
    jnz [rb + tmp], find_symbol_done

    # move to next record
    add [rb + record], 0, [find_symbol_next_record]
+1 = find_symbol_next_record:
    add [0], 0, [rb + record]

    jz  0, find_symbol_loop

find_symbol_done:
    arb 2
    ret 1
.ENDFRAME

##########
add_symbol:
.FRAME identifier; record
    arb -1

    # allocate a block
    add 50, 0, [rb - 1]
    arb -1
    cal alloc
    add [rb - 3], 0, [rb + record]

    # store the identifier
    add [rb + identifier], 0, [rb - 1]
    add [rb + record], 1, [rb - 2]
    arb -2
    cal strcpy

    # set pointer to next symbol
    add [rb + record], 0, [add_symbol_next_record]
+3 = add_symbol_next_record:
    add [symbol_head], 0, [0]

    # set new symbol head
    add [rb + record], 0, [symbol_head]

    arb 1
    ret 1
.ENDFRAME

##########
add_fixup:
.FRAME identifier, address; symbol, fixup, tmp
    arb -3

    # DEBUG
    out 'F'
    out ' '
    add [rb + identifier], 0, [rb - 1]
    arb -1
    cal print_str
    out ' '
    add [rb + address], 0, [rb - 1]
    arb -1
    cal print_num
    out 10

    # find or create the symbol record
    add [rb + identifier], 0, [rb - 1]
    arb -1
    cal find_symbol
    add [rb - 3], 0, [rb + symbol]

    eq  [rb + symbol], 0, [rb + tmp]
    jz  [rb + tmp], add_fixup_have_symbol

    add [rb + identifier], 0, [rb - 1]
    arb -1
    cal add_symbol
    add [rb - 3], 0, [rb + symbol]

add_fixup_have_symbol:
    # allocate a block
    # TODO use smaller blocks, or collect multiple fixups in one block
    add 50, 0, [rb - 1]
    arb -1
    cal alloc
    add [rb - 3], 0, [rb + fixup]

    # store the address of the fixup
    add [rb + fixup], 1, [add_fixup_address_ptr]
+3 = add_fixup_address_ptr:
    add [rb + address], 0, [0]

    # read current fixup list head
    add [rb + symbol], 49, [add_fixup_list_head_1]
+1 = add_fixup_list_head_1:
    add [0], 0, [rb + tmp]

    # set pointer to next fixup
    add [rb + fixup], 0, [add_fixup_next_record]
+3 = add_fixup_next_record:
    add [rb + tmp], 0, [0]

    # set new fixup list head
    add [rb + symbol], 49, [add_fixup_list_head_2]
+3 = add_fixup_list_head_2:
    add [rb + fixup], 0, [0]

    arb 3
    ret 2
.ENDFRAME

##########
set_symbol_address:
.FRAME identifier, address; symbol, tmp
    arb -2

    # DEBUG
    out 'S'
    out ' '
    add [rb + identifier], 0, [rb - 1]
    arb -1
    cal print_str
    out ' '
    add [rb + address], 0, [rb - 1]
    arb -1
    cal print_num
    out 10

    # find or create the symbol record
    add [rb + identifier], 0, [rb - 1]
    arb -1
    cal find_symbol
    add [rb - 3], 0, [rb + symbol]

    eq  [rb + symbol], 0, [rb + tmp]
    jz  [rb + tmp], set_symbol_address_have_symbol

    add [rb + identifier], 0, [rb - 1]
    arb -1
    cal add_symbol
    add [rb - 3], 0, [rb + symbol]

set_symbol_address_have_symbol:
    # store the address of the symbol
    add [rb + symbol], 48, [set_symbol_address_ptr]
+3 = set_symbol_address_ptr:
    add [rb + address], 0, [0]

    arb 2
    ret 2
.ENDFRAME

##########
strcmp:
.FRAME str1, str2; tmp, char1, char2, index
    arb -4

    add 0, 0, [rb + index]

strcmp_loop:
    add [rb + str1], [rb + index], [strcmp_str1_ptr]
+1 = strcmp_str1_ptr:
    add [0], 0, [rb + char1]

    add [rb + str2], [rb + index], [strcmp_str2_ptr]
+1 = strcmp_str2_ptr:
    add [0], 0, [rb + char2]

    # different characters, we are done
    eq  [rb + char1], [rb + char2], [rb + tmp]
    jz  [rb + tmp], strcmp_done

    # same character, is it 0?
    eq  [rb + char1], 0, [rb + tmp]
    jnz [rb + tmp], strcmp_done

    add [rb + index], 1, [rb + index]
    jz  0, strcmp_loop

strcmp_done:
    mul [rb + char2], -1, [rb + tmp]
    add [rb + char1], [rb + tmp], [rb + tmp]

    arb 4
    ret 2
.ENDFRAME

##########
strcpy:
.FRAME src, tgt; tmp, char, index
    arb -3

    add 0, 0, [rb + index]

strcpy_loop:
    add [rb + src], [rb + index], [strcpy_src_ptr]
+1 = strcpy_src_ptr:
    add [0], 0, [rb + char]

    add [rb + tgt], [rb + index], [strcpy_tgt_ptr]
+3 = strcpy_tgt_ptr:
    add [rb + char], 0, [0]

    eq  [rb + char], 0, [rb + tmp]
    jnz [rb + tmp], strcpy_done

    add [rb + index], 1, [rb + index]
    jz  0, strcpy_loop

strcpy_done:
    arb 3
    ret 2
.ENDFRAME

##########
print_str:
.FRAME str; tmp, char, index
    arb -3

    add 0, 0, [rb + index]

print_str_loop:
    add [rb + str], [rb + index], [print_str_str_ptr]
+1 = print_str_str_ptr:
    add [0], 0, [rb + char]

    eq  [rb + char], 0, [rb + tmp]
    jnz [rb + tmp], print_str_done

    out [rb + char]

    add [rb + index], 1, [rb + index]
    jz  0, print_str_loop

print_str_done:
    arb 3
    ret 1
.ENDFRAME

##########
# this function was not tested yet!
dump:
.FRAME ptr, count; tmp, byte, index
    arb -3

    add 0, 0, [rb + index]

dump_loop:
    add [rb + ptr], [rb + index], [dump_ptr]
+1 = dump_ptr:
    add [0], 0, [rb + byte]

    add [rb + ptr], [rb + index], [rb + tmp]
    out [rb + tmp]
    out [rb + byte]
    out 10

    add [rb + index], 1, [rb + index]
    lt  [rb + count], [rb + index], [rb + tmp]
    jz  [rb + tmp], dump_loop

    arb 3
    ret 2
.ENDFRAME

##########
# globals

# symbol record layout:
# 0: pointer to next symbol
# 1-47: zero-terminated symbol identifier
# 48: symbol value (address)
# 49: linked list of fixups

# fixup record layout:
# 0: pointer to next fixup
# 1: fixup address

# head of the linked list of symbols
symbol_head:
    db  0

# head of the linked list of free blocks
free_head:
    db  0

# start of unused memory
heap_end:
    db  stack

# lookahead token type
token:
    db  0

# lookahead token value, if any
value:
    db  0

# current instruction pointer
ip:
    db 0

##########
    ds  50, 0
stack:

.EOF
