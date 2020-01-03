    arb stack

##########
main:
.FRAME tmp
    arb -1

main_loop:
    cal get_token

    # print token type
    out [rb - 2]

    # print token value if relevant
    eq  [rb - 2], 'n', [rb + tmp]
    jnz [rb + tmp], main_print_n
    eq  [rb - 2], 'c', [rb + tmp]
    jnz [rb + tmp], main_print_c
    eq  [rb - 2], 'i', [rb + tmp]
    jnz [rb + tmp], main_print_i
    jz  0, main_finish

main_print_n:
    out ' '
    # reuse [rb - 3] as a parameter to print_num
    arb -3
    cal print_num
    arb 2
    jz  0, main_finish

main_print_c:
    out ' '
    out [rb - 3]
    jz  0, main_finish

main_print_i:
    # TODO free the buffer
    out ' '
    # reuse [rb - 3] as a parameter to print_str
    arb -3
    cal print_str
    arb 2
    jz  0, main_finish

main_finish:
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
# F .FRAME; D .ENDFRAME; $ EOL
# + - = , : ; [ ]
# n [0-9]+ i [a-zA-Z_][a-zA-Z0-9_]* c ' '

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

    hlt

get_token_eat_comment:
    # TODO eat the comment
    hlt

get_token_eol:
    add '$', 0, [rb + tmp]
    arb 2
    ret 0

get_token_identifier:
    # unget last char, parse_identifier will get it again
    add [rb + char], 0, [get_input_buffer]

    # return parsed identifier pointer in [rb + char]
    # this memory needs to be freed by caller of get_token
    cal parse_identifier_or_keyword
    add [rb - 2], 0, [rb + tmp]
    add [rb - 3], 0, [rb + char]

    arb 2
    ret 0

get_token_directive:
    cal get_input

    eq  [rb - 2], 'E', [rb + tmp]
    jnz [rb + tmp], get_token_directive_E
    eq  [rb - 2], 'F', [rb + tmp]
    jnz [rb + tmp], get_token_directive_F

get_token_directive_fail:
    hlt

get_token_directive_F:
    cal get_input
    eq  [rb - 2], 'R', [rb + tmp]
    jz  [rb + tmp], get_token_directive_fail

    cal get_input
    eq  [rb - 2], 'A', [rb + tmp]
    jz  [rb + tmp], get_token_directive_fail

    cal get_input
    eq  [rb - 2], 'M', [rb + tmp]
    jz  [rb + tmp], get_token_directive_fail

    cal get_input
    eq  [rb - 2], 'E', [rb + tmp]
    jz  [rb + tmp], get_token_directive_fail

    add 'F', 0, [rb + tmp]
    arb 2
    ret 0

get_token_directive_E:
    cal get_input
    eq  [rb - 2], 'N', [rb + tmp]
    jz  [rb + tmp], get_token_directive_fail

    cal get_input
    eq  [rb - 2], 'D', [rb + tmp]
    jz  [rb + tmp], get_token_directive_fail

    cal get_input
    eq  [rb - 2], 'F', [rb + tmp]
    jz  [rb + tmp], get_token_directive_fail

    cal get_input
    eq  [rb - 2], 'R', [rb + tmp]
    jz  [rb + tmp], get_token_directive_fail

    cal get_input
    eq  [rb - 2], 'A', [rb + tmp]
    jz  [rb + tmp], get_token_directive_fail

    cal get_input
    eq  [rb - 2], 'M', [rb + tmp]
    jz  [rb + tmp], get_token_directive_fail

    cal get_input
    eq  [rb - 2], 'E', [rb + tmp]
    jz  [rb + tmp], get_token_directive_fail

    add 'D', 0, [rb + tmp]
    arb 2
    ret 0

get_token_symbol:
    add [rb + char], 0, [rb + tmp]
    arb 2
    ret 0

get_token_char:
    # get one character and return it in [rb + char]
    cal get_input
    add [rb - 2], 0, [rb + char]

    # get closing quote
    cal get_input
    eq  [rb - 2], ''', [rb + tmp]
    jz  [rb + tmp], get_token_char_fail

    add 'c', 0, [rb + tmp]
    arb 2
    ret 0

get_token_char_fail:
    # error, missing closing quote
    hlt

get_token_number:
    # unget last char, parse_number will get it again
    add [rb + char], 0, [get_input_buffer]

    # return parsed number in [rb + char]
    cal parse_number
    add [rb - 2], 0, [rb + char]

    add 'n', 0, [rb + tmp]
    arb 2
    ret 0
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
parse_number:
.FRAME byte, digit, tmp
    arb -3

    add 0, 0, [rb + byte]

parse_number_loop:
    # get next character
    cal get_input
    add [rb - 2], 0, [rb + digit]

    # if it is not a digit, end
    add [rb + digit], 0, [rb - 1]
    arb -1
    cal is_digit
    jz  [rb - 3], parse_number_end

    # convert ASCII to a number
    add [rb + digit], -48, [rb + digit]

    # byte = byte * 10 + digit
    mul [rb + byte], 10, [rb + byte]
    add [rb + byte], [rb + digit], [rb + byte]

    jz  0, parse_number_loop

parse_number_end:
    # unget last char
    add [rb + digit], 0, [get_input_buffer]

    arb 3
    ret 0
.ENDFRAME

##########
parse_identifier_or_keyword:
.FRAME token, buffer, tmp
    arb -3

    out 'A'
    out 10

    # parse the identifier into a buffer
    cal parse_identifier
    add [rb - 2], 0, [rb + buffer]

    out 'B'
    out 10

    # check if the identifier is actually a keyword
    # reuse buffer [rb - 2] and length [rb - 3] as parameters
    arb -3
    cal detect_keyword
    arb 1
    add [rb - 5], 0, [rb + token]

    out 'C'
    out [rb + token]
    out 10

    # if it is a keyword, free the buffer, we don't need it
    eq  [rb + token], 'i', [rb + tmp]
    jnz [rb + tmp], parse_identifier_or_keyword_skip_free

    # TODO actually implement free
    #add [rb + buffer], 0, [rb - 1]
    #arb -1
    #cal free
    add 0, 0, [rb + buffer]

parse_identifier_or_keyword_skip_free:
    arb 3
    ret 0
.ENDFRAME
##########

##########
parse_identifier:
.FRAME buffer, index, char, tmp
    arb -4

    # we will store the identifier in dynamic memory that needs to be freed by caller
    add 50, 0, [rb - 1]
    arb -1
    cal alloc
    add [rb - 3], 0, [rb + buffer]

    add 0, 0, [rb + index]

parse_identifier_loop:
    cal get_input
    add [rb - 2], 0, [rb + char]

    # when we find first non-alphanumeric character, we are done
    add [rb + char], 0, [rb - 1]
    arb -1
    cal is_alphanum
    jz  [rb - 3], parse_identifier_done

    # store the character in buffer
    add [rb + buffer], [rb + index], [parse_identifier_buffer_char_ptr]
+3 = parse_identifier_buffer_char_ptr:
    add [rb + char], 0, [0]

    # increase index and check for maximum identifier length (50 - 3)
    add [rb + index], 1, [rb + index]
    lt  [rb + index], 47, [rb + tmp]
    jz  [rb + tmp], parse_identifier_error

    jz  0, parse_identifier_loop

parse_identifier_done:
    # zero terminate
    add [rb + buffer], [rb + index], [parse_identifier_buffer_zero_ptr]
+3 = parse_identifier_buffer_zero_ptr:
    add 0, 0, [0]

    # unget last char
    add [rb + char], 0, [get_input_buffer]

    # TODO remove
    add [rb + buffer], 0, [rb - 1]
    arb -1
    cal print_str

    arb 4
    ret 0

parse_identifier_error:
    hlt
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

    out 'K'
    out [rb + char0]
    out [rb + char1]
    out 10

    # calculate indexes into the asso_values table
    add [rb + char0], -97, [rb + char0]
    add [rb + char1], -97, [rb + char1]

    out 'L'
    out [rb + char0]
    out [rb + char1]
    out 10

    # look up the hash value (store to char0)
    add detect_keyword_asso_values, [rb + char0], [detect_keyword_asso_values_ptr0]
+1 = detect_keyword_asso_values_ptr0:
    add [0], [rb + length], [rb + char0]

    out 'M'
    out [rb + char0]
    out 10

    add detect_keyword_asso_values, [rb + char1], [detect_keyword_asso_values_ptr1]
+1 = detect_keyword_asso_values_ptr1:
    add [0], [rb + char0], [rb + char0]

    out 'N'
    out [rb + char0]
    out 10

    # check hash limit
    lt  33, [rb + char0], [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not

    # find candidate keyword, compare input with the candidate
    mul [rb + char0], 4, [rb + tmp]
    add detect_keyword_wordlist, [rb + tmp], [detect_keyword_wordlist_ptr]
+1 = detect_keyword_wordlist_ptr:
    add [0], 0, [rb - 1]
    add [rb + string], 0, [rb - 2]
    arb -2
    cal strcmp

    out 'O'
    out [rb - 4]
    out 10

    eq  [rb - 4], 0, [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not

    # find token id, return it
    add detect_keyword_tokens, [rb + char0], [detect_keyword_tokens_ptr]
+1 = detect_keyword_tokens_ptr:
    add [0], 0, [rb + tmp]

    arb 3
    ret 2

detect_keyword_is_not:
    # not a keyword, so it is an identifier
    add 'i', 0, [rb + tmp]

    out 'W'
    out [rb + tmp]
    out 10

    arb 3
    ret 2

detect_keyword_asso_values:
    ds  2, 0
    db  3
    db  5
    db  10
    ds  2, 34
    ds  2, 15
    db  10
    db  34
    db  15
    db  3
    ds  2, 10
    db  34
    db  5
    db  0
    db  5
    db  10
    db  5
    ds  4, 34
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
    ds  13, 0
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
    db  'i'
    db  'n'
    ds  17, 0
    db  'h'
    db  'l'
    db  't'
    db  0

detect_keyword_tokens:
    ds  2, 0
    db  'P'
    db  'T'
    ds  2, 0
    db  'C'
    db  'B'
    db  'A'
    ds  2, 0
    db  'M'
    db  'S'
    db  'R'
    ds  3, 0
    db  'E'
    db  'O'
    ds  3, 0
    db  'Z'
    db  'J'
    ds  3, 0
    db  'L'
    db  'I'
    ds  4, 0
    db  'H'
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
    hlt
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

# head of the linked list of symbols
symbol_head:
    db  0

# head of the linked list of free blocks
free_head:
    db  0

# start of unused memory
heap_end:
    db  stack

##########
    ds  50, 0
stack:
