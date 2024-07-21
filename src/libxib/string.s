.EXPORT is_digit
.EXPORT char_to_digit
.EXPORT is_alpha
.EXPORT is_alphanum
.EXPORT strcmp
.EXPORT strcpy

##########
is_digit:
.FRAME char; tmp                        # returns tmp
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
char_to_digit:
.FRAME char, radix; digit, tmp          # returns digit; -1 if invalid digit
    arb -2

    # convert char to digit
    lt  [rb + char], '0', [rb + tmp]
    jnz [rb + tmp], char_to_digit_invalid

    lt  '9', [rb + char], [rb + tmp]
    jz  [rb + tmp], char_to_digit_number

    lt  [rb + char], 'A', [rb + tmp]
    jnz [rb + tmp], char_to_digit_invalid

    lt  'F', [rb + char], [rb + tmp]
    jz  [rb + tmp], char_to_digit_upper_case

    lt  [rb + char], 'a', [rb + tmp]
    jnz [rb + tmp], char_to_digit_invalid

    lt  'f', [rb + char], [rb + tmp]
    jz  [rb + tmp], char_to_digit_lower_case

    jz  0, char_to_digit_invalid

char_to_digit_upper_case:
    add [rb + char], -55, [rb + digit]          # 'A' = 65
    jz  0, char_to_digit_check_radix

char_to_digit_lower_case:
    add [rb + char], -87, [rb + digit]          # 'a' = 97
    jz  0, char_to_digit_check_radix

char_to_digit_number:
    add [rb + char], -'0', [rb + digit]

char_to_digit_check_radix:
    lt  [rb + digit], [rb + radix], [rb + tmp]
    jnz [rb + tmp], char_to_digit_end

char_to_digit_invalid:
    add -1, 0, [rb + digit]

char_to_digit_end:
    arb 2
    ret 2
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
    call is_alpha
    add [rb - 3], 0, [rb + tmp]
    jnz [rb + tmp], is_alphanum_end

    add [rb + char], 0, [rb - 1]
    arb -1
    call is_digit
    add [rb - 3], 0, [rb + tmp]
    jnz [rb + tmp], is_alphanum_end

    eq  [rb + char], '_', [rb + tmp]

is_alphanum_end:
    arb 1
    ret 1
.ENDFRAME

##########
strcmp:
.FRAME str1, str2; tmp, char1, char2, index
    arb -4

    add 0, 0, [rb + index]

strcmp_loop:
    add [rb + str1], [rb + index], [ip + 1]
    add [0], 0, [rb + char1]

    add [rb + str2], [rb + index], [ip + 1]
    add [0], 0, [rb + char2]

    # different characters, we are done
    eq  [rb + char1], [rb + char2], [rb + tmp]
    jz  [rb + tmp], strcmp_done

    # same character, is it 0?
    jz  [rb + char1], strcmp_done

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
.FRAME src, tgt; index, char
    arb -2

    add 0, 0, [rb + index]

strcpy_loop:
    add [rb + src], [rb + index], [ip + 1]
    add [0], 0, [rb + char]

    add [rb + tgt], [rb + index], [ip + 3]
    add [rb + char], 0, [0]

    jz  [rb + char], strcpy_done

    add [rb + index], 1, [rb + index]
    jz  0, strcpy_loop

strcpy_done:
    arb 2
    ret 2
.ENDFRAME

.EOF
