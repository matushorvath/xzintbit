.EXPORT is_digit
.EXPORT char_to_digit
.EXPORT is_alpha
.EXPORT is_alphanum
.EXPORT strcmp
.EXPORT strcpy
.EXPORT zeromem
.EXPORT atoi

##########
is_digit:
.FRAME char; tmp                        # returns tmp
    arb -1

    # check if 0 <= char <= 9
    lt  '9', [rb + char], [rb + tmp]                        # if char > 9, not a digit
    jnz [rb + tmp], .end
    lt  [rb + char], '0', [rb + tmp]                        # if char < 0, not a digit

.end:
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
    jnz [rb + tmp], .invalid

    lt  '9', [rb + char], [rb + tmp]
    jz  [rb + tmp], .number

    lt  [rb + char], 'A', [rb + tmp]
    jnz [rb + tmp], .invalid

    lt  'F', [rb + char], [rb + tmp]
    jz  [rb + tmp], .upper_case

    lt  [rb + char], 'a', [rb + tmp]
    jnz [rb + tmp], .invalid

    lt  'f', [rb + char], [rb + tmp]
    jz  [rb + tmp], .lower_case

    jz  0, .invalid

.upper_case:
    add [rb + char], -55, [rb + digit]          # 'A' = 65
    jz  0, .check_radix

.lower_case:
    add [rb + char], -87, [rb + digit]          # 'a' = 97
    jz  0, .check_radix

.number:
    add [rb + char], -'0', [rb + digit]

.check_radix:
    lt  [rb + digit], [rb + radix], [rb + tmp]
    jnz [rb + tmp], .end

.invalid:
    add -1, 0, [rb + digit]

.end:
    arb 2
    ret 2
.ENDFRAME

##########
is_alpha:
.FRAME char; tmp
    arb -1

    # check if a <= char <= z
    lt  'z', [rb + char], [rb + tmp]                        # if char > z, not a letter
    jnz [rb + tmp], .end
    lt  [rb + char], 'a', [rb + tmp]                        # if !(char < a), is a letter
    jz  [rb + tmp], .end

    # check if A <= char <= Z
    lt  'Z', [rb + char], [rb + tmp]                        # if char > Z, not a letter
    jnz [rb + tmp], .end
    lt  [rb + char], 'A', [rb + tmp]                        # if !(char < A), is a letter

.end:
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
    jnz [rb + tmp], .end

    add [rb + char], 0, [rb - 1]
    arb -1
    call is_digit
    add [rb - 3], 0, [rb + tmp]
    jnz [rb + tmp], .end

    eq  [rb + char], '_', [rb + tmp]

.end:
    arb 1
    ret 1
.ENDFRAME

##########
strcmp:
.FRAME str1, str2; tmp, char1, char2, index
    arb -4

    add 0, 0, [rb + index]

.loop:
    add [rb + str1], [rb + index], [ip + 1]
    add [0], 0, [rb + char1]

    add [rb + str2], [rb + index], [ip + 1]
    add [0], 0, [rb + char2]

    # different characters, we are done
    eq  [rb + char1], [rb + char2], [rb + tmp]
    jz  [rb + tmp], .done

    # same character, is it 0?
    jz  [rb + char1], .done

    add [rb + index], 1, [rb + index]
    jz  0, .loop

.done:
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

.loop:
    add [rb + src], [rb + index], [ip + 1]
    add [0], 0, [rb + char]

    add [rb + tgt], [rb + index], [ip + 3]
    add [rb + char], 0, [0]

    jz  [rb + char], .done

    add [rb + index], 1, [rb + index]
    jz  0, .loop

.done:
    arb 2
    ret 2
.ENDFRAME

##########
zeromem:
.FRAME ptr, size; tmp
    arb -1

.loop:
    add [rb + size], -1, [rb + size]
    lt  [rb + size], 0, [rb + tmp]
    jnz [rb + tmp], .done

    add [rb + ptr], [rb + size], [ip + 3]
    add 0, 0, [0]

    jz  0, .loop

.done:
    arb 1
    ret 2
.ENDFRAME

##########
atoi:
.FRAME input; output, index, sign, digit, char, tmp         # returns output
    arb -6

    add 0, 0, [rb + output]
    add 0, 0, [rb + index]

    # read next character
    add [rb + input], [rb + index], [ip + 1]
    add [0], 0, [rb + char]
    add [rb + index], 1, [rb + index]
    jz  [rb + char], .done

    # process the minus sign if present
    add 1, 0, [rb + sign]

    eq  [rb + char], '-', [rb + tmp]
    jz  [rb + tmp], .loop

    add -1, 0, [rb + sign]

    # read next character
    add [rb + input], [rb + index], [ip + 1]
    add [0], 0, [rb + char]
    add [rb + index], 1, [rb + index]
    jz  [rb + char], .done

.loop:
    # convert current character to a digit
    add [rb + char], 0, [rb - 1]
    add 10, 0, [rb - 2]
    arb -2
    call char_to_digit
    add [rb - 4], 0, [rb + digit]

    # if it is not a digit, end
    eq  [rb + digit], -1, [rb + tmp]
    jnz [rb + tmp], .done

    # output = output * 10 + digit
    mul [rb + output], 10, [rb + output]
    add [rb + output], [rb + digit], [rb + output]

    # read next character
    add [rb + input], [rb + index], [ip + 1]
    add [0], 0, [rb + char]
    add [rb + index], 1, [rb + index]
    jz  [rb + char], .done

    jz  0, .loop

.done:
    mul [rb + digit], [rb + sign], [rb + digit]

    arb 6
    ret 1
.ENDFRAME

.EOF
