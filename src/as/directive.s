.EXPORT detect_directive

# from libxib/string.s
.IMPORT strcmp

# from util.s
.IMPORT report_error

##########
detect_directive:
.FRAME string, length; tmp, char0, char2
    arb -3

    # this uses a perfect hash function, generated using gperf and a list of directives
    # gperf < doc/gperf-directive.in

    # check string length against MAX_WORD_LENGTH and MIN_WORD_LENGTH
    lt  8, [rb + length], [rb + tmp]
    jnz [rb + tmp], detect_directive_is_not
    lt  [rb + length], 3, [rb + tmp]
    jnz [rb + tmp], detect_directive_is_not

    # read first character, check that it is an uppercase letter
    add [rb + string], 0, [ip + 1]
    add [0], 0, [rb + char0]

    lt  [rb + char0], 'A', [rb + tmp]
    jnz [rb + tmp], detect_directive_is_not
    lt  'Z', [rb + char0], [rb + tmp]
    jnz [rb + tmp], detect_directive_is_not

    # read third character, check that it is an uppercase letter
    add [rb + string], 2, [ip + 1]
    add [0], 0, [rb + char2]

    lt  [rb + char2], 'A', [rb + tmp]
    jnz [rb + tmp], detect_directive_is_not
    lt  'Z', [rb + char2], [rb + tmp]
    jnz [rb + tmp], detect_directive_is_not

    # calculate indexes into the asso_values table
    add [rb + char0], -'A', [rb + char0]
    add [rb + char2], -'A', [rb + char2]

    # look up the hash value (store to char0)
    add detect_directive_asso_values, [rb + char0], [ip + 1]
    add [0], [rb + length], [rb + char0]

    add detect_directive_asso_values, [rb + char2], [ip + 1]
    add [0], [rb + char0], [rb + char0]

    # check hash limit MAX_HASH_VALUE
    lt  18, [rb + char0], [rb + tmp]
    jnz [rb + tmp], detect_directive_is_not

    # find candidate directive, compare input string with the candidate
    mul [rb + char0], 10, [rb + tmp]
    add detect_directive_wordlist, [rb + tmp], [rb - 1]
    add [rb + string], 0, [rb - 2]
    arb -2
    call strcmp

    jnz [rb - 4], detect_directive_is_not

    # find token id, return it
    add detect_directive_tokens, [rb + char0], [ip + 1]
    add [0], 0, [rb + tmp]

    arb 3
    ret 2

detect_directive_is_not:
    # not a directive
    add err_invalid_directive, 0, [rb]
    call report_error

detect_directive_asso_values:
    # copied from gperf-directive.c
    db                       0, 19, 19,  5,  5
    db   5, 19, 19,  0, 19, 19, 19,  5, 19, 19
    db   0, 19, 19,  5, 19, 19, 19, 19, 19, 19
    db  19, 19, 19, 19, 19, 19, 19, 19, 19, 19
    db  19

detect_directive_wordlist:
    # copied from gperf-directive.c
    ds  60, 0
    db  "IMPORT", 0, 0, 0, 0
    ds  10, 0
    db  "EOI", 0, 0, 0, 0, 0, 0, 0
    ds  10, 0
    db  "FRAME", 0, 0, 0, 0, 0
    db  "EXPORT", 0, 0, 0, 0
    ds  10, 0
    db  "EOF", 0, 0, 0, 0, 0, 0, 0
    ds  20, 0
    db  "SYMBOL", 0, 0, 0, 0
    ds  10, 0
    db  "ENDFRAME", 0, 0

detect_directive_tokens:
    ds  6, 0
    db  'I'   # IMPORT
    ds  1, 0
    db  'O'   # EOI
    ds  1, 0
    db  'F'   # FRAME
    db  'E'   # EXPORT
    ds  1, 0
    db  'N'   # EOF
    ds  2, 0
    db  'Y'   # SYMBOL
    ds  1, 0
    db  'D'   # ENDFRAME
.ENDFRAME

##########
# error messages

err_invalid_directive:
    db  "Invalid directive", 0

.EOF
