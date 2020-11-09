# this uses a perfect hash function, generated using gperf and a list of keywords
# gperf < doc/gperf-keyword.in

.EXPORT detect_keyword

# from libxib/string.s
.IMPORT strcmp

##########
detect_keyword:
.FRAME string, length; tmp, char0, char1
    arb -3

    # check string length against MAX_WORD_LENGTH and MIN_WORD_LENGTH
    lt  4, [rb + length], [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not
    lt  [rb + length], 2, [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not

    # read first character, check that it is a lowercase letter
    add [rb + string], 0, [ip + 1]
    add [0], 0, [rb + char0]

    lt  [rb + char0], 'a', [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not
    lt  'z', [rb + char0], [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not

    # read second character, check that it is a lowercase letter
    add [rb + string], 1, [ip + 1]
    add [0], 0, [rb + char1]

    lt  [rb + char1], 'a', [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not
    lt  'z', [rb + char1], [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not

    # calculate indexes into the asso_values table
    add [rb + char0], -'a', [rb + char0]
    add [rb + char1], -'a', [rb + char1]

    # look up the hash value (store to char0)
    add detect_keyword_asso_values, [rb + char0], [ip + 1]
    add [0], [rb + length], [rb + char0]

    add detect_keyword_asso_values, [rb + char1], [ip + 1]
    add [0], [rb + char0], [rb + char0]

    # check hash limit MAX_HASH_VALUE
    lt  37, [rb + char0], [rb + tmp]
    jnz [rb + tmp], detect_keyword_is_not

    # find candidate keyword, compare input string with the candidate
    mul [rb + char0], 5, [rb + tmp]
    add detect_keyword_wordlist, [rb + tmp], [rb - 1]
    add [rb + string], 0, [rb - 2]
    arb -2
    call strcmp

    jnz [rb - 4], detect_keyword_is_not

    # find token id, return it
    add detect_keyword_tokens, [rb + char0], [ip + 1]
    add [0], 0, [rb + tmp]

    arb 3
    ret 2

detect_keyword_is_not:
    # not a keyword, so it is an identifier
    add 'i', 0, [rb + tmp]

    arb 3
    ret 2

detect_keyword_asso_values:
    # copied from gperf-keyword.c
    db                               0,  0,  0
    db   5, 10, 38, 38, 10, 20, 10, 38, 20, 15
    db   5, 10, 10,  5,  0,  5, 15, 10, 38, 38
    db  38, 38, 10

detect_keyword_wordlist:
    # copied from gperf-keyword.c
    ds  10, 0
    db  "rb", 0, 0, 0
    db  "arb", 0, 0
    db  "call", 0
    ds  10, 0
    db  "db", 0, 0, 0
    db  "add", 0, 0
    ds  15, 0
    db  "ds", 0, 0, 0
    db  "ret", 0, 0
    ds  15, 0
    db  "eq", 0, 0, 0
    db  "jnz", 0, 0
    ds  15, 0
    db  "jz", 0, 0, 0
    db  "out", 0, 0
    ds  15, 0
    db  "in", 0, 0, 0
    db  "mul", 0, 0
    ds  15, 0
    db  "ip", 0, 0, 0
    db  "hlt", 0, 0
    ds  15, 0
    db  "lt", 0, 0, 0

detect_keyword_tokens:
    ds  2, 0
    db  'P'
    db  9
    db  'C'
    ds  2, 0
    db  'B'
    db  1
    ds  3, 0
    db  'S'
    db  'R'
    ds  3, 0
    db  8
    db  5
    ds  3, 0
    db  6
    db  4
    ds  3, 0
    db  3
    db  2
    ds  3, 0
    db  'I'
    db  99
    ds  3, 0
    db  7
.ENDFRAME

.EOF
