# Display characters read from stdin

.IMPORT print_num_16_b

    jnz 0, extended_init

    out 'X'
    out ' '
    out 10

    hlt

extended_init:
    arb stack

loop:
    in  [char]

    eq  [char], 0x1b, [tmp]
    jz  [tmp], after_eol

    out 10
    out 13

after_eol:
    add [char], 0, [rb - 1]
    arb -1
    call print_num_16_b

    out ' '

    jz  0, loop

char:
    db  0
tmp:
    db  0

    ds 1000, 0
stack:

.EOF
