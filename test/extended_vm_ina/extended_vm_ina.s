# test extended instruction ina

    # magic instruction; extended VM will start at extended_init
    jnz 0, extended_init
    hlt

extended_init:
    # read 5 characters using ina [char]
    # only 3 characters are available, so last two instructions should return -1

    db  13, char                        # ina [char]
    eq  [char], 'A', [tmp]
    jz  [tmp], fail

    db  13, char                        # ina [char]
    eq  [char], 'B', [tmp]
    jz  [tmp], fail

    db  13, char                        # ina [char]
    eq  [char], 'C', [tmp]
    jz  [tmp], fail

    db  13, char                        # ina [char]
    eq  [char], -1, [tmp]
    jz  [tmp], fail

    db  13, char                        # ina [char]
    eq  [char], -1, [tmp]
    jz  [tmp], fail

    out 'O'
    out 'K'
    hlt

fail:
    out 'f'
    out 'a'
    out 'i'
    out 'l'
    hlt

char:
    db  0
tmp:
    db  0

.EOF
