# test extended instruction ina

    # magic instruction; extended VM will start at extended_init
    jnz 0, extended_init
    hlt

extended_init:
    # read 5 characters using ina [char]
    # there is a delay between the characters, so we will receive some -1 in between

.loop0:
    db  13, char                        # ina [char]
    eq  [char], -1, [tmp]
    jnz [tmp], .loop0

    eq  [char], 'A', [tmp]
    jz  [tmp], fail

.loop1:
    db  13, char                        # ina [char]
    eq  [char], -1, [tmp]
    jnz [tmp], .loop1

    eq  [char], 'B', [tmp]
    jz  [tmp], fail

.loop2:
    db  13, char                        # ina [char]
    eq  [char], -1, [tmp]
    jnz [tmp], .loop2

    eq  [char], 'C', [tmp]
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
