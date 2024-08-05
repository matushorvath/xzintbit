# test extended instruction ina

    # magic instruction; extended VM will start at extended_init
    jnz 0, extended_init
    hlt

extended_init:
    # read 5 characters using ina [char]
    # there is a delay between the characters, so we will receive some -1 in between

    add 0, 0, [waited]

.loop0:
    db  13, char                        # ina [char]
    eq  [char], -1, [tmp]
    jz  [tmp], .char0

    add 1, 0, [waited]
    jz  0, .loop0

.char0:
    jz  [waited], .fail                 # expect at least one -1 from ina [char]
    eq  [char], 'A', [tmp]
    jz  [tmp], .fail

    add 0, 0, [waited]

.loop1:
    db  13, char                        # ina [char]
    eq  [char], -1, [tmp]
    jz  [tmp], .char1

    add 1, 0, [waited]
    jz  0, .loop1

.char1:
    jz  [waited], .fail                 # expect at least one -1 from ina [char]
    eq  [char], 'B', [tmp]
    jz  [tmp], .fail

    add 0, 0, [waited]

.loop2:
    db  13, char                        # ina [char]
    eq  [char], -1, [tmp]
    jz  [tmp], .char2

    add 1, 0, [waited]
    jz  0, .loop2

.char2:
    jz  [waited], .fail                 # expect at least one -1 from ina [char]
    eq  [char], 'C', [tmp]
    jz  [tmp], .fail

.loop_eof:
    db  13, char                        # ina [char]
    eq  [char], -1, [tmp]
    jnz [tmp], .loop_eof

    # expecting the VM to exit in the loop above, with "no more inputs" on stderr

.fail:
    out 'f'
    out 'a'
    out 'i'
    out 'l'
    hlt

char:
    db  0
waited:
    db  0
tmp:
    db  0

.EOF
