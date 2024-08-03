# test extended instruction ftr

    # magic instruction; extended VM will start at extended_init
    jnz 0, extended_init
    hlt

extended_init:
    add -42, 0, [res]
    # ftr 0, [res], should be 0
    db  110, 0, res
    jz  [res], ftr0_ok

    out '0'
    hlt

ftr0_ok:
    add -42, 0, [res]
    # ftr 66, [res], should be 0
    db  110, 66, res
    jz  [res], ftr66_ok

    out '6'
    out '6'
    hlt

ftr66_ok:
    add -42, 0, [res]
    # ftr 7, [res], should be 1
    db  110, 7, res
    eq  [res], 1, [tmp]
    jnz [tmp], ftr7_ok

    out '7'
    hlt

ftr7_ok:
    add -42, 0, [res]
    # ftr 10, [res], should be 1
    db  110, 10, res
    eq  [res], 1, [tmp]
    jnz [tmp], ftr10_ok

    out '1'
    out '0'
    hlt

ftr10_ok:
    add -42, 0, [res]
    # ftr 13, [res], should be 0
    db  110, 13, res
    eq  [res], 1, [tmp]
    jnz [tmp], ftr13_ok

    out '1'
    out '3'
    hlt

ftr13_ok:
    out 'O'
    out 'K'
    hlt

tmp:
    db  0
res:
    db  0

.EOF
