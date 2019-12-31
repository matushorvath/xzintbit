    out '?'
    out 10
    in  [data]
    in  [data + 1]
    add 1, [data], [data]
    out [data]
    out [data + 1]

    arb stack
    cal test
    hlt

test:
    add 0, 0, [data]
    out 'X'
    out 10
    ret 0

data:
    ds 2, 0

    ds 50, 0
stack:
