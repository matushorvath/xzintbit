    arb message

loop:
    jz  [rb + 0], done
    out [rb + 0]
    arb 1
    jz  0, loop

done:
    out 10
    hlt

message:
    db "Hello, world!", 0

.EOF
