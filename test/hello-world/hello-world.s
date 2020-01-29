    arb message

loop:
    jz  [rb], done
    out [rb]
    arb 1
    jz  0, loop

done:
    out 10
    hlt

message:
    db "Hello, world!", 0

.EOF
