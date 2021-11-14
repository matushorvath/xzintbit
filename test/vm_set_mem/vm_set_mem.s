add [bas], [inc], [res]
out [res]

add [pbas], 0, [ip + 1]
add [0], [inc], [res]
out [res]

hlt

bas:
    db '0'
inc:
    db 3
res:
    db 0

pbas:
    db bas

.EOF
