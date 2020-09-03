.EXPORT set_as_mem
.EXPORT set_as_mem_str

.EXPORT mem_head
.EXPORT mem_tail
.EXPORT mem_index

# from libxib/memory.s
.IMPORT set_mem

##########
set_as_mem:
.FRAME byte; buffer, tmp
    arb -2

    add mem_head, 0, [rb - 1]
    add mem_tail, 0, [rb - 2]
    add mem_index, 0, [rb - 3]
    add [rb + byte], 0, [rb - 4]
    arb -4
    call set_mem

    arb 2
    ret 1
.ENDFRAME

##########
set_as_mem_str:
.FRAME string; index, char
    arb -2

    add 0, 0, [rb + index]

set_mem_str_loop:
    add [rb + string], [rb + index], [ip + 1]
    add [0], 0, [rb + char]

    jz  [rb + char], set_mem_str_done

    add mem_head, 0, [rb - 1]
    add mem_tail, 0, [rb - 2]
    add mem_index, 0, [rb - 3]
    add [rb + char], 0, [rb - 4]
    arb -4
    call set_mem

    add [rb + index], 1, [rb + index]
    jz  0, set_mem_str_loop

set_mem_str_done:
    arb 2
    ret 1
.ENDFRAME

##########
# globals

# output memory buffer
mem_head:
    db 0
mem_tail:
    db 0

# index of next unused byte in last memory buffer
mem_index:
    db 0

.EOF
