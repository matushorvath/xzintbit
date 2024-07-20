.EXPORT brk
.EXPORT sbrk

.IMPORT __heap_start

# a simple mechanism for allocating memory, inspired by the UNIX brk/sbrk syscalls

##########
brk:
.FRAME addr;
    add [rb + addr], 0, [brk_addr]
    ret 1
.ENDFRAME

##########
sbrk:
.FRAME increment; prev_brk              # returns prev_brk
    arb -1

    add [brk_addr], 0, [rb + prev_brk]
    add [brk_addr], [rb + increment], [brk_addr]

    arb 1
    ret 1
.ENDFRAME

##########
brk_addr:
    db  __heap_start

.EOF
