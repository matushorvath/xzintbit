    # magic instruction; extended VM will start at extended_init
    jnz 0, extended_init

    # standard VM starts here
    arb stack

    add standard_msg, 0, [rb - 1]
    arb -1
    call print_str

    hlt

extended_init:
    # extended VM starts here
    arb stack

    add extended_msg, 0, [rb - 1]
    arb -1
    call print_str

    hlt


print_str:
.FRAME str; tmp
    arb -1

loop:
    add [rb + str], 0, [ip + 1]
    add [0], 0, [rb + tmp]

    jz  [rb + tmp], done
    out [rb + tmp]

    add [rb + str], 1, [rb + str]
    jz  0, loop

done:
    arb 1
    ret 1
.ENDFRAME

standard_msg:
    db  "Standard IC VM", 10, 0
extended_msg:
    db  "Extended IC VM", 10, 13, 0

    ds 50, 0
stack:

.EOF

# TODO create a Makefile to test this with an extended VM
# ~/intcode/xzintbit/vms/c/ic test/extended_vm/bin/extended_vm.input
