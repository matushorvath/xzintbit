# from process.s
.IMPORT process

# from output.s
.IMPORT print_map

##########
# entry point
    arb stack

    call main
    hlt

##########
main:
.FRAME
    arb -0

    call process
    call print_map

    arb 0
    ret 0
.ENDFRAME

##########
    ds  50, 0
stack:

.EOF
