# from process.s
.IMPORT process

# from output.s
.IMPORT print_modules

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
    call print_modules

    arb 0
    ret 0
.ENDFRAME

##########
    ds  1000, 0
stack:

.EOF
