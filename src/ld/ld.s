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
    call process
    call print_modules

    ret 0
.ENDFRAME

##########
    ds  1000, 0
stack:

.EOF
