# from fixup.s
.IMPORT do_fixups

# from object.s
.IMPORT output_object

# from parser.s
.IMPORT parse

    arb stack

    call main
    hlt

##########
main:
.FRAME
    arb -0

    call parse
    call do_fixups
    call output_object

    arb 0
    ret 0
.ENDFRAME

##########
    ds  50, 0
stack:

.EOF
