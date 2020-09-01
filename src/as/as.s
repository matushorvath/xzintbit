# from as_split.s
.IMPORT initialize
.IMPORT parse

# from fixup.s
.IMPORT do_fixups

# from object.s
.IMPORT output_object

    arb stack

    call main
    hlt

##########
main:
.FRAME
    arb -0

    call initialize
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
