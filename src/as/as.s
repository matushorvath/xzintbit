# from fixup.s
.IMPORT process_fixups

# from object.s
.IMPORT output_object

# from parser.s
.IMPORT parse

##########
# entry point
    arb stack

    call main
    hlt

##########
main:
.FRAME
    call parse
    call process_fixups
    call output_object

    ret 0
.ENDFRAME

##########
    ds  1000, 0
stack:

.EOF
