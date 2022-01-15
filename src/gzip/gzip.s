# from headers.s
.IMPORT read_member

##########
# entry point
    arb stack

    call main
    hlt

##########
main:
.FRAME
    arb -0

    # Assume there is just one member. RFC 1952 says there may be multiple, but we have no
    # way of detecting them since Intcode VM has no EOF handling.
    call read_member

    arb 0
    ret 0
.ENDFRAME

##########
    ds  50, 0
stack:

.EOF
