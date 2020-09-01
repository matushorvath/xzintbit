# from load.s
.IMPORT load_objects

.IMPORT include_objects
.IMPORT resolve_symbols
.IMPORT relocate
.IMPORT connect_imports
.IMPORT print_modules
#.IMPORT dump_symbols

    arb stack

    call main
    hlt

##########
main:
.FRAME
    arb -0

    call load_objects
    call include_objects
    call resolve_symbols
    call relocate
    call connect_imports
    call print_modules

    #call dump_symbols

    arb 0
    ret 0
.ENDFRAME

##########
    ds  50, 0
stack:

.EOF
