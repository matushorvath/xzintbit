.EXPORT process

# from load.s
.IMPORT load_objects

# from resolve.s
.IMPORT include_objects
.IMPORT resolve_symbols

# from relocate.s
.IMPORT relocate

# from link.s
.IMPORT link_imports

##########
process:
.FRAME
    arb -0

    call load_objects
    call include_objects
    call resolve_symbols
    call relocate
    call link_imports

    arb 0
    ret 0
.ENDFRAME

.EOF
