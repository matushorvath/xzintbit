# from as_split.s
.IMPORT initialize
.IMPORT parse
.IMPORT do_fixups
.IMPORT print_mem
.IMPORT print_reloc
.IMPORT print_imports
.IMPORT print_exports

    arb stack

    call main
    hlt

##########
main:
.FRAME
    arb -0

    call initialize

    call parse

    # run fixups
    call do_fixups

    # print compiled memory contents
    call print_mem

    # print relocations, imported and exported symbols
    call print_reloc
    call print_imports
    call print_exports

    arb 0
    ret 0
.ENDFRAME

##########
    ds  50, 0
stack:

.EOF
