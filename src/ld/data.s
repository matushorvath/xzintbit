.EXPORT create_module
.EXPORT create_import
.EXPORT create_export

.EXPORT module_head
.EXPORT module_tail
.EXPORT symbol_head
.EXPORT symbol_tail

# from libxib/heap.s
.IMPORT alloc

# from libxib/string.s
.IMPORT zeromem

##########
create_module:
.FRAME is_library; module
    arb -1

    # allocate a block
    add MODULE_SIZE, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + module]

    # initialize to zeros
    add [rb + module], 0, [rb - 1]
    add MODULE_SIZE, 0, [rb - 2]
    arb -2
    call zeromem

    # object files are mandatory, libraries are optional
    add [rb + module], MODULE_NEEDED, [ip + 3]
    eq  [rb + is_library], 0, [0]

    # append to the tail if any
    jz  [module_tail], create_module_is_first

    add [module_tail], MODULE_NEXT_PTR, [ip + 3]
    add [rb + module], 0, [0]
    add [rb + module], 0, [module_tail]

    jz  0, create_module_done

create_module_is_first:
    add [rb + module], 0, [module_head]
    add [rb + module], 0, [module_tail]

create_module_done:
    arb 1
    ret 1
.ENDFRAME

##########
create_import:
.FRAME module; import, tmp
    arb -2

    # allocate a block
    add IMPORT_SIZE, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + import]

    # initialize to zeros
    add [rb + import], 0, [rb - 1]
    add IMPORT_SIZE, 0, [rb - 2]
    arb -2
    call zeromem

    # save module pointer
    add [rb + import], IMPORT_MODULE, [ip + 3]
    add [rb + module], 0, [0]

    # add to the head of doubly-linked list
    add [rb + module], MODULE_IMPORTS_HEAD, [ip + 1]
    add [0], 0, [rb + tmp]

    add [rb + import], IMPORT_NEXT_PTR, [ip + 3]
    add [rb + tmp], 0, [0]

    add [rb + import], IMPORT_PREV_PTR, [ip + 3]
    add 0, 0, [0]

    add [rb + module], MODULE_IMPORTS_HEAD, [ip + 3]
    add [rb + import], 0, [0]

    arb 2
    ret 1
.ENDFRAME

##########
create_export:
.FRAME module; export, tmp
    arb -2

    # allocate a block
    add EXPORT_SIZE, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + export]

    # initialize to zeros
    add [rb + export], 0, [rb - 1]
    add EXPORT_SIZE, 0, [rb - 2]
    arb -2
    call zeromem

    # save module pointer
    add [rb + export], EXPORT_MODULE, [ip + 3]
    add [rb + module], 0, [0]

    # default export address is -1, not 0
    add [rb + export], EXPORT_ADDRESS, [ip + 3]
    add -1, 0, [0]

    # add to the head of doubly-linked list
    add [rb + module], MODULE_EXPORTS_HEAD, [ip + 1]
    add [0], 0, [rb + tmp]

    add [rb + export], EXPORT_NEXT_PTR, [ip + 3]
    add [rb + tmp], 0, [0]

    add [rb + export], EXPORT_PREV_PTR, [ip + 3]
    add 0, 0, [0]

    add [rb + module], MODULE_EXPORTS_HEAD, [ip + 3]
    add [rb + export], 0, [0]

    arb 2
    ret 1
.ENDFRAME

##########
# globals

# loaded modules
module_head:
    db 0
module_tail:
    db 0

# included symbols
symbol_head:
    db 0
symbol_tail:
    db 0

.EOF
