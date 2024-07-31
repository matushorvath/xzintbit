.EXPORT create_module
.EXPORT create_import
.EXPORT create_export
.EXPORT create_symbol

.EXPORT module_head
.EXPORT module_tail
.EXPORT resolved_head
.EXPORT resolved_tail

# from libxib/heap.s
.IMPORT alloc_blocks
.IMPORT zeromem_blocks

##########
create_module:
.FRAME is_library; module
    arb -1

    # allocate a block
    add MODULE_ALLOC_SIZE, 0, [rb - 1]
    arb -1
    call alloc_blocks
    add [rb - 3], 0, [rb + module]

    # initialize to zeros
    add [rb + module], 0, [rb - 1]
    add MODULE_ALLOC_SIZE, 0, [rb - 2]
    arb -2
    call zeromem_blocks

    # object files are mandatory, libraries are optional
    add [rb + module], MODULE_NEEDED, [ip + 3]
    eq  [rb + is_library], 0, [0]

    # append to the tail if any
    jz  [module_tail], .is_first

    add [module_tail], MODULE_NEXT_PTR, [ip + 3]
    add [rb + module], 0, [0]
    add [rb + module], 0, [module_tail]

    jz  0, .done

.is_first:
    add [rb + module], 0, [module_head]
    add [rb + module], 0, [module_tail]

.done:
    arb 1
    ret 1
.ENDFRAME

##########
create_import:
.FRAME module; import, tmp
    arb -2

    # allocate a block
    add IMPORT_ALLOC_SIZE, 0, [rb - 1]
    arb -1
    call alloc_blocks
    add [rb - 3], 0, [rb + import]

    # initialize to zeros
    add [rb + import], 0, [rb - 1]
    add IMPORT_ALLOC_SIZE, 0, [rb - 2]
    arb -2
    call zeromem_blocks

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
    add EXPORT_ALLOC_SIZE, 0, [rb - 1]
    arb -1
    call alloc_blocks
    add [rb - 3], 0, [rb + export]

    # initialize to zeros
    add [rb + export], 0, [rb - 1]
    add EXPORT_ALLOC_SIZE, 0, [rb - 2]
    arb -2
    call zeromem_blocks

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
create_symbol:
.FRAME module; symbol, tmp
    arb -2

    # allocate a block
    add SYMBOL_ALLOC_SIZE, 0, [rb - 1]
    arb -1
    call alloc_blocks
    add [rb - 3], 0, [rb + symbol]

    # initialize to zeros
    add [rb + symbol], 0, [rb - 1]
    add SYMBOL_ALLOC_SIZE, 0, [rb - 2]
    arb -2
    call zeromem_blocks

    # save module pointer
    add [rb + symbol], SYMBOL_MODULE, [ip + 3]
    add [rb + module], 0, [0]

    # default address is -1, not 0
    add [rb + symbol], SYMBOL_ADDRESS, [ip + 3]
    add -1, 0, [0]

    # add to the head of doubly-linked list
    add [rb + module], MODULE_SYMBOLS_HEAD, [ip + 1]
    add [0], 0, [rb + tmp]

    add [rb + symbol], SYMBOL_NEXT_PTR, [ip + 3]
    add [rb + tmp], 0, [0]

    add [rb + symbol], SYMBOL_PREV_PTR, [ip + 3]
    add 0, 0, [0]

    add [rb + module], MODULE_SYMBOLS_HEAD, [ip + 3]
    add [rb + symbol], 0, [0]

    arb 2
    ret 1
.ENDFRAME

##########
# globals

# loaded modules
module_head:
    db  0
module_tail:
    db  0

# resolved exports and imports
resolved_head:
    db  0
resolved_tail:
    db  0

.EOF
