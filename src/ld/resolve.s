.EXPORT include_objects
.EXPORT resolve_symbols

# from libxib/error.s
.IMPORT report_error_with_symbol

# from libxib/heap.s
.IMPORT alloc_blocks
.IMPORT zeromem_blocks

# from libxib/string.s
.IMPORT strcmp

# from data.s
.IMPORT module_head
.IMPORT symbol_head
.IMPORT symbol_tail

##########
include_objects:
.FRAME module, tmp
    arb -2

    # process all modules
    add [module_head], 0, [rb + module]

.loop:
    jz  [rb + module], .done

    add [rb + module], MODULE_NEEDED, [ip + 1]
    jz  [0], .next

    add [rb + module], MODULE_INCLUDED, [ip + 1]
    jnz [0], .next

    # module is needed, but not yet included
    add [rb + module], 0, [rb - 1]
    arb -1
    call include_module

.next:
    add [rb + module], MODULE_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + module]

    jz  0, .loop

.done:
    arb 2
    ret 0
.ENDFRAME

##########
resolve_symbols:
.FRAME symbol, tmp
    arb -2

    # process all symbols that don't yet have an exporting module
    add [symbol_head], 0, [rb + symbol]

.loop:
    jz  [rb + symbol], .done

    add [rb + symbol], EXPORT_MODULE, [ip + 1]
    jnz [0], .next

    add [rb + symbol], 0, [rb - 1]
    arb -1
    call resolve_one_symbol

.next:
    add [rb + symbol], EXPORT_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + symbol]

    jnz [rb + symbol], .loop

.done:
    arb 2
    ret 0
.ENDFRAME

##########
resolve_one_symbol:
.FRAME symbol; module, tmp
    arb -2

    # find first module that exports this symbol
    add [rb + symbol], EXPORT_IDENTIFIER, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call find_exporting_module

    add [rb - 3], 0, [rb + module]
    jnz [rb + module], .have_module

    add err_export_not_found, 0, [rb + 1]
    add [rb + symbol], EXPORT_IDENTIFIER, [ip + 1]
    add [0], 0, [rb]
    call report_error_with_symbol

.have_module:
    # include that module
    add [rb + module], 0, [rb - 1]
    arb -1
    call include_module

    # sanity check, including that module should have resolved this symbol
    add [rb + symbol], EXPORT_MODULE, [ip + 1]
    jnz [0], .done

    add err_included_not_resolved, 0, [rb + 1]
    add [rb + symbol], EXPORT_IDENTIFIER, [ip + 1]
    add [0], 0, [rb]
    call report_error_with_symbol

.done:
    arb 2
    ret 1
.ENDFRAME

##########
include_module:
.FRAME module; tmp
    arb -1

    # process exported
    add [rb + module], MODULE_EXPORTS_HEAD, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call include_exported

    # process imported
    add [rb + module], MODULE_IMPORTS_HEAD, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call include_imported

    # set the module as included
    add [rb + module], MODULE_INCLUDED, [ip + 3]
    add 1, 0, [0]

    arb 1
    ret 1
.ENDFRAME

##########
include_exported:
.FRAME exported_head; export, symbol, tmp
    arb -3

.loop:
    jz  [rb + exported_head], .done

    # move to next exported symbol
    add [rb + exported_head], 0, [rb + export]
    add [rb + exported_head], EXPORT_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + exported_head]

    # do we have this symbol already included?
    add [rb + export], EXPORT_IDENTIFIER, [ip + 1]
    add [0], 0, [rb - 1]
    add [symbol_head], 0, [rb - 2]
    arb -2
    call find_symbol

    add [rb - 4], 0, [rb + symbol]
    jz  [rb + symbol], .is_new

    # yes, but it should not be exported from any module
    add [rb + symbol], EXPORT_MODULE, [ip + 1]
    jz  [0], .have_symbol

    add err_duplicate_export, 0, [rb + 1]
    add [rb + export], EXPORT_IDENTIFIER, [ip + 1]
    add [0], 0, [rb]
    call report_error_with_symbol

.is_new:
    # reuse the export as an included symbol
    add [rb + export], 0, [rb + symbol]

    # append this export to list of included symbols
    add [rb + symbol], 0, [rb - 1]
    add symbol_head, 0, [rb - 2]
    add symbol_tail, 0, [rb - 3]
    arb -3
    call append_double_linked

    jz  0, .loop

.have_symbol:
    # update existing symbol with information from the export
    add [rb + export], EXPORT_MODULE, [ip + 1]
    add [0], 0, [rb + tmp]
    add [rb + symbol], EXPORT_MODULE, [ip + 3]
    add [rb + tmp], 0, [0]

    add [rb + export], EXPORT_ADDRESS, [ip + 1]
    add [0], 0, [rb + tmp]
    add [rb + symbol], EXPORT_ADDRESS, [ip + 3]
    add [rb + tmp], 0, [0]

    jz  0, .loop

.done:
    arb 3
    ret 1
.ENDFRAME

##########
include_imported:
.FRAME imported_head; import, symbol, tmp
    arb -3

    add [rb + imported_head], 0, [rb + import]

.loop:
    jz  [rb + import], .done

    # do we have this symbol already included?
    add [rb + import], IMPORT_IDENTIFIER, [ip + 1]
    add [0], 0, [rb - 1]
    add [symbol_head], 0, [rb - 2]
    arb -2
    call find_symbol

    add [rb - 4], 0, [rb + symbol]
    jnz [rb + symbol], .have_symbol

    # no, create a new included symbol
    add EXPORT_ALLOC_SIZE, 0, [rb - 1]
    arb -1
    call alloc_blocks
    add [rb - 3], 0, [rb + symbol]

    # initialize to zeros
    add [rb + symbol], 0, [rb - 1]
    add EXPORT_ALLOC_SIZE, 0, [rb - 2]
    arb -2
    call zeromem_blocks

    # default symbol address is -1, not 0
    add [rb + symbol], EXPORT_ADDRESS, [ip + 3]
    add -1, 0, [0]

    # save identifier (reusing the existing string)
    add [rb + import], IMPORT_IDENTIFIER, [ip + 1]
    add [0], 0, [rb + tmp]
    add [rb + symbol], EXPORT_IDENTIFIER, [ip + 3]
    add [rb + tmp], 0, [0]
    add [rb + import], IMPORT_IDENTIFIER, [ip + 3]
    add 0, 0, [0]

    # append this symbol to list of symbols
    add [rb + symbol], 0, [rb - 1]
    add symbol_head, 0, [rb - 2]
    add symbol_tail, 0, [rb - 3]
    arb -3
    call append_double_linked

.have_symbol:
    # move to next imported symbol, saving the current one in [rb + tmp]
    add [rb + import], 0, [rb + tmp]
    add [rb + import], IMPORT_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + import]

    # add this import to list of imports for this symbol
    add [rb + tmp], 0, [rb - 1]
    add [rb + symbol], EXPORT_IMPORTS_HEAD, [rb - 2]
    add [rb + symbol], EXPORT_IMPORTS_TAIL, [rb - 3]
    arb -3
    call append_double_linked

    jz  0, .loop

.done:
    arb 3
    ret 1
.ENDFRAME

##########
find_exporting_module:
.FRAME identifier; module, tmp
    arb -2

    # process all modules not yet included
    add [module_head], 0, [rb + module]

.loop:
    jz  [rb + module], .done

    add [rb + module], MODULE_INCLUDED, [ip + 1]
    jnz [0], .next

    # is the symbol exported from this module
    add [rb + identifier], 0, [rb - 1]
    add [rb + module], MODULE_EXPORTS_HEAD, [ip + 1]
    add [0], 0, [rb - 2]
    arb -2
    call find_symbol

    jnz [rb - 4], .done

.next:
    add [rb + module], MODULE_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + module]

    jz  0, .loop

.done:
    # result is in [rb + module]
    arb 2
    ret 1
.ENDFRAME

##########
find_symbol:
.FRAME identifier, head; symbol
    arb -1

    add [rb + head], 0, [rb + symbol]

.loop:
    jz  [rb + symbol], .done

    add [rb + symbol], EXPORT_IDENTIFIER, [ip + 1]
    add [0], 0, [rb - 1]
    add [rb + identifier], 0, [rb - 2]
    arb -2
    call strcmp

    jz  [rb - 4], .done

    add [rb + symbol], EXPORT_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + symbol]

    jnz [rb + symbol], .loop

.done:
    # result is in [rb + symbol]
    arb 1
    ret 2
.ENDFRAME

.SYMBOL DOUBLE_LINKED_NEXT_PTR 0
.SYMBOL DOUBLE_LINKED_PREV_PTR 1

##########
append_double_linked:
.FRAME item, head_ptr, tail_ptr; tmp
    arb -1

    add [rb + head_ptr], 0, [ip + 1]
    jnz [0], .have_head

    # empty list, set head
    add [rb + head_ptr], 0, [ip + 3]
    add [rb + item], 0, [0]

    jz  0, .common

.have_head:
    # non-empty list, set next item in tail
    add [rb + tail_ptr], 0, [ip + 1]
    add [0], DOUBLE_LINKED_NEXT_PTR, [ip + 3]
    add [rb + item], 0, [0]

.common:
    # set our prev item to old tail
    add [rb + item], DOUBLE_LINKED_PREV_PTR, [ip + 3]
    add [rb + tail_ptr], 0, [0]

    # set our next item to 0
    add [rb + item], DOUBLE_LINKED_NEXT_PTR, [ip + 3]
    add 0, 0, [0]

    # set tail
    add [rb + tail_ptr], 0, [ip + 3]
    add [rb + item], 0, [0]

    arb 1
    ret 3
.ENDFRAME

##########
# error messages

err_duplicate_export:
    db  "Duplicate exported symbol", 0
err_export_not_found:
    db  "Exported symbol not found", 0
err_included_not_resolved:
    db  "Symbol not resolved by including its module", 0

.EOF
