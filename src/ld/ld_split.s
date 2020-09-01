.EXPORT include_objects
.EXPORT resolve_symbols
.EXPORT relocate
.EXPORT connect_imports
.EXPORT print_modules
#.EXPORT dump_symbols
.EXPORT module_head
.EXPORT module_tail

# from libxib/print.s
.IMPORT print_num
.IMPORT print_str

# from libxib/error.s
.IMPORT report_error_at_location
.IMPORT report_error_with_symbol

# from libxib/heap.s
.IMPORT alloc

# from libxib/string.s
.IMPORT is_digit
.IMPORT is_alphanum
.IMPORT strcmp
.IMPORT zeromem

# from memory.s
.IMPORT inc_mem
.IMPORT print_mem

##########
include_objects:
.FRAME module, tmp
    arb -2

    # process all modules
    add [module_head], 0, [rb + module]

include_objects_loop:
    jz  [rb + module], include_objects_done

    add [rb + module], MODULE_NEEDED, [ip + 1]
    jz  [0], include_objects_next

    add [rb + module], MODULE_INCLUDED, [ip + 1]
    jnz [0], include_objects_next

    # module is needed, but not yet included
    add [rb + module], 0, [rb - 1]
    arb -1
    call include_module

include_objects_next:
    add [rb + module], MODULE_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + module]

    jz  0, include_objects_loop

include_objects_done:
    arb 2
    ret 0
.ENDFRAME

##########
resolve_symbols:
.FRAME symbol, tmp
    arb -2

    # process all symbols that don't yet have an exporting module
    add [symbol_head], 0, [rb + symbol]

resolve_symbols_loop:
    jz  [rb + symbol], resolve_symbols_done

    add [rb + symbol], EXPORT_MODULE, [ip + 1]
    jnz [0], resolve_symbols_next

    add [rb + symbol], 0, [rb - 1]
    arb -1
    call resolve_one_symbol

resolve_symbols_next:
    add [rb + symbol], EXPORT_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + symbol]

    jnz [rb + symbol], resolve_symbols_loop

resolve_symbols_done:
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
    jnz [rb + module], resolve_one_symbol_have_module

    add err_export_not_found, 0, [rb + 1]
    add [rb + symbol], EXPORT_IDENTIFIER, [ip + 1]
    add [0], 0, [rb]
    call report_error_with_symbol

resolve_one_symbol_have_module:
    # include that module
    add [rb + module], 0, [rb - 1]
    arb -1
    call include_module

    # sanity check, including that module should have resolved this symbol
    add [rb + symbol], EXPORT_MODULE, [ip + 1]
    jnz [0], resolve_one_symbol_done

    add err_included_not_resolved, 0, [rb + 1]
    add [rb + symbol], EXPORT_IDENTIFIER, [ip + 1]
    add [0], 0, [rb]
    call report_error_with_symbol

resolve_one_symbol_done:
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

include_exported_loop:
    jz  [rb + exported_head], include_exported_done

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
    jz  [rb + symbol], include_exported_is_new

    # yes, but it should not be exported from any module
    add [rb + symbol], EXPORT_MODULE, [ip + 1]
    jz  [0], include_exported_have_symbol

    add err_duplicate_export, 0, [rb + 1]
    add [rb + export], EXPORT_IDENTIFIER, [ip + 1]
    add [0], 0, [rb]
    call report_error_with_symbol

include_exported_is_new:
    # reuse the export as an included symbol
    add [rb + export], 0, [rb + symbol]

    # append this export to list of included symbols
    add [rb + symbol], 0, [rb - 1]
    add symbol_head, 0, [rb - 2]
    add symbol_tail, 0, [rb - 3]
    arb -3
    call append_double_linked

    jz  0, include_exported_loop

include_exported_have_symbol:
    # update existing symbol with information from the export
    add [rb + export], EXPORT_MODULE, [ip + 1]
    add [0], 0, [rb + tmp]
    add [rb + symbol], EXPORT_MODULE, [ip + 3]
    add [rb + tmp], 0, [0]

    add [rb + export], EXPORT_ADDRESS, [ip + 1]
    add [0], 0, [rb + tmp]
    add [rb + symbol], EXPORT_ADDRESS, [ip + 3]
    add [rb + tmp], 0, [0]

    jz  0, include_exported_loop

include_exported_done:
    arb 3
    ret 1
.ENDFRAME

##########
include_imported:
.FRAME imported_head; import, symbol, tmp
    arb -3

    add [rb + imported_head], 0, [rb + import]

include_imported_loop:
    jz  [rb + import], include_imported_done

    # do we have this symbol already included?
    add [rb + import], IMPORT_IDENTIFIER, [ip + 1]
    add [0], 0, [rb - 1]
    add [symbol_head], 0, [rb - 2]
    arb -2
    call find_symbol

    add [rb - 4], 0, [rb + symbol]
    jnz [rb + symbol], include_imported_have_symbol

    # no, create a new included symbol
    add EXPORT_SIZE, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + symbol]

    # initialize to zeros
    add [rb + symbol], 0, [rb - 1]
    add EXPORT_SIZE, 0, [rb - 2]
    arb -2
    call zeromem

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

include_imported_have_symbol:
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

    jz  0, include_imported_loop

include_imported_done:
    arb 3
    ret 1
.ENDFRAME

##########
find_exporting_module:
.FRAME identifier; module, tmp
    arb -2

    # process all modules not yet included
    add [module_head], 0, [rb + module]

find_exporting_module_loop:
    jz  [rb + module], find_exporting_module_done

    add [rb + module], MODULE_INCLUDED, [ip + 1]
    jnz [0], find_exporting_module_next

    # is the symbol exported from this module
    add [rb + identifier], 0, [rb - 1]
    add [rb + module], MODULE_EXPORTS_HEAD, [ip + 1]
    add [0], 0, [rb - 2]
    arb -2
    call find_symbol

    jnz [rb - 4], find_exporting_module_done

find_exporting_module_next:
    add [rb + module], MODULE_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + module]

    jz  0, find_exporting_module_loop

find_exporting_module_done:
    # result is in [rb + module]
    arb 2
    ret 1
.ENDFRAME

##########
find_symbol:
.FRAME identifier, head; symbol
    arb -1

    add [rb + head], 0, [rb + symbol]

find_resolved_loop:
    jz  [rb + symbol], find_resolved_done

    add [rb + symbol], EXPORT_IDENTIFIER, [ip + 1]
    add [0], 0, [rb - 1]
    add [rb + identifier], 0, [rb - 2]
    arb -2
    call strcmp

    jz  [rb - 4], find_resolved_done

    add [rb + symbol], EXPORT_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + symbol]

    jnz [rb + symbol], find_resolved_loop

find_resolved_done:
    # result is in [rb + symbol]
    arb 1
    ret 2
.ENDFRAME

##########
relocate:
.FRAME module, address, tmp
    arb -3

    add [module_head], 0, [rb + module]
    add 0, 0, [rb + address]

relocate_loop:
    jz  [rb + module], relocate_done

    # skip modules that are not included
    add [rb + module], MODULE_INCLUDED, [ip + 1]
    jz  [0], relocate_next

    # set module address
    add [rb + module], MODULE_ADDRESS, [ip + 3]
    add [rb + address], 0, [0]

    # relocate the module
    add [rb + module], 0, [rb - 1]
    arb -1
    call relocate_module

    # add module size to the address
    add [rb + module], MODULE_CODE_LENGTH, [ip + 1]
    add [0], [rb + address], [rb + address]

relocate_next:
    add [rb + module], MODULE_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + module]

    jz  0, relocate_loop

 relocate_done:
    arb 3
    ret 0
.ENDFRAME

##########
relocate_module:
.FRAME module; buffer, limit, index, tmp
    arb -4

    add [rb + module], MODULE_RELOC_HEAD, [ip + 1]
    add [0], 0, [rb + buffer]

relocate_module_block:
    jz  [rb + buffer], relocate_module_done
    add 1, 0, [rb + index]

    # maximum index within a block is MEM_BLOCK_SIZE, except for last block
    add MEM_BLOCK_SIZE, 0, [rb + limit]
    add [rb + module], MODULE_RELOC_TAIL, [ip + 1]
    eq  [0], [rb + buffer], [rb + tmp]
    jz  [rb + tmp], relocate_module_byte

    add [rb + module], MODULE_RELOC_INDEX, [ip + 1]
    add [0], 0, [rb + limit]

relocate_module_byte:
    lt  [rb + index], [rb + limit], [rb + tmp]
    jz  [rb + tmp], relocate_module_block_done

    # increment memory at relocation address by module address
    add [rb + module], MODULE_CODE_HEAD, [ip + 1]
    add [0], 0, [rb - 1]
    add [rb + module], MODULE_CODE_TAIL, [ip + 1]
    add [0], 0, [rb - 2]
    add [rb + module], MODULE_CODE_INDEX, [ip + 1]
    add [0], 0, [rb - 3]
    add [rb + buffer], [rb + index], [ip + 1]
    add [0], 0, [rb - 4]
    add [rb + module], MODULE_ADDRESS, [ip + 1]
    add [0], 0, [rb - 5]
    arb -5
    call inc_mem

    add [rb + index], 1, [rb + index]
    jz  0, relocate_module_byte

relocate_module_block_done:
    # next block in linked list
    add [rb + buffer], 0, [ip + 1]
    add [0], 0, [rb + buffer]

    jz  0, relocate_module_block

relocate_module_done:
    arb 4
    ret 1
.ENDFRAME

##########
connect_imports:
.FRAME symbol, import, tmp
    arb -3

    add [symbol_head], 0, [rb + symbol]

connect_imports_symbols_loop:
    jz  [rb + symbol], connect_imports_symbols_done

    add [rb + symbol], EXPORT_IMPORTS_HEAD, [ip + 1]
    add [0], 0, [rb + import]

connect_imports_imports_loop:
    jz  [rb + import], connect_imports_imports_done

    add [rb + symbol], 0, [rb - 1]
    add [rb + import], 0, [rb - 2]
    arb -2
    call connect_one_import

    add [rb + import], IMPORT_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + import]

    jz  0, connect_imports_imports_loop

connect_imports_imports_done:
    add [rb + symbol], EXPORT_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + symbol]

    jz  0, connect_imports_symbols_loop

connect_imports_symbols_done:
    arb 3
    ret 0
.ENDFRAME

##########
connect_one_import:
.FRAME symbol, import; buffer, limit, index, import_module, symbol_address, tmp
    arb -6

    add [rb + import], IMPORT_MODULE, [ip + 1]
    add [0], 0, [rb + import_module]

    # symbol address = symbol module address + symbol address within module
    add [rb + symbol], EXPORT_MODULE, [ip + 1]
    add [0], MODULE_ADDRESS, [ip + 1]
    add [0], 0, [rb + symbol_address]
    add [rb + symbol], EXPORT_ADDRESS, [ip + 1]
    add [0], [rb + symbol_address], [rb + symbol_address]

    add [rb + import], IMPORT_FIXUPS_HEAD, [ip + 1]
    add [0], 0, [rb + buffer]

connect_one_import_block:
    jz  [rb + buffer], connect_one_import_done
    add 1, 0, [rb + index]

    # maximum index within a block is MEM_BLOCK_SIZE, except for last block
    add MEM_BLOCK_SIZE, 0, [rb + limit]
    add [rb + import], IMPORT_FIXUPS_TAIL, [ip + 1]
    eq  [0], [rb + buffer], [rb + tmp]
    jz  [rb + tmp], connect_one_import_byte

    add [rb + import], IMPORT_FIXUPS_INDEX, [ip + 1]
    add [0], 0, [rb + limit]

connect_one_import_byte:
    lt  [rb + index], [rb + limit], [rb + tmp]
    jz  [rb + tmp], connect_one_import_block_done

    # increment memory at in import module at import fixup address
    # by export module address + export symbol address
    #
    # for (fixup in import.fixups) {
    #     inc_mem(import.module.code, fixup, export.module.address + export.address)
    # }

    add [rb + import_module], MODULE_CODE_HEAD, [ip + 1]
    add [0], 0, [rb - 1]
    add [rb + import_module], MODULE_CODE_TAIL, [ip + 1]
    add [0], 0, [rb - 2]
    add [rb + import_module], MODULE_CODE_INDEX, [ip + 1]
    add [0], 0, [rb - 3]
    add [rb + buffer], [rb + index], [ip + 1]
    add [0], 0, [rb - 4]
    add [rb + symbol_address], 0, [rb - 5]
    arb -5
    call inc_mem

    add [rb + index], 1, [rb + index]
    jz  0, connect_one_import_byte

connect_one_import_block_done:
    # next block in linked list
    add [rb + buffer], 0, [ip + 1]
    add [0], 0, [rb + buffer]

    jz  0, connect_one_import_block

connect_one_import_done:
    arb 6
    ret 2
.ENDFRAME

.SYMBOL DOUBLE_LINKED_NEXT_PTR 0
.SYMBOL DOUBLE_LINKED_PREV_PTR 1

##########
append_double_linked:
.FRAME item, head_ptr, tail_ptr; tmp
    arb -1

    add [rb + head_ptr], 0, [ip + 1]
    jnz [0], append_double_linked_have_head

    # empty list, set head
    add [rb + head_ptr], 0, [ip + 3]
    add [rb + item], 0, [0]

    jz  0, append_double_linked_common

append_double_linked_have_head:
    # non-empty list, set next item in tail
    add [rb + tail_ptr], 0, [ip + 1]
    add [0], DOUBLE_LINKED_NEXT_PTR, [ip + 3]
    add [rb + item], 0, [0]

append_double_linked_common:
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
print_modules:
.FRAME module, first, tmp
    arb -3

    # print all included modules
    add [module_head], 0, [rb + module]
    add 1, 0, [rb + first]

print_modules_loop:
    jz  [rb + module], print_modules_done

    add [rb + module], MODULE_INCLUDED, [ip + 1]
    jz  [0], print_modules_next
    add [rb + module], MODULE_CODE_HEAD, [ip + 1]
    jz  [0], print_modules_next

    jnz [rb + first], print_modules_skip_comma
    out ','

print_modules_skip_comma:
    add 0, 0, [rb + first]

    add [rb + module], MODULE_CODE_HEAD, [ip + 1]
    add [0], 0, [rb - 1]
    add [rb + module], MODULE_CODE_TAIL, [ip + 1]
    add [0], 0, [rb - 2]
    add [rb + module], MODULE_CODE_INDEX, [ip + 1]
    add [0], 0, [rb - 3]
    arb -3
    call print_mem

print_modules_next:
    add [rb + module], MODULE_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + module]

    jz  0, print_modules_loop

print_modules_done:
    out 10

    arb 3
    ret 0
.ENDFRAME

##########
dump_symbols:
.FRAME module, symbol, import, tmp
    arb -4

    add [module_head], 0, [rb + module]

dump_symbols_modules_loop:
    jz  [rb + module], dump_symbols_modules_done

    add [rb + module], 0, [rb - 1]
    arb -1
    call print_num
    out ' '

    add [rb + module], MODULE_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + module]

    jz  0, dump_symbols_modules_loop

dump_symbols_modules_done:
    add [symbol_head], 0, [rb + symbol]

dump_symbols_symbols_loop:
    jz  [rb + symbol], dump_symbols_symbols_done

    add [rb + symbol], EXPORT_IDENTIFIER, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_str

    add dump_symbols_str_export_mod_start, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + symbol], EXPORT_MODULE, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_num

    add dump_symbols_str_export_mod_end, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + symbol], EXPORT_IMPORTS_HEAD, [ip + 1]
    add [0], 0, [rb + import]

dump_symbols_imports_loop:
    jz  [rb + import], dump_symbols_imports_done

    add dump_symbols_str_import_mod_start, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + import], IMPORT_MODULE, [ip + 1]
    add [0], 0, [rb - 1]
    arb -1
    call print_num

    add dump_symbols_str_import_mod_end, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + import], IMPORT_FIXUPS_HEAD, [ip + 1]
    add [0], 0, [rb - 1]
    add [rb + import], IMPORT_FIXUPS_TAIL, [ip + 1]
    add [0], 0, [rb - 2]
    add [rb + import], IMPORT_FIXUPS_INDEX, [ip + 1]
    add [0], 0, [rb - 3]
    arb -3
    call print_mem

    add dump_symbols_str_import_end, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + import], IMPORT_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + import]

    jz  0, dump_symbols_imports_loop

dump_symbols_imports_done:
    add [rb + symbol], EXPORT_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + symbol]

    out 10

    jz  0, dump_symbols_symbols_loop

dump_symbols_symbols_done:
    arb 4
    ret 0

dump_symbols_str_export_mod_start:
    db  " (", 0
dump_symbols_str_export_mod_end:
    db  "):", 0
dump_symbols_str_import_mod_start:
    db  " [", 0
dump_symbols_str_import_mod_end:
    db  "] {", 0
dump_symbols_str_import_end:
    db  "}", 0
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

##########
# error messages

err_duplicate_export:
    db  "Duplicate exported symbol", 0
err_export_not_found:
    db  "Exported symbol not found", 0
err_included_not_resolved:
    db  "Symbol not resolved by including its module", 0

.EOF
