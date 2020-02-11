# from print.s
.IMPORT print_num
.IMPORT print_str

# from error.s
.IMPORT report_error_at_location
.IMPORT report_error_with_symbol

# from heap.s
.IMPORT alloc
.IMPORT free

# from string.s
.IMPORT is_digit
.IMPORT is_alphanum
.IMPORT strcmp
.IMPORT zeromem

##########
    arb stack

##########
link:
.FRAME module, tmp
    arb -2

link_loop:
    # check for more files
    call expect_next_file
    eq  [rb - 2], '$', [rb + tmp]
    jnz [rb + tmp], link_load_done

    # create a module
    call create_module
    add [rb - 2], 0, [rb + module]

    # we have .C, load the code
    add [rb + module], 0, [rb - 1]
    arb -1
    call load_code

    # next expect .R
    add 'R', 0, [rb - 1]
    add err_expect_dot_r, 0, [rb - 2]
    arb -2
    call expect_directive

    # we have .R, load addresses for relocation
    add [rb + module], 0, [rb - 1]
    arb -1
    call load_relocated

    # next expect .I
    add 'I', 0, [rb - 1]
    add err_expect_dot_i, 0, [rb - 2]
    arb -2
    call expect_directive

    # we have .I, load imported symbols
    add [rb + module], 0, [rb - 1]
    arb -1
    call load_imported

    # next expect .E
    add 'E', 0, [rb - 1]
    add err_expect_dot_e, 0, [rb - 2]
    arb -2
    call expect_directive

    # we have .E, load exported symbols
    add [rb + module], 0, [rb - 1]
    arb -1
    call load_exported

    jz  0, link_loop

link_load_done:
    call add_linker_symbols
    call include_objects
    call resolve_symbols
    call relocate
    call connect_imports
    call print_modules

    #call dump_symbols

    hlt
.ENDFRAME

##########
get_input:
.FRAME char, tmp
    arb -2

    # get input from the buffer if we have it
    add [input_buffer], 0, [rb + char]
    add 0, 0, [input_buffer]

    jnz [rb + char], get_input_have_char
    in  [rb + char]

get_input_have_char:
    # track line and column number
    eq  [rb + char], 10, [rb + tmp]
    jz  [rb + tmp], get_input_same_line

    # we have a new line
    add [input_line_num], 1, [input_line_num]
    add [input_column_num], 0, [input_prev_column_num]
    add 1, 0, [input_column_num]
    jz  0, get_input_done

get_input_same_line:
    # we are on the same line
    add [input_column_num], 1, [input_column_num]

get_input_done:
    arb 2
    ret 0
.ENDFRAME

##########
unget_input:
.FRAME char; tmp
    arb -1

    # "unget" an unused char so we get it again later
    add [rb + char], 0, [input_buffer]

    # track line and column number
    eq  [rb + char], 10, [rb + tmp]
    jz  [rb + tmp], unget_input_same_line

    # we moved back to previous line, use [input_prev_column_num] as column num
    add [input_line_num], -1, [input_line_num]
    add [input_prev_column_num], 0, [input_column_num]
    jz  0, unget_input_done

unget_input_same_line:
    add [input_column_num], -1, [input_column_num]

unget_input_done:
    arb 1
    ret 1
.ENDFRAME

##########
expect_next_file:
.FRAME char, tmp
    arb -2

    add err_expect_dot_c_l_at, 0, [rb - 1]
    arb -1
    call read_directive
    add [rb - 3], 0, [rb + char]

    # object files begin with a .C, libraries begin with a .L, after the last file we expect a .$
    eq  [rb + char], '$', [rb + tmp]
    jnz [rb + tmp], expect_next_file_done
    eq  [rb + char], 'C', [rb + tmp]
    jnz [rb + tmp], expect_next_file_done
    eq  [rb + char], 'L', [rb + tmp]
    jnz [rb + tmp], expect_next_file_library

    add err_expect_dot_c_l_at, 0, [rb]
    call report_error

expect_next_file_library:
    # mark that we are now processing libraries
    add 1, 0, [is_library]

    add err_expect_dot_c_at, 0, [rb - 1]
    arb -1
    call read_directive
    add [rb - 3], 0, [rb + char]

    # we are already in a library, so now we only accept a .C and .$
    eq  [rb + char], '$', [rb + tmp]
    jnz [rb + tmp], expect_next_file_done
    eq  [rb + char], 'C', [rb + tmp]
    jnz [rb + tmp], expect_next_file_done

    add err_expect_dot_c_at, 0, [rb]
    call report_error

expect_next_file_done:
    # return [rb + char]

    arb 2
    ret 0
.ENDFRAME

##########
expect_directive:
.FRAME directive, error_message; char, tmp
    arb -2

    add [rb + error_message], 0, [rb - 1]
    arb -1
    call read_directive

    eq  [rb - 3], [rb + directive], [rb + tmp]
    jz  [rb + tmp], expect_directive_error

    arb 2
    ret 2

expect_directive_error:
    add [rb + error_message], 0, [rb]
    call report_error
.ENDFRAME

##########
read_directive:
.FRAME error_message; char, tmp
    arb -2

    call get_input
    add [rb - 2], 0, [rb + char]

    eq  [rb + char], '.', [rb + tmp]
    jz  [rb + tmp], read_directive_error

    call get_input
    add [rb - 2], 0, [rb + char]

    call get_input
    add [rb - 2], 0, [rb + tmp]

    eq  [rb + tmp], 10, [rb + tmp]
    jz  [rb + tmp], read_directive_error

    # return [rb + char]

    arb 2
    ret 1

read_directive_error:
    add [rb + error_message], 0, [rb]
    call report_error
.ENDFRAME

##########
create_module:
.FRAME module
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
    eq  [is_library], 0, [0]

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
    ret 0
.ENDFRAME

##########
load_code:
.FRAME module; byte, char, length, tmp
    arb -4

    add 0, 0, [rb + length]

load_code_loop:
    call read_number
    add [rb - 2], 0, [rb + byte]
    add [rb - 3], 0, [rb + char]

    # store the byte
    add [rb + module], MODULE_CODE_HEAD, [rb - 1]
    add [rb + module], MODULE_CODE_TAIL, [rb - 2]
    add [rb + module], MODULE_CODE_INDEX, [rb - 3]
    add [rb + byte], 0, [rb - 4]
    arb -4
    call set_mem

    add [rb + length], 1, [rb + length]

    # next character should be comma or line end
    eq  [rb + char], ',', [rb + tmp]
    jnz [rb + tmp], load_code_loop
    eq  [rb + char], 10, [rb + tmp]
    jnz [rb + tmp], load_code_done

    add err_expect_comma_eol, 0, [rb]
    call report_error

load_code_done:
    add [rb + module], MODULE_CODE_LENGTH, [ip + 3]
    add [rb + length], 0, [0]

    arb 4
    ret 1
.ENDFRAME

##########
load_relocated:
.FRAME module; byte, char, tmp
    arb -3

    # peek one character to see if we have relocated data at all
    call get_input
    add [rb - 2], 0, [rb + char]
    add [rb + char], 0, [rb - 1]
    arb -1
    call unget_input

    eq  [rb + char], '.', [rb + tmp]
    jnz [rb + tmp], load_relocated_done

load_relocated_loop:
    call read_number
    add [rb - 2], 0, [rb + byte]
    add [rb - 3], 0, [rb + char]

    # store the byte
    add [rb + module], MODULE_RELOC_HEAD, [rb - 1]
    add [rb + module], MODULE_RELOC_TAIL, [rb - 2]
    add [rb + module], MODULE_RELOC_INDEX, [rb - 3]
    add [rb + byte], 0, [rb - 4]
    arb -4
    call set_mem

    # next character should be comma or line end
    eq  [rb + char], ',', [rb + tmp]
    jnz [rb + tmp], load_relocated_loop
    eq  [rb + char], 10, [rb + tmp]
    jnz [rb + tmp], load_relocated_done

    add err_expect_comma_eol, 0, [rb]
    call report_error

load_relocated_done:
    arb 3
    ret 1
.ENDFRAME

##########
load_imported:
.FRAME module; import, identifier, byte, char, tmp
    arb -5

load_imported_loop:
    # read the identifier
    call read_identifier
    add [rb - 2], 0, [rb + identifier]
    add [rb - 3], 0, [rb + char]

    # if there is no identifier, finish
    add [rb + identifier], 0, [ip + 1]
    jz  [0], load_imported_done

    eq  [rb + char], ':', [rb + tmp]
    jnz [rb + tmp], load_imported_save_identifier

    add err_expect_colon, 0, [rb]
    call report_error

load_imported_save_identifier:
    add [rb + module], 0, [rb - 1]
    arb -1
    call create_import
    add [rb - 3], 0, [rb + import]

    add [rb + import], IMPORT_IDENTIFIER, [ip + 3]
    add [rb + identifier], 0, [0]

load_imported_fixup_loop:
    call read_number
    add [rb - 2], 0, [rb + byte]
    add [rb - 3], 0, [rb + char]

    # store the byte
    add [rb + import], IMPORT_FIXUPS_HEAD, [rb - 1]
    add [rb + import], IMPORT_FIXUPS_TAIL, [rb - 2]
    add [rb + import], IMPORT_FIXUPS_INDEX, [rb - 3]
    add [rb + byte], 0, [rb - 4]
    arb -4
    call set_mem

    # next character should be comma or line end
    eq  [rb + char], ',', [rb + tmp]
    jnz [rb + tmp], load_imported_fixup_loop
    eq  [rb + char], 10, [rb + tmp]
    jnz [rb + tmp], load_imported_loop

    add err_expect_comma_eol, 0, [rb]
    call report_error

load_imported_done:
    # free the empty identifier
    add [rb + identifier], 0, [rb - 1]
    arb -1
    call free

    add [rb + char], 0, [rb - 1]
    arb -1
    call unget_input

    arb 5
    ret 1
.ENDFRAME

##########
load_exported:
.FRAME module; export, identifier, byte, char, tmp
    arb -5

load_exported_loop:
    # read the identifier
    call read_identifier
    add [rb - 2], 0, [rb + identifier]
    add [rb - 3], 0, [rb + char]

    # if there is no identifier, finish
    add [rb + identifier], 0, [ip + 1]
    jz  [0], load_exported_done

    eq  [rb + char], ':', [rb + tmp]
    jnz [rb + tmp], load_exported_save_identifier

    add err_expect_colon, 0, [rb]
    call report_error

load_exported_save_identifier:
    add [rb + module], 0, [rb - 1]
    arb -1
    call create_export
    add [rb - 3], 0, [rb + export]

    add [rb + export], EXPORT_IDENTIFIER, [ip + 3]
    add [rb + identifier], 0, [0]

    call read_number
    add [rb - 2], 0, [rb + byte]
    add [rb - 3], 0, [rb + char]

    # store the byte
    add [rb + export], EXPORT_ADDRESS, [ip + 3]
    add [rb + byte], 0, [0]

    # next character should be line end
    eq  [rb + char], 10, [rb + tmp]
    jnz [rb + tmp], load_exported_loop

    add err_expect_eol, 0, [rb]
    call report_error

load_exported_done:
    # free the empty identifier
    add [rb + identifier], 0, [rb - 1]
    arb -1
    call free

    add [rb + char], 0, [rb - 1]
    arb -1
    call unget_input

    arb 5
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
add_linker_symbols:
.FRAME module, export
    arb -2

    # create a dummy module at the end
    # create a module
    call create_module
    add [rb - 2], 0, [rb + module]

    # the dummy module exports a __heap_start symbol
    add [rb + module], 0, [rb - 1]
    arb -1
    call create_export
    add [rb - 3], 0, [rb + export]

    # be careful when freeing memory, this identifier is not dynamically allocated
    add [rb + export], EXPORT_IDENTIFIER, [ip + 3]
    add add_linker_symbols_heap_start, 0, [0]

    # address of the symbol within the module is 0
    add [rb + export], EXPORT_ADDRESS, [ip + 3]
    add 0, 0, [0]

    arb 2
    ret 0

add_linker_symbols_heap_start:
    db  "__heap_start", 0
.ENDFRAME

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
set_mem:
.FRAME head_ptr, tail_ptr, tail_index_ptr, byte; buffer, tmp
    arb -2

    add [rb + tail_ptr], 0, [ip + 1]
    add [0], 0, [rb + buffer]

    # do we have a buffer at all?
    jnz [rb + buffer], set_mem_have_buffer

    # no, create one
    add MEM_BLOCK_SIZE, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + buffer]

    # reset next buffer pointer
    add [rb + buffer], 0, [ip + 3]
    add 0, 0, [0]

    add [rb + head_ptr], 0, [ip + 3]
    add [rb + buffer], 0, [0]

    add [rb + tail_ptr], 0, [ip + 3]
    add [rb + buffer], 0, [0]

    add [rb + tail_index_ptr], 0, [ip + 3]
    add 1, 0, [0]

    jz  0, set_mem_have_space

set_mem_have_buffer:
    # is there enough space for one more byte?
    add [rb + tail_index_ptr], 0, [ip + 1]
    lt  [0], MEM_BLOCK_SIZE, [rb + tmp]
    jnz [rb + tmp], set_mem_have_space

    # no, create a new buffer
    add MEM_BLOCK_SIZE, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + buffer]

    # reset next buffer pointer
    add [rb + buffer], 0, [ip + 3]
    add 0, 0, [0]

    # add it to the tail of buffer linked list
    add [rb + tail_ptr], 0, [ip + 1]
    add [0], 0, [ip + 3]
    add [rb + buffer], 0, [0]

    add [rb + tail_ptr], 0, [ip + 3]
    add [rb + buffer], 0, [0]

    add [rb + tail_index_ptr], 0, [ip + 3]
    add 1, 0, [0]

set_mem_have_space:
    add [rb + tail_index_ptr], 0, [ip + 1]
    add [0], 0, [rb + tmp]

    add [rb + tail_ptr], 0, [ip + 1]
    add [0], [rb + tmp], [ip + 3]
    add [rb + byte], 0, [0]

    add [rb + tail_index_ptr], 0, [ip + 1]
    add [0], 0, [rb + tmp]
    add [rb + tail_index_ptr], 0, [ip + 3]
    add [rb + tmp], 1, [0]

    arb 2
    ret 4
.ENDFRAME

##########
inc_mem:
.FRAME head, tail, tail_index, address, increment; index, offset, tmp
    arb -3

    # find out which memory block should be updated
    add [rb + address], 0, [rb - 1]
    arb -1
    call calc_mem

    add [rb - 3], 0, [rb + index]
    add [rb - 4], 0, [rb + offset]

    # update the memory
    add [rb + head], 0, [rb - 1]
    add [rb + tail], 0, [rb - 2]
    add [rb + tail_index], 0, [rb - 3]
    add [rb + index], 0, [rb - 4]
    add [rb + offset], 0, [rb - 5]
    add [rb + increment], 0, [rb - 6]
    arb -6
    call inc_mem_internal

    arb 3
    ret 5
.ENDFRAME

##########
calc_mem:
.FRAME address; index, offset, tmp
    arb -3

    # calculate index = address / (MEM_BLOCK_SIZE - 1) ; offset = address % (MEM_BLOCK_SIZE - 1) + 1
    add 0, 0, [rb + index]
    add [rb + address], 0, [rb + offset]

calc_mem_loop:
    lt  [rb + offset], MEM_BLOCK_SIZE - 1, [rb + tmp]
    jnz [rb + tmp], calc_mem_done

    mul MEM_BLOCK_SIZE - 1, -1, [rb + tmp]
    add [rb + offset], [rb + tmp], [rb + offset]
    add [rb + index], 1, [rb + index]

    jz  0, calc_mem_loop

calc_mem_done:
    # data in memory blocks starts at offset 1
    add [rb + offset], 1, [rb + offset]

    arb 3
    ret 1
.ENDFRAME

##########
inc_mem_internal:
.FRAME head, tail, tail_index, index, offset, increment; buffer, tmp
    arb -2

    # increase memory location in index-th memory block, location offset
    # assume this does not require creating new blocks
    # TODO validate against tail and tail_index

    add [rb + head], 0, [rb + buffer]

inc_mem_at_loop:
    # any more blocks?
    jnz [rb + buffer], inc_mem_at_have_block

    add err_invalid_address, 0, [rb]
    call report_error

inc_mem_at_have_block:
    # is this the block we need?
    jz  [rb + index], inc_mem_at_this_block
    add [rb + index], -1, [rb + index]

    # next block in linked list
    add [rb + buffer], 0, [ip + 1]
    add [0], 0, [rb + buffer]

    jz  0, inc_mem_at_loop

inc_mem_at_this_block:
    # set the value
    add [rb + buffer], [rb + offset], [ip + 1]
    add [0], 0, [rb + tmp]
    add [rb + buffer], [rb + offset], [ip + 3]
    add [rb + tmp], [rb + increment], [0]

    arb 2
    ret 6
.ENDFRAME

##########
print_mem:
.FRAME head, tail, tail_index; tmp, buffer, limit, index, first
    arb -5

    add 1, 0, [rb + first]

    add [rb + head], 0, [rb + buffer]
    jz  [rb + head], print_mem_done

print_mem_block:
    add 1, 0, [rb + index]

    # maximum index within a block is MEM_BLOCK_SIZE, except for last block
    add MEM_BLOCK_SIZE, 0, [rb + limit]
    eq  [rb + buffer], [rb + tail], [rb + tmp]
    jz  [rb + tmp], print_mem_byte
    add [rb + tail_index], 0, [rb + limit]

print_mem_byte:
    lt  [rb + index], [rb + limit], [rb + tmp]
    jz  [rb + tmp], print_mem_block_done

    # skip comma when printing first byte
    jnz [rb + first], print_mem_skip_comma
    out ','

print_mem_skip_comma:
    add 0, 0, [rb + first]

    add [rb + buffer], [rb + index], [ip + 1]
    add [0], 0, [rb + tmp]

    add [rb + tmp], 0, [rb - 1]
    arb -1
    call print_num

    add [rb + index], 1, [rb + index]
    jz  0, print_mem_byte

print_mem_block_done:
    # next block in linked list
    add [rb + buffer], 0, [ip + 1]
    add [0], 0, [rb + buffer]

    jnz [rb + buffer], print_mem_block

print_mem_done:
    arb 5
    ret 3
.ENDFRAME

##########
report_error:
.FRAME message;
    # we don't bother with updating the stack pointer, this function never returns
    add [rb + message], 0, [rb + 2]
    add [input_line_num], 0, [rb + 1]
    add [input_column_num], 0, [rb]
    call report_error_at_location
.ENDFRAME

##########
read_identifier:
.FRAME buffer, char, index, tmp
    arb -4

    # we will store the identifier in dynamic memory that needs to be freed by caller
    add MEM_BLOCK_SIZE, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + buffer]

    add 0, 0, [rb + index]

read_identifier_loop:
    call get_input
    add [rb - 2], 0, [rb + char]

    # when we find first non-alphanumeric character, we are done
    add [rb + char], 0, [rb - 1]
    arb -1
    call is_alphanum
    jz  [rb - 3], read_identifier_done

    # store the character in buffer
    add [rb + buffer], [rb + index], [ip + 3]
    add [rb + char], 0, [0]

    # increase index and check for maximum identifier length
    add [rb + index], 1, [rb + index]
    lt  [rb + index], IDENTIFIER_LENGTH, [rb + tmp]
    jnz [rb + tmp], read_identifier_loop

    add err_max_identifier_length, 0, [rb]
    call report_error

read_identifier_done:
    # zero terminate
    add [rb + buffer], [rb + index], [ip + 3]
    add 0, 0, [0]

    arb 4
    ret 0
.ENDFRAME

##########
read_number:
.FRAME byte, digit, sign, tmp
    arb -4

    add 0, 0, [rb + byte]
    add 1, 0, [rb + sign]

    # get first character, process the minus sign if present
    call get_input
    add [rb - 2], 0, [rb + digit]

    eq  [rb + digit], '-', [rb + tmp]
    jz  [rb + tmp], read_number_loop

    add -1, 0, [rb + sign]

    call get_input
    add [rb - 2], 0, [rb + digit]

read_number_loop:
    # if it is not a digit, end
    add [rb + digit], 0, [rb - 1]
    arb -1
    call is_digit
    jz  [rb - 3], read_number_end

    # convert ASCII to a number
    add [rb + digit], -'0', [rb + digit]

    # byte = byte * 10 + digit
    mul [rb + byte], 10, [rb + byte]
    add [rb + byte], [rb + digit], [rb + byte]

    # get next character
    call get_input
    add [rb - 2], 0, [rb + digit]

    jz  0, read_number_loop

read_number_end:
    mul [rb + byte], [rb + sign], [rb + byte]

    # return:
    # [rb + byte] is the numbner
    # [rb + digit] is the next non-digit character

    arb 4
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

# line and column number of next input character
input_line_num:
    db  1
input_column_num:
    db  1

# column number before last input character
input_prev_column_num:
    db  0

# last input char buffer
input_buffer:
    db  0

# allocation block size
# TODO don't duplicate this from heap.s
.SYMBOL MEM_BLOCK_SIZE 50

.SYMBOL IDENTIFIER_LENGTH 45

# module record layout:
.SYMBOL MODULE_NEXT_PTR             0
.SYMBOL MODULE_CODE_HEAD            1
.SYMBOL MODULE_CODE_TAIL            2
.SYMBOL MODULE_CODE_INDEX           3
.SYMBOL MODULE_CODE_LENGTH          4
.SYMBOL MODULE_RELOC_HEAD           5
.SYMBOL MODULE_RELOC_TAIL           6
.SYMBOL MODULE_RELOC_INDEX          7
.SYMBOL MODULE_IMPORTS_HEAD         8
.SYMBOL MODULE_EXPORTS_HEAD         9
.SYMBOL MODULE_NEEDED               10          # 0 = not needed, 1 = needed
.SYMBOL MODULE_INCLUDED             11          # 0 = not included, 1 = included
.SYMBOL MODULE_ADDRESS              12
.SYMBOL MODULE_SIZE                 13

# loaded modules
module_head:
    db 0
module_tail:
    db 0

# export record layout:
.SYMBOL EXPORT_NEXT_PTR             0
.SYMBOL EXPORT_PREV_PTR             1
.SYMBOL EXPORT_IDENTIFIER           2
.SYMBOL EXPORT_MODULE               3
.SYMBOL EXPORT_ADDRESS              4
.SYMBOL EXPORT_IMPORTS_HEAD         5
.SYMBOL EXPORT_IMPORTS_TAIL         6
.SYMBOL EXPORT_SIZE                 7

# import record layout:
.SYMBOL IMPORT_NEXT_PTR             0
.SYMBOL IMPORT_PREV_PTR             1
.SYMBOL IMPORT_IDENTIFIER           2
.SYMBOL IMPORT_MODULE               3
.SYMBOL IMPORT_FIXUPS_HEAD          4
.SYMBOL IMPORT_FIXUPS_TAIL          5
.SYMBOL IMPORT_FIXUPS_INDEX         6
.SYMBOL IMPORT_SIZE                 7

# included symbols
symbol_head:
    db 0
symbol_tail:
    db 0

# 0 = processing object files, 1 = processing libraries
is_library:
    db  0

##########
# error messages

err_expect_dot_c_l_at:
    db  "Expecting a .C, .L or or .$", 0
err_expect_dot_c_at:
    db  "Expecting a .C or or .$", 0
err_expect_dot_i:
    db  "Expecting a .I", 0
err_expect_dot_e:
    db  "Expecting a .E", 0
err_expect_dot_r:
    db  "Expecting a .R", 0
err_expect_comma_eol:
    db  "Expecting a comma or line end", 0
err_expect_eol:
    db  "Expecting a line end", 0
err_expect_colon:
    db  "Expecting a colon", 0
err_max_identifier_length:
    db  "Maximum identifier length exceeded", 0
err_duplicate_export:
    db  "Duplicate exported symbol", 0
err_export_not_found:
    db  "Exported symbol not found", 0
err_included_not_resolved:
    db  "Symbol not resolved by including its module", 0
err_invalid_address:
    db  "Invalid memory address", 0

##########
    ds  50, 0
stack:

.EOF
