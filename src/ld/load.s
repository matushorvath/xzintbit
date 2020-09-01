.EXPORT load_objects

# from libxib/heap.s
.IMPORT alloc
.IMPORT free

# from libxib/string.s
.IMPORT zeromem

# from data.s
.IMPORT module_head
.IMPORT module_tail

# from lexer.s
.IMPORT read_directive
.IMPORT read_identifier
.IMPORT read_number
.IMPORT get_input
.IMPORT unget_input

# from memory.s
.IMPORT set_mem

# from util.s
.IMPORT report_error

##########
load_objects:
.FRAME module, tmp
    arb -2

load_objects_loop:
    # check for more files
    call expect_next_file
    eq  [rb - 2], '$', [rb + tmp]
    jnz [rb + tmp], load_objects_load_done

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

    jz  0, load_objects_loop

load_objects_load_done:
    call add_linker_symbols

    arb 2
    ret 0
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
# globals

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

.EOF
