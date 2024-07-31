.EXPORT load_objects

# from libxib/heap.s
.IMPORT free

# from libxib/input.s
.IMPORT get_input
.IMPORT peek_input
.IMPORT unget_input

# from libxib/lexer.s
.IMPORT read_identifier
.IMPORT read_number

# from libxib/memory.s
.IMPORT set_mem

# from data.s
.IMPORT create_module
.IMPORT create_import
.IMPORT create_export
.IMPORT create_symbol
.IMPORT module_head
.IMPORT module_tail

# from error.s
.IMPORT report_error

##########
load_objects:
.FRAME module, directive, is_library, tmp
    arb -4

    # check for more files
    call expect_next_file
    eq  [rb - 2], '$', [rb + tmp]
    jnz [rb + tmp], .done
    eq  [rb - 2], 'L', [rb + is_library]

.loop:
    # create a module
    add [rb + is_library], 0, [rb - 1]
    arb -1
    call create_module
    add [rb - 3], 0, [rb + module]

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

    # check for either the symbols or more files
    call expect_symbols_or_next_file
    add [rb - 2], 0, [rb + directive]

    # do we have symbols for current file?
    eq  [rb + directive], 'S', [rb + tmp]
    jz  [rb + tmp], .no_symbols

    # we have .S, load symbols
    add [rb + module], 0, [rb - 1]
    arb -1
    call load_symbols

    # check for more files
    call expect_next_file
    add [rb - 2], 0, [rb + directive]

.no_symbols:
    # is this the last file?
    eq  [rb + directive], '$', [rb + tmp]
    jnz [rb + tmp], .done
    eq  [rb + directive], 'L', [rb + is_library]

    jz  0, .loop

.done:
    call add_linker_symbols

    arb 4
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
    jnz [rb + tmp], .done
    eq  [rb + char], 'C', [rb + tmp]
    jnz [rb + tmp], .done
    eq  [rb + char], 'L', [rb + tmp]
    jnz [rb + tmp], .done

    add err_expect_dot_c_l_at, 0, [rb]
    call report_error

.done:
    # return [rb + char]

    arb 2
    ret 0
.ENDFRAME

##########
expect_symbols_or_next_file:
.FRAME char, tmp
    arb -2

    add err_expect_dot_s_c_l_at, 0, [rb - 1]
    arb -1
    call read_directive
    add [rb - 3], 0, [rb + char]

    # symbols begin with a .S, object files begin with a .C,
    # libraries begin with a .L, after the last file we expect a .$
    eq  [rb + char], 'S', [rb + tmp]
    jnz [rb + tmp], .done
    eq  [rb + char], 'C', [rb + tmp]
    jnz [rb + tmp], .done
    eq  [rb + char], 'L', [rb + tmp]
    jnz [rb + tmp], .done
    eq  [rb + char], '$', [rb + tmp]
    jnz [rb + tmp], .done

    add err_expect_dot_s_c_l_at, 0, [rb]
    call report_error

.done:
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
    jz  [rb + tmp], .error

    arb 2
    ret 2

.error:
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
    jz  [rb + tmp], .error

    call get_input
    add [rb - 2], 0, [rb + char]

    call get_input
    add [rb - 2], 0, [rb + tmp]

    eq  [rb + tmp], 10, [rb + tmp]
    jz  [rb + tmp], .error

    # return [rb + char]

    arb 2
    ret 1

.error:
    add [rb + error_message], 0, [rb]
    call report_error
.ENDFRAME

##########
load_code:
.FRAME module; byte, length, tmp
    arb -3

    add 0, 0, [rb + length]

    # peek one character to see if we have code at all
    call peek_input

    eq  [rb - 2], '.', [rb + tmp]
    jnz [rb + tmp], .done

.loop:
    call read_number
    add [rb - 2], 0, [rb + byte]

    # was there actually a number?
    jnz [rb - 3], .have_number

    add err_expect_number, 0, [rb]
    call report_error

.have_number:
    # was the number base 10?
    eq  [rb - 4], 10, [rb + tmp]
    jnz [rb + tmp], .decimal

    add err_expect_decimal, 0, [rb]
    call report_error

.decimal:
    # store the address
    add [rb + module], MODULE_CODE_HEAD, [rb - 1]
    add [rb + module], MODULE_CODE_TAIL, [rb - 2]
    add [rb + module], MODULE_CODE_INDEX, [rb - 3]
    add [rb + byte], 0, [rb - 4]
    arb -4
    call set_mem

    add [rb + length], 1, [rb + length]

    # next character should be comma or line end
    call get_input

    eq  [rb - 2], ',', [rb + tmp]
    jnz [rb + tmp], .loop
    eq  [rb - 2], 10, [rb + tmp]
    jnz [rb + tmp], .done

    add err_expect_comma_eol, 0, [rb]
    call report_error

.done:
    add [rb + module], MODULE_CODE_LENGTH, [ip + 3]
    add [rb + length], 0, [0]

    arb 3
    ret 1
.ENDFRAME

##########
load_relocated:
.FRAME module; byte, tmp
    arb -2

    # peek one character to see if we have relocated data at all
    call peek_input

    eq  [rb - 2], '.', [rb + tmp]
    jnz [rb + tmp], .done

.loop:
    call read_number
    add [rb - 2], 0, [rb + byte]

    # was there actually a number?
    jnz [rb - 3], .have_number

    add err_expect_number, 0, [rb]
    call report_error

.have_number:
    # was the number base 10?
    eq  [rb - 4], 10, [rb + tmp]
    jnz [rb + tmp], .decimal

    add err_expect_decimal, 0, [rb]
    call report_error

.decimal:
    # store the address
    add [rb + module], MODULE_RELOC_HEAD, [rb - 1]
    add [rb + module], MODULE_RELOC_TAIL, [rb - 2]
    add [rb + module], MODULE_RELOC_INDEX, [rb - 3]
    add [rb + byte], 0, [rb - 4]
    arb -4
    call set_mem

    # next character should be comma or line end
    call get_input

    eq  [rb - 2], ',', [rb + tmp]
    jnz [rb + tmp], .loop
    eq  [rb - 2], 10, [rb + tmp]
    jnz [rb + tmp], .done

    add err_expect_comma_eol, 0, [rb]
    call report_error

.done:
    arb 2
    ret 1
.ENDFRAME

##########
load_imported:
.FRAME module; import, identifier, byte, tmp
    arb -4

.loop:
    # read the identifier
    call read_identifier
    add [rb - 2], 0, [rb + identifier]

    # if there is no identifier, finish
    add [rb + identifier], 0, [ip + 1]
    jz  [0], .done

    call get_input
    eq  [rb - 2], ':', [rb + tmp]
    jnz [rb + tmp], .save_identifier

    add err_expect_colon, 0, [rb]
    call report_error

.save_identifier:
    add [rb + module], 0, [rb - 1]
    arb -1
    call create_import
    add [rb - 3], 0, [rb + import]

    add [rb + import], IMPORT_IDENTIFIER, [ip + 3]
    add [rb + identifier], 0, [0]

.fixup_loop:
    call read_number
    add [rb - 2], 0, [rb + byte]

    # was there actually a number?
    jnz [rb - 3], .fixup_have_number

    add err_expect_number, 0, [rb]
    call report_error

.fixup_have_number:
    # was the number base 10?
    eq  [rb - 4], 10, [rb + tmp]
    jnz [rb + tmp], .decimal

    add err_expect_decimal, 0, [rb]
    call report_error

.decimal:
    # store the address
    add [rb + import], IMPORT_FIXUPS_HEAD, [rb - 1]
    add [rb + import], IMPORT_FIXUPS_TAIL, [rb - 2]
    add [rb + import], IMPORT_FIXUPS_INDEX, [rb - 3]
    add [rb + byte], 0, [rb - 4]
    arb -4
    call set_mem

    # next character should be comma or line end
    call get_input

    eq  [rb - 2], ',', [rb + tmp]
    jnz [rb + tmp], .fixup_loop
    eq  [rb - 2], 10, [rb + tmp]
    jnz [rb + tmp], .loop

    add err_expect_comma_eol, 0, [rb]
    call report_error

.done:
    # free the empty identifier
    add [rb + identifier], 0, [rb - 1]
    arb -1
    call free

    arb 4
    ret 1
.ENDFRAME

##########
load_exported:
.FRAME module; export, identifier, byte, tmp
    arb -4

.loop:
    # read the identifier
    call read_identifier
    add [rb - 2], 0, [rb + identifier]

    # if there is no identifier, finish
    add [rb + identifier], 0, [ip + 1]
    jz  [0], .done

    call get_input
    eq  [rb - 2], ':', [rb + tmp]
    jnz [rb + tmp], .save_identifier

    add err_expect_colon, 0, [rb]
    call report_error

.save_identifier:
    add [rb + module], 0, [rb - 1]
    arb -1
    call create_export
    add [rb - 3], 0, [rb + export]

    add [rb + export], EXPORT_IDENTIFIER, [ip + 3]
    add [rb + identifier], 0, [0]

    call read_number
    add [rb - 2], 0, [rb + byte]

    # was there actually a number?
    jnz [rb - 3], .have_number

    add err_expect_number, 0, [rb]
    call report_error

.have_number:
    # was the number base 10?
    eq  [rb - 4], 10, [rb + tmp]
    jnz [rb + tmp], .decimal

    add err_expect_decimal, 0, [rb]
    call report_error

.decimal:
    # store the address
    add [rb + export], EXPORT_ADDRESS, [ip + 3]
    add [rb + byte], 0, [0]

    # next character should be line end
    call get_input

    eq  [rb - 2], 10, [rb + tmp]
    jnz [rb + tmp], .loop

    add err_expect_eol, 0, [rb]
    call report_error

.done:
    # free the empty identifier
    add [rb + identifier], 0, [rb - 1]
    arb -1
    call free

    arb 4
    ret 1
.ENDFRAME

##########
load_symbols:
.FRAME module; symbol, identifier, byte, tmp
    arb -4

.loop:
    # read the parent identifier
    call read_identifier
    add [rb - 2], 0, [rb + identifier]

    # if there is no identifier, finish
    add [rb + identifier], 0, [ip + 1]
    jz  [0], .done

    # save the parent identifier
    add [rb + module], 0, [rb - 1]
    arb -1
    call create_symbol
    add [rb - 3], 0, [rb + symbol]

    add [rb + symbol], SYMBOL_PARENT_IDENTIFIER, [ip + 3]
    add [rb + identifier], 0, [0]

    # is there a child identifier?
    call get_input
    eq  [rb - 2], '.', [rb + tmp]
    jnz [rb + tmp], .read_child
    eq  [rb - 2], ':', [rb + tmp]
    jnz [rb + tmp], .after_child

    add err_expect_colon_or_dot, 0, [rb]
    call report_error

.read_child:
    # read the child identifier
    call read_identifier
    add [rb - 2], 0, [rb + identifier]

    # if there is no identifier, report an error
    add [rb + identifier], 0, [ip + 1]
    jnz [0], .save_child

    add err_expect_identifier, 0, [rb]
    call report_error

.save_child:
    # save the child identifier
    add [rb + symbol], SYMBOL_CHILD_IDENTIFIER, [ip + 3]
    add [rb + identifier], 0, [0]

    call get_input
    eq  [rb - 2], ':', [rb + tmp]
    jnz [rb + tmp], .after_child

    add err_expect_colon, 0, [rb]
    call report_error

.after_child:
    call read_number
    add [rb - 2], 0, [rb + byte]

    # was there actually a number?
    jnz [rb - 3], .have_number

    add err_expect_number, 0, [rb]
    call report_error

.have_number:
    # was the number base 10?
    eq  [rb - 4], 10, [rb + tmp]
    jnz [rb + tmp], .decimal

    add err_expect_decimal, 0, [rb]
    call report_error

.decimal:
    # store the address
    add [rb + symbol], SYMBOL_ADDRESS, [ip + 3]
    add [rb + byte], 0, [0]

    # next character should be line end
    call get_input

    eq  [rb - 2], 10, [rb + tmp]
    jnz [rb + tmp], .loop

    add err_expect_eol, 0, [rb]
    call report_error

.done:
    # free the empty identifier
    add [rb + identifier], 0, [rb - 1]
    arb -1
    call free

    arb 4
    ret 1
.ENDFRAME

##########
add_linker_symbols:
.FRAME module, export
    arb -2

    # create a dummy module at the end
    # create a module
    add 1, 0, [rb - 1]
    arb -1
    call create_module
    add [rb - 3], 0, [rb + module]

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
# error messages

err_expect_dot_c_l_at:
    db  "Expecting a .C, .L or .$", 0
err_expect_dot_s_c_l_at:
    db  "Expecting a .S, .C, .L or .$", 0
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
err_expect_colon_or_dot:
    db  "Expecting a colon or a dot", 0
err_expect_identifier:
    db  "Expecting an identifier", 0
err_expect_number:
    db  "Expecting a number", 0
err_expect_decimal:
    db  "Expecting a decimal number", 0

.EOF
