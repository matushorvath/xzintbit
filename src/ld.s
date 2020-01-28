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
    call resolve_symbols

    call print_modules

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
.FRAME module; byte, char, tmp
    arb -3

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

    # next character should be comma or line end
    eq  [rb + char], ',', [rb + tmp]
    jnz [rb + tmp], load_code_loop
    eq  [rb + char], 10, [rb + tmp]
    jnz [rb + tmp], load_code_done

    add err_expect_comma_eol, 0, [rb]
    call report_error

load_code_done:
    arb 3
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
resolve_symbols:
.FRAME module, tmp
    arb -2

resolve_symbols_restart:
    # process all modules
    add [module_head], 0, [rb + module]

resolve_symbols_loop:
    jz  [rb + module], resolve_symbols_done

    add [rb + module], MODULE_NEEDED, [ip + 1]
    jz  [0], resolve_symbols_next

    add [rb + module], MODULE_INCLUDED, [ip + 1]
    jnz [0], resolve_symbols_next

    # module is needed, but not yet included
    add [rb + module], 0, [rb - 1]
    arb -1
    call include_module

    # new modules may be needed, process all modules from the start
    jz  0, resolve_symbols_restart

resolve_symbols_next:
    add [rb + module], MODULE_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + module]

    jz  0, resolve_symbols_loop

resolve_symbols_done:
    arb 2
    ret 0
.ENDFRAME

##########
include_module:
.FRAME module; tmp
    arb -1

    # process exported
    add [rb + module], MODULE_EXPORTS_HEAD, [rb - 1]
    arb -1
    call include_exported

    # process imported
    add [rb + module], MODULE_IMPORTS_HEAD, [rb - 1]
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
.FRAME exported_head; tmp
    arb -1

include_exported_loop:
    jz  [rb + exported_head], include_exported_done

    add [rb + exported_head], EXPORT_IDENTIFIER, [rb - 1]
    arb -1
    call find_resolved

    jz  [rb - 3], include_exported_is_new

    add err_duplicate_export, 0, [rb]
    call report_error

include_exported_is_new:
    # move to next exported symbol
    add [rb + exported_head], 0, [rb + tmp]
    add [rb + exported_head], EXPORT_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + exported_head]

    # append this symbol to list of resolved symbols
    add [rb + tmp], 0, [rb - 1]
    add resolved_head, 0, [rb - 2]
    add resolved_tail, 0, [rb - 3]
    arb -3
    call append_double_linked

    # TODO resolve imported symbol using this newly added exported symbol

    jz  0, include_exported_loop

include_exported_done:
    arb 1
    ret 1
.ENDFRAME

##########
include_imported:
.FRAME imported_head; tmp
    arb -1

    #out 'I'
    #out 10

    arb 1
    ret 1
.ENDFRAME

##########
find_resolved:
.FRAME identifier; head
    arb -1

    add [resolved_head], 0, [rb + head]

find_resolved_loop:
    add [rb + head], EXPORT_IDENTIFIER, [rb - 1]
    add [rb + identifier], 0, [rb - 2]
    arb -2
    call strcmp

    jz  [rb - 4], find_resolved_done

    add [rb + head], EXPORT_NEXT_PTR, [ip + 1]
    add [0], 0, [rb + head]

    jnz [rb + head], find_resolved_loop

find_resolved_done:
    # result is in [rb + head]
    arb 1
    ret 1
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
report_error_at_location:
.FRAME message, line_num, column_num;
    add report_error_msg_start, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + message], 0, [rb - 1]
    arb -1
    call print_str

    add report_error_msg_line, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + line_num], 0, [rb - 1]
    arb -1
    call print_num

    add report_error_msg_column, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + column_num], 0, [rb - 1]
    arb -1
    call print_num

    add report_error_msg_end, 0, [rb - 1]
    arb -1
    call print_str

    out 10

    # instead of halting, we will read all inputs, which hopefully
    # will cause the intcode vm to crash, indicating a problem
    # this is the only way we can return a non-zero result from intcode

report_error_loop:
    in  [0]
    jz  0, report_error_loop

report_error_msg_start:
    db "Error: ", 0
report_error_msg_line:
    db " (line ", 0
report_error_msg_column:
    db ", column ", 0
report_error_msg_end:
    db ")", 0
.ENDFRAME

##########
print_str:
.FRAME str; index, tmp, char
    arb -3

    add 0, 0, [rb + index]

print_str_loop:
    add [rb + str], [rb + index], [ip + 1]
    add [0], 0, [rb + char]

    jz  [rb + char], print_str_done

    out [rb + char]

    add [rb + index], 1, [rb + index]
    jz  0, print_str_loop

print_str_done:
    arb 3
    ret 1
.ENDFRAME

##########
# convert number to string
print_num:
.FRAME num; tmp, order, digit; digits
    arb -3

    # determine highest power of 10
    add 1, 0, [rb + order]

    # handle sign if negative
    lt  [rb + num], 0, [rb + tmp]
    jz  [rb + tmp], print_num_next_order
    out '-'
    mul [rb + num], -1, [rb + num]

print_num_next_order:
+3 = print_num_digit_ptr_1:
    add [rb + order], 0, [rb + digits]
    add [print_num_digit_ptr_1], -1, [print_num_digit_ptr_1]

    mul [rb + order], 10, [rb + order]
    lt  [rb + num], [rb + order], [rb + tmp]
    jz  [rb + tmp], print_num_next_order

print_num_finish_order:
    add [print_num_digit_ptr_1], 1, [print_num_digit_ptr_2]

print_num_next_digit:
+1 = print_num_digit_ptr_2:
    add [rb + digits], 0, [rb + order]
    add -1, 0, [rb + digit]

print_num_increase:
    add [rb + digit], 1, [rb + digit]
    mul [rb + order], -1, [rb + tmp]
    add [rb + num], [rb + tmp], [rb + num]
    lt  [rb + num], 0, [rb + tmp]
    jz  [rb + tmp], print_num_increase

    add [rb + num], [rb + order], [rb + num]
    add [rb + digit], '0', [rb + digit]
    out [rb + digit]

    eq  [rb + order], 1, [rb + tmp]
    jnz [rb + tmp], print_num_finish

    add [print_num_digit_ptr_2], 1, [print_num_digit_ptr_2]
    jz  0, print_num_next_digit

print_num_finish:
    add digits, 0, [print_num_digit_ptr_2]
    add digits, 0, [print_num_digit_ptr_1]

    arb 3
    ret 1
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
is_digit:
.FRAME char; tmp
    arb -1

    # check if 0 <= char <= 9
    lt  '9', [rb + char], [rb + tmp]                        # if char > 9, not a digit
    jnz [rb + tmp], is_digit_end
    lt  [rb + char], '0', [rb + tmp]                        # if char < 0, not a digit

is_digit_end:
    # the result is a logical negation of [rb + tmp]
    eq  [rb + tmp], 0, [rb + tmp]

    arb 1
    ret 1
.ENDFRAME

##########
is_alpha:
.FRAME char; tmp
    arb -1

    # check if a <= char <= z
    lt  'z', [rb + char], [rb + tmp]                        # if char > z, not a letter
    jnz [rb + tmp], is_alpha_end
    lt  [rb + char], 'a', [rb + tmp]                        # if !(char < a), is a letter
    jz  [rb + tmp], is_alpha_end

    # check if A <= char <= Z
    lt  'Z', [rb + char], [rb + tmp]                        # if char > Z, not a letter
    jnz [rb + tmp], is_alpha_end
    lt  [rb + char], 'A', [rb + tmp]                        # if !(char < A), is a letter

is_alpha_end:
    # the result is a logical negation of [rb + tmp]
    eq  [rb + tmp], 0, [rb + tmp]

    arb 1
    ret 1
.ENDFRAME

##########
is_alphanum:
.FRAME char; tmp
    arb -1

    add [rb + char], 0, [rb - 1]
    arb -1
    call is_alpha
    add [rb - 3], 0, [rb + tmp]
    jnz [rb + tmp], is_alphanum_end

    add [rb + char], 0, [rb - 1]
    arb -1
    call is_digit
    add [rb - 3], 0, [rb + tmp]
    jnz [rb + tmp], is_alphanum_end

    eq  [rb + char], '_', [rb + tmp]

is_alphanum_end:
    arb 1
    ret 1
.ENDFRAME

##########
strcmp:
.FRAME str1, str2; tmp, char1, char2, index
    arb -4

    add 0, 0, [rb + index]

strcmp_loop:
    add [rb + str1], [rb + index], [ip + 1]
    add [0], 0, [rb + char1]

    add [rb + str2], [rb + index], [ip + 1]
    add [0], 0, [rb + char2]

    # different characters, we are done
    eq  [rb + char1], [rb + char2], [rb + tmp]
    jz  [rb + tmp], strcmp_done

    # same character, is it 0?
    jz  [rb + char1], strcmp_done

    add [rb + index], 1, [rb + index]
    jz  0, strcmp_loop

strcmp_done:
    mul [rb + char2], -1, [rb + tmp]
    add [rb + char1], [rb + tmp], [rb + tmp]

    arb 4
    ret 2
.ENDFRAME

##########
alloc:
.FRAME size; block, tmp
    arb -2

    # we only support certain block sizes
    lt  MEM_BLOCK_SIZE, [rb + size], [rb + tmp]
    jz  [rb + tmp], alloc_size_ok

    add err_allocation_size, 0, [rb]
    call report_error

alloc_size_ok:
    # do we have any free blocks?
    jz  [free_head], alloc_create_block

    # yes, remove first block from the list and return it
    add [free_head], 0, [rb + block]

    add [free_head], 0, [ip + 1]
    add [0], 0, [free_head]

    jz  0, alloc_done

alloc_create_block:
    # there are no free blocks, create one
    add [heap_end], 0, [rb + block]
    add [heap_end], MEM_BLOCK_SIZE, [heap_end]

alloc_done:
    arb 2
    ret 1
.ENDFRAME

##########
free:
.FRAME block; tmp
    arb -1

    # set pointer to next free block in the block we are returning
    add [rb + block], 0, [ip + 3]
    add [free_head], 0, [0]

    # set new free block head
    add [rb + block], 0, [free_head]

    arb 1
    ret 1
.ENDFRAME

##########
zeromem:
.FRAME ptr, size; tmp
    arb -1

zeromem_loop:
    add [rb + size], -1, [rb + size]
    lt  [rb + size], 0, [rb + tmp]
    jnz [rb + tmp], zeromem_done

    add [rb + ptr], [rb + size], [ip + 3]
    add 0, 0, [0]

    jz  0, zeromem_loop

zeromem_done:
    arb 1
    ret 2
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

# head of the linked list of free blocks
free_head:
    db  0

# start of unused memory
heap_end:
    db  stack

# allocation block size
.SYMBOL MEM_BLOCK_SIZE 50

.SYMBOL IDENTIFIER_LENGTH 45

# module record layout:
.SYMBOL MODULE_NEXT_PTR             0
.SYMBOL MODULE_CODE_HEAD            1
.SYMBOL MODULE_CODE_TAIL            2
.SYMBOL MODULE_CODE_INDEX           3
.SYMBOL MODULE_RELOC_HEAD           4
.SYMBOL MODULE_RELOC_TAIL           5
.SYMBOL MODULE_RELOC_INDEX          6
.SYMBOL MODULE_IMPORTS_HEAD         7
.SYMBOL MODULE_EXPORTS_HEAD         8
.SYMBOL MODULE_NEEDED               9           # 0 = not needed, 1 = needed
.SYMBOL MODULE_INCLUDED             10          # 0 = not included, 1 = included
.SYMBOL MODULE_ADDRESS              11
.SYMBOL MODULE_SIZE                 12

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

# resolved (exported) symbols
resolved_head:
    db 0
resolved_tail:
    db 0

# unresolved (exported) symbols
unresolved_head:
    db 0
unresolved_tail:
    db 0

# 0 = processing object files, 1 = processing libraries
is_library:
    db  0

##########
# error messages

err_allocation_size:
    db  "Unsupported allocation size", 0
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

##########
    ds  50, 0
stack:

.EOF
