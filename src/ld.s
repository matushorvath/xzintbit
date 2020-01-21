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
    jnz [rb + tmp], link_done

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

link_done:
    call print_modules

    hlt
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
    # TODO mark that we are now processing libraries

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

    in  [rb + char]
    eq  [rb + char], '.', [rb + tmp]
    jz  [rb + tmp], read_directive_error

    in  [rb + char]

    in  [rb + tmp]
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
    add MEM_BLOCK_SIZE, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + module]

    # initialize to zeros
    add [rb + module], 0, [rb - 1]
    add MODULE_SIZE, 0, [rb - 2]
    arb -2
    call zeromem

    # TODO set up 'included' flag correctly
    add [rb + module], MODULE_INCLUDED, [ip + 3]
    add 1, 0, [0]

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
.FRAME module; symbol, byte, char, tmp
    arb -4

load_imported_loop:
    # read the identifier
    call read_identifier

    # if there is no identifier, finish
    jz  [rb - 4], load_imported_endline

    in  [rb + char]
    eq  [rb + char], ':', [rb + tmp]
    jnz [rb + tmp], load_imported_fixup_loop

    add err_expect_colon, 0, [rb]
    call report_error

    add [rb + module], 0, [rb - 1]
    add [rb + module], MODULE_IMPORTED_HEAD, [rb - 2]
    arb -2
    call create_symbol
    add [rb - 4], 0, [rb + symbol]

load_imported_fixup_loop:
    call read_number
    add [rb - 2], 0, [rb + byte]
    add [rb - 3], 0, [rb + char]

    # store the byte
    add [rb + symbol], SYMBOL_FIXUP_HEAD, [rb - 1]
    add [rb + symbol], SYMBOL_FIXUP_TAIL, [rb - 2]
    add [rb + symbol], SYMBOL_FIXUP_INDEX, [rb - 3]
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

load_imported_endline:
    # character read after the (empty) identifier should be EOL
    eq  [rb - 3], 10, [rb + tmp]
    jnz [rb + tmp], load_imported_done

    add err_expect_eol, 0, [rb]
    call report_error

load_imported_done:
    arb 4
    ret 1
.ENDFRAME

##########
load_exported:
.FRAME module;
    # TODO implement
#    add load_exported_str, 0, [rb - 1]
#    arb -1
#    call print_str
#    out 10

    ret 1

load_exported_str:
    db  "load exported", 0
.ENDFRAME

##########
create_symbol:
.FRAME module, symbol_head_ptr; symbol, tmp
    arb -2

    # allocate a block
    add MEM_BLOCK_SIZE, 0, [rb - 1]
    arb -1
    call alloc
    add [rb - 3], 0, [rb + symbol]

    # initialize to zeros
    add [rb + symbol], 0, [rb - 1]
    add SYMBOL_SIZE, 0, [rb - 2]
    arb -2
    call zeromem

    # save module pointer
    add [rb + symbol], SYMBOL_MODULE, [ip + 3]
    add [rb + module], 0, [0]

    # default symbol address is -1, not 0
    add [rb + symbol], SYMBOL_RELOC_ADDRESS, [ip + 3]
    add -1, 0, [0]

    # add to the head of doubly-linked list
    add [rb + symbol_head_ptr], 0, [ip + 1]
    add [0], 0, [rb + tmp]

    add [rb + symbol], SYMBOL_NEXT_PTR, [ip + 3]
    add [rb + tmp], 0, [0]

    add [rb + symbol], SYMBOL_PREV_PTR, [ip + 3]
    add 0, 0, [0]

    add [rb + symbol_head_ptr], 0, [ip + 3]
    add [rb + symbol], 0, [0]

    arb 2
    ret 1
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
    add report_error_msg_start, 0, [rb - 1]
    arb -1
    call print_str

    add [rb + message], 0, [rb - 1]
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
    in  [rb + char]

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
    in  [rb + digit]
    eq  [rb + digit], '-', [rb + tmp]
    jz  [rb + tmp], read_number_loop

    add -1, 0, [rb + sign]
    in  [rb + digit]

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
    in  [rb + digit]

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
.SYMBOL MODULE_IMPORTED_HEAD        7
.SYMBOL MODULE_EXPORTED_HEAD        8
.SYMBOL MODULE_MANDATORY            9           # 0 = optional, 1 = mandatory
.SYMBOL MODULE_INCLUDED             10          # 0 = not included, 1 = included
.SYMBOL MODULE_RELOC_ADDRESS        11
.SYMBOL MODULE_SIZE                 12

# loaded modules
module_head:
    db 0
module_tail:
    db 0

# imported symbol record layout:
.SYMBOL SYMBOL_NEXT_PTR             0
.SYMBOL SYMBOL_PREV_PTR             1
.SYMBOL SYMBOL_RELOC_ADDRESS        2
.SYMBOL SYMBOL_MODULE               3
.SYMBOL SYMBOL_FIXUP_HEAD           4
.SYMBOL SYMBOL_FIXUP_TAIL           5
.SYMBOL SYMBOL_FIXUP_INDEX          6
.SYMBOL SYMBOL_IDENTIFIER           7
.SYMBOL SYMBOL_SIZE                 50

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

##########
    ds  50, 0
stack:

.EOF
