##########
    arb stack

##########
link:
.FRAME tmp
    arb -1

link_loop:
    # check for more files
    call expect_next_file
    eq  [rb - 2], '$', [rb + tmp]
    jnz [rb + tmp], link_done

    # we have .C, load the code
    call load_code

    # next expect .R
    add 'R', 0, [rb - 1]
    add err_expect_dot_r, 0, [rb - 2]
    arb -2
    call expect_directive

    # we have .R, load addresses for relocation
    call load_relocated

    # next expect .I
    add 'I', 0, [rb - 1]
    add err_expect_dot_i, 0, [rb - 2]
    arb -2
    call expect_directive

    # we have .I, load imported symbols
    call load_imported

    # next expect .E
    add 'E', 0, [rb - 1]
    add err_expect_dot_e, 0, [rb - 2]
    arb -2
    call expect_directive

    # we have .E, load exported symbols
    call load_exported

    # TODO relocate

    jz  0, link_loop

link_done:
    # TODO link exported and imported symbols

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
load_code:
.FRAME
    # TODO implement
    add load_code_str, 0, [rb - 1]
    arb -1
    call print_str
    out 10

    ret 0

load_code_str:
    db  "load code", 0
.ENDFRAME

##########
load_imported:
.FRAME
    # TODO implement
    add load_imported_str, 0, [rb - 1]
    arb -1
    call print_str
    out 10

    ret 0

load_imported_str:
    db  "load imported", 0
.ENDFRAME

##########
load_exported:
.FRAME
    # TODO implement
    add load_exported_str, 0, [rb - 1]
    arb -1
    call print_str
    out 10

    ret 0

load_exported_str:
    db  "load exported", 0
.ENDFRAME

##########
load_relocated:
.FRAME
    # TODO implement
    add load_relocated_str, 0, [rb - 1]
    arb -1
    call print_str
    out 10

    ret 0

load_relocated_str:
    db  "load relocated", 0
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

# symbol record layout:
# 0: pointer to next symbol
# 1-IDENTIFIER_LENGTH: zero-terminated symbol identifier
# 48: symbol value (address)
# 49: linked list of fixups

# fixup record layout:
# 0: pointer to next fixup
# 1: fixup address

# head of the linked list of global symbols
global_head:
    db  0

# current instruction pointer
current_address:
    db 0

# output memory buffer
mem_head:
    db 0
mem_tail:
    db 0

# index of next unused byte in last memory buffer
mem_index:
    db 0

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

##########
    ds  50, 0
stack:

.EOF
