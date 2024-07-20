alloc:

as:
FIXUP_SIZE      4
FRAME_SIZE      50
    has identifier, strcpy
GLOBAL_SIZE     50
    has identifier, strcpy

ld:
MODULE_SIZE     13
EXPORT_SIZE     7
IMPORT_SIZE     7

libxib:
MEM_BLOCK_SIZE  50
    lexer.s: read_identifier, read_string
    memory.s: set_mem

-----

free:

as:
reset_frame                     free frame symbols when exiting frame - free identifier
read_identifier_or_keyword      free string after detecting a keyword
read_directive                  free string after detecting a directive
parse_db                        free string after setting it to memory - check set_as_mem_str parameter length
parse_dir_frame_block           free string after copying it to frame symbol - check add_frame_symbol identifier length - instead hand over the string
parse_value                     free string after copying it to a fixup - check add_fixup, token_value parameter (1st) - instead hand over the string
parse_symbol                    free string after copying it to global symbol - check set_global_symbol_address, token_value (1st) - instead hand over the string
parse_dir_symbol                free identifier (its 2nd param) - check set_global_symbol_address as above, check all callers, they will be handing over the string
                                there is something complex going on with parse_dir_symbol, it returns the identifier to caller, but only sometimes...
parse_dir_import_export         free string after copying it to global symbol - check set_global_symbol_type

ld:
load_imported                   loaded identifier from .o, gets freed after processing
load_exported                   same as load_imported

all strings we now hand over, check where they come from
[token_value] is a global!

-----

alloc always allocates MEM_BLOCK_SIZE = 50, fails for bigger allocations

https://sourceware.org/glibc/wiki/MallocInternals

-----

Design:

chunk:
    size
    free
    ----
    next
    prev

min chunk size: >2, say 8

small: bins for sizes 8-64

large:
    >64, probably just one list

alloc algorithm:
    if (size > 64) alloc_large
    else alloc_small

alloc_small:
    go through the respective list
    return first item from the list, relink
    if not found, go through larger small lists, first item, cut it, return and store the rest in correct small list
    if not found, call alloc_large (but we already have request_size so optimize and reuse it)

alloc_large:
    iterate through list
    if chunk_size >= request_size, cut chunk to request_size + rest, save rest to list, return request_sized chunk, relink list
    else if next chunk is free, merge chunks
    if end of list, optional go through the small lists and merge them, if large enough then cut and use, else store into large list if >64, small list if <=64
    if still not found, call sbrk to make new chunk

-----

Test Design:

empty bins

alloc 0 1 5 6 17 5 90 17
free all of them in same order, including the 0
heap has 8: 4, 19: 2, 92: 1

alloc 5 6 1 5(all use 8, no split) 3(uses 19, split off 11) 4(uses 11, no split) 91(brk) 89(uses 92) 90(brk) 29(brk)
heap has 19: 1

free 29
heap has 19: 1, 31: 1

alloc 21(uses 31, split off 8)
alloc 17(uses 19), 6(uses 8)
heap has nothing

free 90
heap has large: [92: 1]

alloc 1(uses 90, split off 82 to large bin)
alloc 30(uses 82, split off 50 to small bin 50)
alloc 50(uses 50 whole)

alloc 500
