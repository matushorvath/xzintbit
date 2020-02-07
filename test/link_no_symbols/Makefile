include ../test.mk

test: prep output/link_no_symbols.input
output/link_no_symbols.input: output/link_no_symbols.o output/link_no_symbols_lib.a
output/link_no_symbols_lib.a: output/link_no_symbols_lib.o output/link_no_symbols_lib_2.o

.PHONY: test
