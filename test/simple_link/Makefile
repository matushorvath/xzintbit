include ../test.mk

test: prep output/simple_link.input
output/simple_link.input: output/simple_link.o output/simple_link_lib.a
output/simple_link_lib.a: output/simple_link_lib.o output/simple_link_lib2.o

.PHONY: test
