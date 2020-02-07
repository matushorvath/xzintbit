include ../test.mk

test: prep output/$(NAME).input
output/$(NAME).input: output/$(NAME).o output/$(NAME)_lib.a
output/$(NAME)_lib.a: output/$(NAME)_lib.o output/$(NAME)_lib_2.o

.PHONY: test
