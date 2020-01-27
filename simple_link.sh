#!/usr/bin/env bash

set -xe

# Create a library
echo .L | cat - test/simple_link.2.a.o test/simple_link.3.a.o > simple_library.a.tmp

# Link an object with the library
echo .$ | cat test/simple_link.1.o simple_library.a.tmp - | vm/ic ld.input.tmp
