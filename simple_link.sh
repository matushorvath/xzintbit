#!/usr/bin/env bash

set -xe

# Create a library
echo .L | cat - test/ld/simple_object_2.o test/ld/simple_object_3.o > simple_library.a.tmp

# Link an object with the library
echo .$ | cat test/ld/simple_object_1.o simple_library.a.tmp - | vm/ic ld.tmp
