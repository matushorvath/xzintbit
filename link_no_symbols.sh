#!/usr/bin/env bash

set -xe

# Build the linker
./build.sh src/ld.s

# Build objects
vm/ic bin/as.input < test/link_no_symbols/link_no_symbols.s > link_no_symbols.o.tmp
vm/ic bin/as.input < test/link_no_symbols/link_no_symbols_lib.s > link_no_symbols_lib.o.tmp
vm/ic bin/as.input < test/link_no_symbols/link_no_symbols_lib_2.s > link_no_symbols_lib_2.o.tmp

# Create a library
echo .L | cat - link_no_symbols_lib.o.tmp link_no_symbols_lib_2.o.tmp > link_no_symbols_lib.a.tmp

# Link an object with the library
echo .$ | cat link_no_symbols.o.tmp link_no_symbols_lib.a.tmp - | vm/ic ld.input.tmp
