#!/usr/bin/env bash

set -xe

# Build the linker
./build.sh src/ld.s

# Build objects
vm/ic bin/as.input < test/link_one_symbol/link_one_symbol.s > link_one_symbol.o.tmp
vm/ic bin/as.input < test/link_one_symbol/link_one_symbol_2.s > link_one_symbol_2.o.tmp

# Link the objects
echo .$ | cat link_one_symbol.o.tmp link_one_symbol_2.o.tmp - | vm/ic ld.input.tmp
