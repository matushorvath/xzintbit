#!/usr/bin/env bash

set -xe

# Build the linker
./build.sh src/ld.s

# Build objects
vm/ic bin/as.input < test/link_reverse/link_reverse.s > link_reverse.o.tmp
vm/ic bin/as.input < test/link_reverse/link_reverse_2.s > link_reverse_2.o.tmp

# Link the objects
echo .$ | cat link_reverse.o.tmp link_reverse_2.o.tmp - | vm/ic ld.input.tmp
