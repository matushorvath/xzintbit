#!/usr/bin/env bash

set -xe

# Build the linker
./build.sh src/ld.s

# Build objects
vm/ic bin/as.input < test/link_complex/link_complex.s > link_complex.o.tmp
vm/ic bin/as.input < test/link_complex/link_complex_2.s > link_complex_2.o.tmp

# Link the objects
echo .$ | cat link_complex.o.tmp link_complex_2.o.tmp - | vm/ic ld.input.tmp
