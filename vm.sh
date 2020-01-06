#!/usr/bin/env bash

set -e

# Edit this script to run your ASCII-enabled IntCode VM
# It should read input from stdin and write output to stdout (with no extra characters)

# build the VM
if [ ! -e vm/ic ]; then
    echo "First run, building the Intcode VM" >&2
    make -C vm >&2
fi

# execute the vm
vm/ic "$1"
