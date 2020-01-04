#!/usr/bin/env bash

set -e

# Edit this script to run your ASCII-enabled IntCode VM
# It should read input from stdin and write output to stdout (with no extra characters)

pushd vm > /dev/null
npx ts-node src/ic.ts "../$1"
popd > /dev/null
