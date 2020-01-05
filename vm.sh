#!/usr/bin/env bash

set -e

# Edit this script to run your ASCII-enabled IntCode VM
# It should read input from stdin and write output to stdout (with no extra characters)

pushd vm > /dev/null

# sanity check, node.js must be installed
if ! node --version > /dev/null; then
    echo "You will need Node.js version 10.x or newer to run the Intcode VM" >&2
    echo "Or you can use your own Intcode VM, see https://tinyurl.com/vq59mtk" >&2
    exit 1
fi

# install modules if they don't exist
if [ ! -e node_modules ]; then
    echo "First run, installing TypeScript modules" >&2
    npm i >&2
fi

# execute the vm
npx ts-node src/ic.ts "../$1"

popd > /dev/null
