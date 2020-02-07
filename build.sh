#!/usr/bin/env bash

set -xe

id="$(basename -s .s $1)"

vm/ic bin/as.input < "$1" > "$id.o.tmp" && status=$? || status=$?
if [ $status -ne 0 ]; then
    awk '/[^-0-9,]/' < "$id.o.tmp"
    exit $status
fi

echo .$ | cat "$id.o.tmp" - | vm/ic bin/ld.input > "$id.input.tmp" && status=$? || status=$?
if [ $status -ne 0 ]; then
    awk '/[^-0-9,]/' < "$id.input.tmp"
    exit $status
fi
