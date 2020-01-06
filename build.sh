#!/usr/bin/env bash

set -xe

./vm.sh src/as.input < src/as.s > as.stg1.input && status=$? || status=$?
if [ $status -ne 0 ]; then
    awk '/[^-0-9,]/' < as.stg1.input
    exit $status
fi

./vm.sh as.stg1.input < src/as.s > as.stg2.input && status=$? || status=$?
if [ $status -ne 0 ]; then
    awk '/[^-0-9,]/' < as.stg2.input
    exit $status
fi

diff as.stg1.input as.stg2.input
cp as.stg2.input src/as.input

rm as.stg1.input
rm as.stg2.input
