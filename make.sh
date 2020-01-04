#!/usr/bin/env bash

set -xe

./vm.sh src/as.input < src/as.s > src/as.stg1.input && status=$? || status=$?
if [ $status -ne 0 ]; then
    awk '/[^-0-9,]/' < src/as.stg1.input
    exit $status
fi

./vm.sh src/as.stg1.input < src/as.s > src/as.stg2.input && status=$? || status=$?
if [ $status -ne 0 ]; then
    awk '/[^-0-9,]/' < src/as.stg2.input
    exit $status
fi

diff src/as.stg1.input src/as.stg2.input
cp src/as.stg2.input src/as.input

rm src/as.stg1.input
rm src/as.stg2.input
