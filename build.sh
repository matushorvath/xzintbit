#!/usr/bin/env bash

set -xe

# Use submitted binaries to compile stage 1
mkdir -p stage1

./vm.sh bin/as.input < src/as.s > stage1/as.o && status=$? || status=$?
if [ $status -ne 0 ]; then
    awk '/[^-0-9,]/' < stage1/as.o
    exit $status
fi

echo .$ | cat stage1/as.o - | ./vm.sh bin/ld.input > stage1/as.input && status=$? || status=$?
if [ $status -ne 0 ]; then
    awk '/[^-0-9,]/' < stage1/as.input
    exit $status
fi

./vm.sh bin/as.input < src/ld.s > stage1/ld.o && status=$? || status=$?
if [ $status -ne 0 ]; then
    awk '/[^-0-9,]/' < stage1/ld.o
    exit $status
fi

echo .$ | cat stage1/ld.o - | ./vm.sh bin/ld.input > stage1/ld.input && status=$? || status=$?
if [ $status -ne 0 ]; then
    awk '/[^-0-9,]/' < stage1/ld.input
    exit $status
fi

# Use stage 1 binaries to compile stage 2
mkdir -p stage2

./vm.sh stage1/as.input < src/as.s > stage2/as.o && status=$? || status=$?
if [ $status -ne 0 ]; then
    awk '/[^-0-9,]/' < stage2/as.o
    exit $status
fi

echo .$ | cat stage2/as.o - | ./vm.sh stage1/ld.input > stage2/as.input && status=$? || status=$?
if [ $status -ne 0 ]; then
    awk '/[^-0-9,]/' < stage2/as.input
    exit $status
fi

./vm.sh stage1/as.input < src/ld.s > stage2/ld.o && status=$? || status=$?
if [ $status -ne 0 ]; then
    awk '/[^-0-9,]/' < stage2/ld.o
    exit $status
fi

echo .$ | cat stage2/ld.o - | ./vm.sh stage1/ld.input > stage2/ld.input && status=$? || status=$?
if [ $status -ne 0 ]; then
    awk '/[^-0-9,]/' < stage2/ld.input
    exit $status
fi

# Compare outputs and install the new compiler

diff -r stage1/as.input stage2/as.input
diff -r stage1/ld.input stage2/ld.input

cp stage2/as.input bin/as.input
cp stage2/ld.input bin/ld.input

rm -rf stage1
rm -rf stage2
