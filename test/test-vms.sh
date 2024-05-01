#!/bin/sh

set -e

# Expects MAKE, TESTLOG and TESTDIRS to be set

rm -rf $TESTLOG
failed=0

for type in $@
do
    echo "====================" >> $TESTLOG
    echo "ICVM_TYPE = $type" >> $TESTLOG
    for testdir in $TESTDIRS
    do
        ICVM_TYPE=$type $MAKE -C $testdir test || failed=1
    done
done

cat test/test.log
[ $failed = 0 ] || exit 1
