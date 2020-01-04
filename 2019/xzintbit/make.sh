#!/bin/sh

set -xe

npx ts-node src/ic.ts src/as.input < src/as.s > src/as.stg1.input
npx ts-node src/ic.ts src/as.stg1.input < src/as.s > src/as.stg2.input

diff src/as.stg1.input src/as.stg2.input
cp src/as.stg2.input src/as.input

rm src/as.stg1.input
rm src/as.stg2.input
