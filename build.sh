#!/bin/sh
echo "Building C librabry ... "
cd `dirname $0`/lib || abort
make || abort
cd ..
echo "Building Project-To-Program compiler librabry ... "
cd `dirname $0`/ptp || abort
ghc --make Main.hs -o ptp || abort
