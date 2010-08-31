#!/bin/sh
echo "Building C library ... "
cd `dirname $0`/lib || abort
make || abort
cd ..
echo "Building Project-To-Program compiler ... "
cd `dirname $0`/ptp || abort
ghc --make Main.hs -o ptp || abort
