#!/bin/bash
echo "Building C library ... "
cd `dirname $0`/lib || abort;
scons -Q || exit 1
cd ..
echo "Building Project-To-Program compiler ... "
cd `dirname $0`/ptp || exit 1
ghc --make Main.hs -o ptp || exit 1

type -P mpicc &>/dev/null || {
echo "MPI not found. Kaira is compiled without MPI support."
echo "MPI is not necessary to running Kaira."
echo "If you want to use MPI, please install MPI and run build.sh again."; exit 0; 
}

cd ../lib
scons -Q build-mpi
