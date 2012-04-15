#!/bin/bash

KAIRA_ROOT=$(readlink -f `dirname "$0"`)

echo "Building Cailie ... "
cd $KAIRA_ROOT/lib || exit 1;
scons -Q || exit 1

echo "Building CaServer ... "
cd $KAIRA_ROOT/libs/caserver || exit 1;
scons -Q || exit 1

type -P mpicc &>/dev/null || {
echo "MPI not found. Kaira is compiled without MPI support."
echo "MPI is not necessary to running Kaira."
echo "If you want to use MPI, please install MPI and run build.sh again."; exit 0;
}

echo "Building Cailie-MPI ..."
cd $KAIRA_ROOT/lib || abort;
scons -Q build-mpi
