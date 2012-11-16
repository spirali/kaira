#!/bin/bash

KAIRA_ROOT=$(readlink -f `dirname "$0"`)

echo "Building Cailie ... "
cd $KAIRA_ROOT/libs/cailie || exit 1;
scons -Q || exit 1

echo "Building CaServer ... "
cd $KAIRA_ROOT/libs/caserver || exit 1;
scons -Q || exit 1

echo "Building CaClient ... "
cd $KAIRA_ROOT/libs/caclient || exit 1;
scons -Q || exit 1

echo "Building CaVerif ... "
cd $KAIRA_ROOT/libs/caverif || exit 1;
scons -Q || {
	echo "------------------------------------------------------------------"
	echo "Building of CaVerif failed."
    echo "CaVerif is not necessary for basic usage of Kaira,"
	echo "but some analysis will be not available"
	echo "------------------------------------------------------------------"
}

type -P mpicc &>/dev/null || {
echo "------------------------------------------------------------------"
echo "MPI not found. Kaira is compiled without MPI support."
echo "MPI is not necessary to running Kaira."
echo "If you want to use MPI, please install MPI and run build.sh again."; exit 0;
echo "------------------------------------------------------------------"
}

echo "Building Cailie-MPI ..."
cd $KAIRA_ROOT/libs/cailie || abort;
scons -Q build-mpi
