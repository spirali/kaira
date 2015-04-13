#!/bin/sh
SELF=`dirname $0`
RUN_PYTHON=${SELF}/build/run_python

if [ -f ${RUN_PYTHON} ]
then
    sh ${RUN_PYTHON} ${SELF}/gui/app.py $@
else
    echo "Kaira is not correctly installed."
    echo "Run ./waf configure in Kaira directory"
fi
