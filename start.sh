#!/bin/sh
cd `dirname $0`/gui
exec python app.py $@
