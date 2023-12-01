#!/bin/bash
set -e

SCRIPT_DIR=`dirname "$(readlink -f ${BASH_SOURCE[0]} 2>/dev/null || greadlink -f ${BASH_SOURCE[0]} 2>/dev/null)"`
mkdir -p $SCRIPT_DIR/build
cd build
cmake $SCRIPT_DIR
make
