#!/bin/bash

#####################################################################################
#
#	This script cleans this package to the original install state.
#	It does not delete the timing text file.
#
#####################################################################################

rm -rf ./tmp/Makefile_* 2>/dev/null
rm -rf ./tmp/*.log 2>/dev/null
rm -rf ./results/test_* 2>/dev/null
rm -rf ./bin/diffuse* 2>/dev/null
rm -rf ./.nv 2>/dev/null



