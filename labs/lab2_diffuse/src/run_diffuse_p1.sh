#!/bin/bash

TEST_NAME=$1
DIFFUSE_EXE=$2
BRFILE=../../input/br_input.h5

mkdir test_$TEST_NAME 2>/dev/null
cd test_$TEST_NAME

execline="../${DIFFUSE_EXE} -v ${BRFILE} -pt -viscgrid br_smoothed.h5"

{ time $execline 1> diffuse.log 2> diffuse.log ; } 2> diffuse.time

cd ..
