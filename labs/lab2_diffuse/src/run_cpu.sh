#!/bin/bash

###########################################################################
#
# Assigning input variables
#
###########################################################################
Compiler=$1
Code=$2
###########################################################################

echo "working directory : $PWD"
echo "Compiler: $Compiler"
echo "Code: $Code"

mkdir -p results

cd results
###########################################################################
#
#  Run the codes in the singularity containers on the CPU
#
###########################################################################

#nvfortran
if [[ ${Compiler} == "nvfortran" ]]; then
	export LD_LIBRARY_PATH=/opt/psi/nv/ext_deps/deps/hdf5/lib:$LD_LIBRARY_PATH
	../src/run_diffuse_p1.sh ${Compiler}_CPU_${Code} ../bin/diffuse_${Compiler}_CPU_${Code}
#gfortran
elif [[ ${Compiler} == "gfortran" ]]; then
	export LD_LIBRARY_PATH=/opt/psi/gnu/ext_deps/deps/hdf5/lib:$LD_LIBRARY_PATH
	../src/run_diffuse_p1.sh ${Compiler}_CPU_${Code} ../bin/diffuse_${Compiler}_CPU_${Code}
#ifort
else
	export LD_LIBRARY_PATH=/opt/psi/intel/ext_deps/deps/hdf5/lib:$LD_LIBRARY_PATH
	../src/run_diffuse_p1.sh ${Compiler}_CPU_${Code} ../bin/diffuse_${Compiler}_CPU_${Code}
fi


