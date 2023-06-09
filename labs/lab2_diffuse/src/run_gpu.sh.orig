#!/bin/bash

###########################################################################
#
# Assigning input variables
#
###########################################################################
Compiler=$1
Code=$2
###########################################################################

cd results
###########################################################################
#
#  Run the codes in the singularity containers on the CPU
#
###########################################################################

#nvfortran
if [[ ${Compiler} == "nvfortran" ]]; then
	singularity exec --nv --home ${PWD}/.. ../singularity_containers/nvfortran.sif ../src/run_diffuse_p1.sh ${Compiler}_GPU_${Code} ../bin/diffuse_${Compiler}_GPU_${Code}
#gfortran
else
	singularity exec --nv --home ${PWD}/.. ../singularity_containers/gfortran.sif ../src/run_diffuse_p1.sh ${Compiler}_GPU_${Code} ../bin/diffuse_${Compiler}_GPU_${Code}
fi





