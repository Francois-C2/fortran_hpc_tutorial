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
	singularity exec --home ${PWD}/.. ../singularity_containers/nvfortran.sif ../src/run_diffuse_p1.sh ${Compiler}_CPU_${Code} ../bin/diffuse_${Compiler}_CPU_${Code}
#gfortran
elif [[ ${Compiler} == "gfortran" ]]; then
	singularity exec --home ${PWD}/.. ../singularity_containers/gfortran.sif ../src/run_diffuse_p1.sh ${Compiler}_CPU_${Code} ../bin/diffuse_${Compiler}_CPU_${Code}
#ifort
else
	singularity exec --home ${PWD}/.. ../singularity_containers/ifort.sif ../src/run_diffuse_p1.sh ${Compiler}_CPU_${Code} ../bin/diffuse_${Compiler}_CPU_${Code}
fi


