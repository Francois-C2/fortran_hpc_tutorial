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
	singularity exec --home ${PWD}/.. /bootcamp_scripts/ISO_PROGRAMMING/stdpar_sc.simg ../src/run_diffuse_p1.sh ${Compiler}_CPU_${Code} ../bin/diffuse_${Compiler}_CPU_${Code}
#gfortran
elif [[ ${Compiler} == "gfortran" ]]; then
	singularity exec --home ${PWD}/.. /bootcamp_scripts/ISO_PROGRAMMING/stdpar_sc.simg ../src/run_diffuse_p1.sh ${Compiler}_CPU_${Code} ../bin/diffuse_${Compiler}_CPU_${Code}
#ifort
else
	singularity exec --home ${PWD}/.. /bootcamp_scripts/ISO_PROGRAMMING/stdpar_sc.simg ../src/run_diffuse_p1.sh ${Compiler}_CPU_${Code} ../bin/diffuse_${Compiler}_CPU_${Code}
fi


