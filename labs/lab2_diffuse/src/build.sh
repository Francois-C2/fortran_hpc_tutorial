#!/bin/bash

###########################################################################
#
# Assigning input variables
#
###########################################################################
Compiler=$1
cpu_or_gpu=$2
Code=$3
GF_CPU_PARALLEL=$4
GF_GPU_PARALLEL=$5
FFLAGS_NV_CPU=$6
FFLAGS_GF_CPU=$7
FFLAGS_if_CPU=$8
FFLAGS_NV_GPU=$9
FFLAGS_GF_GPU=${10}
###########################################################################

cd src/
# Create all the Makefiles and move them to tmp
./create_Makefiles.sh ${Compiler} ${cpu_or_gpu} ${Code} "${GF_CPU_PARALLEL}" "${GF_GPU_PARALLEL}" \
        "${FFLAGS_NV_CPU}" "${FFLAGS_GF_CPU}" "${FFLAGS_if_CPU}" "${FFLAGS_NV_GPU}" "${FFLAGS_GF_GPU}"
mv Makefile_* ../tmp
cd ../tmp

###########################################################################
#Make all the codes and move them to bin
###########################################################################
#nvfortran GPU
###########################################################################
if [[ ${Compiler} == "nvfortran" && ${cpu_or_gpu} == "gpu" ]]; then
        Makefile_compiler="${Compiler}_GPU"
        Singularity="nvfortran.sif"
        singularity exec --nv --home ${PWD}/.. ../singularity_containers/${Singularity} make -f Makefile_${Makefile_compiler}_${Code} 1>make_${Makefile_compiler}_${Code}.log 2>make_${Makefile_compiler}_${Code}.log
        mv diffuse_${Makefile_compiler}_${Code} ../bin/
        #clean
        rm -f *.mod *.o

###########################################################################
#nvfortran CPU
###########################################################################
elif [[ ${Compiler} == "nvfortran" && ${cpu_or_gpu} == "cpu" ]]; then
        Makefile_compiler="${Compiler}_CPU"
        Singularity="nvfortran.sif"
        singularity exec --home ${PWD}/.. ../singularity_containers/${Singularity} make -f Makefile_${Makefile_compiler}_${Code} 1>make_${Makefile_compiler}_${Code}.log 2>make_${Makefile_compiler}_${Code}.log
        mv diffuse_${Makefile_compiler}_${Code} ../bin/
        #clean
        rm -f *.mod *.o

###########################################################################
#gfortran GPU
###########################################################################
elif [[ ${Compiler} == "gfortran" && ${cpu_or_gpu} == "gpu" ]]; then
        Makefile_compiler="${Compiler}_GPU"
        Singularity="gfortran.sif"
        singularity exec --nv --home ${PWD}/.. ../singularity_containers/${Singularity} make -f Makefile_${Makefile_compiler}_${Code} 1>make_${Makefile_compiler}_${Code}.log 2>make_${Makefile_compiler}_${Code}.log
        mv diffuse_${Makefile_compiler}_${Code} ../bin/
        #clean
        rm -f *.mod *.o

###########################################################################
#gfortran CPU
###########################################################################
elif [[ ${Compiler} == "gfortran" && ${cpu_or_gpu} == "cpu" ]]; then
        Makefile_compiler="${Compiler}_CPU"
        Singularity="gfortran.sif"
        singularity exec --home ${PWD}/.. ../singularity_containers/${Singularity} make -f Makefile_${Makefile_compiler}_${Code} 1>make_${Makefile_compiler}_${Code}.log 2>make_${Makefile_compiler}_${Code}.log
        mv diffuse_${Makefile_compiler}_${Code} ../bin/
        #clean
        rm -f *.mod *.o

###########################################################################
#ifort CPU
###########################################################################
else
        Makefile_compiler="${Compiler}_CPU"
        Singularity="ifort.sif"
        singularity exec --home ${PWD}/.. ../singularity_containers/${Singularity} make -f Makefile_${Makefile_compiler}_${Code} 1>make_${Makefile_compiler}_${Code}.log 2>make_${Makefile_compiler}_${Code}.log
        mv diffuse_${Makefile_compiler}_${Code} ../bin/
        #clean
        rm -f *.mod *.o
fi


