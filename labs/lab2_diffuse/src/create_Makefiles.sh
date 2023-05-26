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


if [[ ${Compiler} == "nvfortran" && ${cpu_or_gpu} == "gpu" ]]; then
###########################################################################
# NVFortran GPU Makefile Creation
###########################################################################
Source_codes=("Original" "New" "Experimental")
Compile_flags=("-acc=gpu -Minfo=accel" "-acc=gpu -stdpar=gpu -Minfo=accel -Minfo=stdpar" "-stdpar=gpu -Minfo=accel -Minfo=stdpar" " ")
Makefile_compiler="${Compiler}_GPU"
Compiler="nvfortran"
Prefix="/opt/psi/nv"

for i in ${!Source_codes[@]}; do
   if [[ ${Code} == ${Source_codes[$i]} ]]; then
   	cp Makefile.template Makefile_${Makefile_compiler}_${Source_codes[$i]}
        sed -i "s#<FFLAGS>#${FFLAGS_NV_GPU}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
        sed -i "s#<Compile_Flag>#${Compile_flags[$i]}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
        sed -i "s#<Target>#diffuse_${Makefile_compiler}_${Source_codes[$i]}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
        sed -i "s#<Compiler>#${Compiler}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
	sed -i "s#<type>#${Source_codes[$i]}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
        sed -i "s#<Prefix>#${Prefix}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
   fi
done

elif [[ ${Compiler} == "nvfortran" && ${cpu_or_gpu} == "cpu" && ${Code} != "Original_mp" ]]; then
###########################################################################
# NVFortran CPU Makefile Creation
###########################################################################
Source_codes=("Original" "New" "Experimental" "Serial")
Compile_flags=("-acc=multicore -Minfo=accel" "-acc=multicore -stdpar=multicore -Minfo=accel -Minfo=stdpar" "-stdpar=multicore -Minfo=accel -Minfo=stdpar" " ")
Makefile_compiler="${Compiler}_CPU"
Compiler="nvfortran"
Prefix="/opt/psi/nv"

for i in ${!Source_codes[@]}; do
   if [[ ${Code} == ${Source_codes[$i]} ]]; then
	cp Makefile.template Makefile_${Makefile_compiler}_${Source_codes[$i]}
        sed -i "s#<FFLAGS>#${FFLAGS_NV_CPU}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
        sed -i "s#<Compile_Flag>#${Compile_flags[$i]}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
        sed -i "s#<Target>#diffuse_${Makefile_compiler}_${Source_codes[$i]}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
        sed -i "s#<Compiler>#${Compiler}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
        sed -i "s#<type>#${Source_codes[$i]}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
        sed -i "s#<Prefix>#${Prefix}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
   fi
done

elif [[ ${Compiler} == "nvfortran" && ${cpu_or_gpu} == "cpu" && ${Code} == "Original_mp" ]]; then
###########################################################################
# NVFortran CPU MP  Makefile Creation
###########################################################################
Source_codes=("Original")
Compile_flags=("-mp -Minfo=accel")
Makefile_compiler="${Compiler}_CPU"
Compiler="nvfortran"
Prefix="/opt/psi/nv"

        cp Makefile.template Makefile_${Makefile_compiler}_${Source_codes}_mp
        sed -i "s#<FFLAGS>#${FFLAGS_GF_GPU}#g" Makefile_${Makefile_compiler}_${Source_codes}_mp
        sed -i "s#<Compile_Flag>#${Compile_flags}#g" Makefile_${Makefile_compiler}_${Source_codes}_mp
        sed -i "s#<Target>#diffuse_${Makefile_compiler}_${Source_codes}_mp#g" Makefile_${Makefile_compiler}_${Source_codes}_mp
        sed -i "s#<Compiler>#${Compiler}#g" Makefile_${Makefile_compiler}_${Source_codes}_mp
        sed -i "s#<type>#${Source_codes}#g" Makefile_${Makefile_compiler}_${Source_codes}_mp
        sed -i "s#<Prefix>#${Prefix}#g" Makefile_${Makefile_compiler}_${Source_codes}_mp

elif [[ ${Compiler} == "gfortran" && ${cpu_or_gpu} == "gpu" ]]; then
###########################################################################
# GFortran GPU Makefile Creation
###########################################################################
Source_codes=("Original")
Compile_flags=("-fopenacc -foffload=nvptx-none ${GF_GPU_PARALLEL}")
Makefile_compiler="${Compiler}_GPU"
Compiler="gfortran"
Prefix="/opt/psi/gnu"

        cp Makefile.template Makefile_${Makefile_compiler}_${Source_codes}
        sed -i "s#<FFLAGS>#${FFLAGS_GF_GPU}#g" Makefile_${Makefile_compiler}_${Source_codes}
        sed -i "s#<Compile_Flag>#${Compile_flags}#g" Makefile_${Makefile_compiler}_${Source_codes}
        sed -i "s#<Target>#diffuse_${Makefile_compiler}_${Source_codes}#g" Makefile_${Makefile_compiler}_${Source_codes}
        sed -i "s#<Compiler>#${Compiler}#g" Makefile_${Makefile_compiler}_${Source_codes}
        sed -i "s#<type>#${Source_codes}#g" Makefile_${Makefile_compiler}_${Source_codes}
        sed -i "s#<Prefix>#${Prefix}#g" Makefile_${Makefile_compiler}_${Source_codes}

elif [[ ${Compiler} == "gfortran" && ${cpu_or_gpu} == "cpu" ]]; then
###########################################################################
# GFortran CPU Makefile Creation
###########################################################################
Source_codes=("Original" "New" "Experimental" "Serial")
Compile_flags=("-fopenmp" "-fopenmp ${GF_CPU_PARALLEL}" "${GF_CPU_PARALLEL}" " ")
Makefile_compiler="${Compiler}_CPU"
Compiler="gfortran"
Prefix="/opt/psi/gnu"

for i in ${!Source_codes[@]}; do
   if [[ ${Code} == ${Source_codes[$i]} ]]; then
	cp Makefile.template Makefile_${Makefile_compiler}_${Source_codes[$i]}
	sed -i "s#<FFLAGS>#${FFLAGS_GF_CPU}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
	sed -i "s#<Compile_Flag>#${Compile_flags[$i]}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
	sed -i "s#<Target>#diffuse_${Makefile_compiler}_${Source_codes[$i]}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
	sed -i "s#<Compiler>#${Compiler}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
        sed -i "s#<type>#${Source_codes[$i]}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
        sed -i "s#<Prefix>#${Prefix}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
   fi
done
else
###########################################################################
# ifort CPU Makefile Creation
###########################################################################
Source_codes=("Original" "New" "Serial")
Compile_flags=("-fopenmp" "-fopenmp" " ")
Makefile_compiler="${Compiler}_CPU"
Compiler="ifort"
Prefix="/opt/psi/intel"

for i in ${!Source_codes[@]}; do
   if [[ ${Code} == ${Source_codes[$i]} ]]; then
	cp Makefile.template Makefile_${Makefile_compiler}_${Source_codes[$i]}
        sed -i "s#<FFLAGS>#${FFLAGS_if_CPU}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
        sed -i "s#<Compile_Flag>#${Compile_flags[$i]}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
        sed -i "s#<Target>#diffuse_${Makefile_compiler}_${Source_codes[$i]}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
        sed -i "s#<Compiler>#${Compiler}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
        sed -i "s#<type>#${Source_codes[$i]}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
        sed -i "s#<Prefix>#${Prefix}#g" Makefile_${Makefile_compiler}_${Source_codes[$i]}
   fi
done
fi
