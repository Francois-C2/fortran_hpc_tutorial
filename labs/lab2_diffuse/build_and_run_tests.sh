#!/bin/bash
#####################################################################################
#
#   build_and_run_tests.sh <ARG>  (see below for options for <ARG>)
#
#####################################################################################
#
# IMPORTANT!   Please set your architecture-specific options here:
#
#  For multi-core CPU runs, set NUM_CPU_THREADS to be the total number of threads to run on.
#  Typically, this will be the same as the number of CPU cores.
#
   NUM_CPU_THREADS=4
#
#  For CPUs, please indicate the vectorization capabilities to use with the Intel compiler.
#  Some examples are:
#    Intel Haswell/Broadwell:            core-avx2
#    Intel Skylake/Cascadelake/Icelake:  core-avx512
#    AMD EPYC:                           core-avx2
#
   CPU_VEC_INSTR=core-avx2
#
#  For NVIDIA GPU accelerated tests with nvfortran, set the compute capability of your
#  NVIDIA GPU. See "https://en.wikipedia.org/wiki/CUDA" for a complete list.
#  Some recent examples are:
#  V100:           cc70             RTX 20-series:  cc75
#  A100:           cc80             RTX 30-series:  cc86
#  Otherwise, the default "ccall" will compile for all supported GPUs (slow!).
#
   NV_GPU_CC="ccall"
#
#  For gfortran NVIDIA GPU runs, it is best tp specify the desired vector length.
#  In our tests, the nvfortran sets this to 128, so it is best to leave that number here.
#
   GF_GPU_VEC_LENGTH="128"
#
#####################################################################################
#
#   Options for script argument <ARG>:
#
#   ALL                     : runs all GPU and CPU cases
#   CPU_All                 : runs only CPU cases
#   GPU                     : runs only GPU cases
#   CPU_Parallel            : runs only parallel CPU cases
#   CPU_Serial              : runs only serial CPU cases
#
#   You can also run a custom set of cases using an array, i.e. :
#
#  ./build_run_test "1 2 3"
#
#  where each number corresponds with the following cases:
#
#      1 - nvfortran_CPU_Original       (Original code, nvfortran, CPU, parallel, OpenACC)
#      2 - nvfortran_CPU_Original_mp    (Original code, nvfortran, CPU, parallel, OpenMP)
#      3 - nvfortran_CPU_New            (New code, nvfortran, CPU, parallel, DC+OpenACC)
#      4 - nvfortran_CPU_EXP            (Experimental code, nvfortran, CPU, parallel, DC)
#      5 - gfortran_CPU_Original        (Original code, gfortran, CPU, parallel, OpenMP)
#      6 - gfortran_CPU_New             (New code, gfortran, CPU, parallel, DC+OpenMP)
#      7 - gfortran_CPU_EXP             (Experimental code, gfortran, CPU, parallel, DC)
#      8 - ifort_CPU_Original           (Original code, ifort, CPU, parallel, OpenMP)
#      9 - ifort_CPU_New                (New code, ifort, CPU, parallel, DC+OpenMP)
#     10 - nvfortran_CPU_Serial         (Serial code, nvfortran, CPU, serial, None)
#     11 - gfortran_CPU_Serial          (Serial code, gfortran, CPU, serial, None)
#     12 - ifort_CPU_Serial             (Serial code, ifort, CPU, serial, None)
#     13 - nvfortran_GPU_Original       (Original code, nvfortran, GPU, parallel, OpenACC)
#     14 - nvfortran_GPU_New            (New code, nvfortran, GPU, parallel, DC+OpenACC)
#     15 - nvfortran_GPU_EXP            (Experimental code, nvfortran, GPU, parallel, DC)
#     16 - gfortran_GPU_Original        (Original code, gfortran, GPU, parallel, OpenACC)
#
#####################################################################################
#####################################################################################
#
#  Setting CPU mulit-core parallel thread counts to chosen number of threads:
#
#------------------------------------------------------------------------------------
#
GF_CPU_PARALLEL="-ftree-parallelize-loops=${NUM_CPU_THREADS}"
export OMP_NUM_THREADS=$NUM_CPU_THREADS
export ACC_NUM_CORES=$NUM_CPU_THREADS
#
#------------------------------------------------------------------------------------
#
#  -------- Parallelization flags for gfortran GPU runs
#
#  Specify the parallel topology parallel offload regions.
#  The value is a triple of ’:’-separated sizes, specifying
#  ’gang’, ’worker’ and, ’vector’ level parallelism.
#  Any size can be omitted.
#
#------------------------------------------------------------------------------------
GF_GPU_PARALLEL="-fopenacc-dim=::${GF_GPU_VEC_LENGTH}"
#------------------------------------------------------------------------------------
#
#  -------- Compiler-specific default flags and archetecture flags.
#
#------------------------------------------------------------------------------------
# ---- For CPU with nvfortran
FFLAGS_NV_CPU="-march=native"
# ---- For CPU with gfortran
FFLAGS_GF_CPU='-march=native -fcray-pointer -fallow-argument-mismatch -fallow-invalid-boz'
# ---- For CPU with ifort
FFLAGS_if_CPU="-march=${CPU_VEC_INSTR} -fp-model precise -heap-arrays"
# ---- For GPU with nvfortran
FFLAGS_NV_GPU="-gpu=${NV_GPU_CC},cuda11.4"
# ---- For GPU with gfortran
FFLAGS_GF_GPU="-march=native"
#
#####################################################################################
#
# Formulate set of runs:
#
if [ "$1" ==  "CPU_Parallel" ]; then
	To_Run=( 1 2 3 4 5 6 7 8 9 )
elif [ "$1" ==  "CPU_Serial" ]; then
	To_Run=( 10 11 12 )
elif [ "$1" ==  "CPU_All" ]; then
	To_Run=( 1 2 3 4 5 6 7 8 9 10 11 12 )
elif [ "$1" ==  "GPU" ]; then
	To_Run=( 13 14 15 16 )
elif [ "$1" ==  "ALL" ]; then
	To_Run=( 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
elif [[ $1 =~ ^[A-Za-z_]+ ]]; then
	echo 'Please enter a valid input.'
else
	To_Run=(${1})
	for i in ${!To_Run[@]}; do
        	if ! [[ ${To_Run[$i]} -gt 0 && ${To_Run[$i]} -lt 17 ]]; then
			echo 'Please enter a valid input.'
			break
		fi
	done
fi

# Main build and run code
for j in ${!To_Run[@]}; do
	# Compiler selection
	if [[ ${To_Run[$j]} -eq 1 || ${To_Run[$j]} -eq 2 || ${To_Run[$j]} -eq 3 || \
		${To_Run[$j]} -eq 4 || ${To_Run[$j]} -eq 10 || ${To_Run[$j]} -eq 13 ||\
	       	${To_Run[$j]} -eq 14 || ${To_Run[$j]} -eq 15 ]]; then
		Compiler=nvfortran
	elif [[ ${To_Run[$j]} -eq 5 || ${To_Run[$j]} -eq 6 || ${To_Run[$j]} -eq 7 || \
                ${To_Run[$j]} -eq 11 || ${To_Run[$j]} -eq 16 ]]; then
                Compiler=gfortran
	else
                Compiler=ifort
	fi
	# CPU or GPU selection
        if [[ ${To_Run[$j]} -lt 13 ]]; then
                cpu_or_gpu=cpu
	else
		cpu_or_gpu=gpu
        fi
	# Code selections
	if [[ ${To_Run[$j]} -eq 1 || ${To_Run[$j]} -eq 5 || ${To_Run[$j]} -eq 8 || \
                ${To_Run[$j]} -eq 13 || ${To_Run[$j]} -eq 16 ]]; then
                Code=Original
        elif [[ ${To_Run[$j]} -eq 3 || ${To_Run[$j]} -eq 6 || ${To_Run[$j]} -eq 9 || \
                ${To_Run[$j]} -eq 14 ]]; then
                Code=New
	elif [[ ${To_Run[$j]} -eq 4 || ${To_Run[$j]} -eq 7 || ${To_Run[$j]} -eq 15 ]]; then
                Code=Experimental
	elif [[ ${To_Run[$j]} -eq 10 || ${To_Run[$j]} -eq 11 || ${To_Run[$j]} -eq 12 ]]; then
                Code=Serial
	else
		Code=Original_mp
	fi
	# Build the codes
	echo "Building codes..."
	./src/build.sh ${Compiler} ${cpu_or_gpu} ${Code} "${GF_CPU_PARALLEL}" "${GF_GPU_PARALLEL}" \
	"${FFLAGS_NV_CPU}" "${FFLAGS_GF_CPU}" "${FFLAGS_if_CPU}" "${FFLAGS_NV_GPU}" "${FFLAGS_GF_GPU}"
	# Run scripts
	echo "Running ${Code} with ${Compiler} ..."
	if [[ ${cpu_or_gpu} == cpu ]]; then
		./src/run_cpu.sh ${Compiler} ${Code}
	else
		./src/run_gpu.sh ${Compiler} ${Code}
	fi
done
echo "Collecting results into timing_results.txt..."
# time file creation
singularity exec --home ${PWD} singularity_containers/ifort.sif python3 src/get_time.py
cat timing_results.txt

