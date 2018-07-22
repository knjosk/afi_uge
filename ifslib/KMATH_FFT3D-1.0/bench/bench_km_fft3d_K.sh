#!/bin/bash
#PJM --rsc-list "rscgrp=small"
#PJM --rsc-list "node=48"
#PJM --rsc-list "elapse=02:00:00"
#PJM -S
#PJM --stg-transfiles all
#PJM --mpi "use-rankdir"
#PJM --stgin "rank=* ./bench_km_fft3d %r:./"
#PJM --stgout "rank=0 ./_dif_fft3d_dft3d ./"
#PJM --stgout "rank=0 ./_out_fft3d ./"

. /work/system/Env_base

export PARALLEL=8
export OMP_NUM_THREADS=$PARALLEL

mpiexec ./bench_km_fft3d 60 60 60 4 4 3 2 2 1 0 1

