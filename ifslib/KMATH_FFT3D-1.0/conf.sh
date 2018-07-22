#!/usr/bin/bash
export FC=mpiifort
export FCFLAGS='-qopenmp -fpp -O3 '  
export F77=mpiifort
export FFLAGS='-qopenmp -fpp -O3' 
export LDFLAGS='-qopenmp'
./configure 
