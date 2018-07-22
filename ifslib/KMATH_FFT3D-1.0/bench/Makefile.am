#--------1---------2---------3---------4---------5---------6---------7---------8
#
#  File     : Makefile
#  Authors  : TI, NT
#  Modified : 2013/01/22 (NT)
#
#  (c) Copyright 2013 RIKEN. All rights reserved.
#
#--------1---------2---------3---------4---------5---------6---------7---------8

LIBDIR  = ../src
LIBFILE	= $(LIBDIR)/libkmath_fft_3d.a

bin_PROGRAMS =	bench_km_fft3d
bench_km_fft3d_SOURCES = bench_km_fft3d.f
bench_km_fft3d_LDADD = $(LIBFILE) $(LDFLAGS)

.f.o:
	$(FC) -c $(FCFLAGS) $(FCFLAGS_for) -I$(LIBDIR) $*.f

clean:
	rm -f *.mod *.o *~ *.bak \
	     bench_km_fft3d

