#--------1---------2---------3---------4---------5---------6---------7---------8
#
#  File     : Makefile
#  Authors  : TI, NT
#  Modified : 2013/01/21 (NT)
#
#  (c) Copyright 2013 RIKEN. All rights reserved.
#
#--------1---------2---------3---------4---------5---------6---------7---------8

LIBFILE = libkmath_fft_3d.a 
lib_LIBRARIES	= $(LIBFILE)

moddir = $(libdir)
mod_DATA = kmath_dft_mod.mod \
	kmath_msg_mod.mod  \
	kmath_time_mod.mod \
	kmath_transpose_mod.mod \
	kmath_fft3d_mod.mod

libkmath_fft_3d_a_SOURCES	= \
	ffte_kernel.f \
	ffte_factor.f \
	ffte_fft235.f \
	ffte_zfft1d.f \
	kmath_dft.F   \
	kmath_msg.F   \
	kmath_time.F  \
	kmath_transpose.F \
	kmath_fft3d.F

.f.o:
	$(F77) -c $(FFLAGS) $*.f

.fpp.o:
	$(FC) -c $(FCFLAGS) $(FCFLAGS_for) $*.fpp

.F.o:
	$(FC) -c $(FCFLAGS) $(FCFLAGS_for) $*.F

%.mod: %.F %.o
	@:
%_mod.mod: %.F %.o
	@:

clean:
	rm -f *.mod *.o *~ *.bak $(LIBFILE)

