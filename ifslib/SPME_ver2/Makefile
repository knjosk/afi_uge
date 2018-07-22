FC = ifort -mkl
#FFLAGS = -g -CB -traceback
FFLAGS = -O3 -fpic
OBJS = 	spme_all.o utility.o pmesh_kspace.o bspline.o fftcalls.o charge_grid.o spme_init.o utility_sp.o dir_spline_interp.o
OBJSP = spme_prmt.o spme_prmt_sp.o utility_prmt.o reg_kspace.o spme_prec.o spme_prec_sp.o
LIB = spme.a

$(LIB): $(OBJSP) $(OBJS)
	ar rc $(LIB) $(OBJSP) $(OBJS)

fftcalls.o: fftcalls.f
	$(FC) -c $(FFLAGS) fftcalls.f

dir_spline_interp.o: dir_spline_interp.f
	$(FC) -c $(FFLAGS) dir_spline_interp.f

.f.o:
	$(FC) -c $(FFLAGS) -qopenmp $*.f

clean:
	rm -rf *.o *.a
