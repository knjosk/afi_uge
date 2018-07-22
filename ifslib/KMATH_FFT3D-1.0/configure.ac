#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

#AC_PREREQ([2.69])
AC_INIT([KMATH_FFT], [1.0], [])
AM_INIT_AUTOMAKE
AC_CONFIG_SRCDIR([src/param.h])
AC_CONFIG_HEADERS([config.h])

# compilers from environment variables ?
f77_defined=yes
if test "x${F77}" = x ; then
   # F77 not assigned
   f77_defined=no
fi
fc_defined=yes
if test "x${FC}" = x ; then
   # FC not assigned
   fc_defined=no
fi

# Checks for programs.
AC_PROG_LN_S
AC_PROG_RANLIB

# Is FC MPI compiler ?
AC_LANG_PUSH([Fortran])
AC_MSG_CHECKING([whether $FC is MPI Fortran compiler])
ac_mpi_fc=no
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],[
      include 'mpif.h'
])], [ac_mpi_fc=yes])
AC_MSG_RESULT([$ac_mpi_fc])
AC_LANG_POP([Fortran])

if test "x$ac_mpi_fc" = xno ; then
    # not MPI compiler
    # search MPI Fortran compiler
    AC_MSG_NOTICE(search MPI fortran compiler)
    CANDIDATE_MPIF90="mpifrtpx mpiifort mpif90 mpif77"
    AC_PATH_PROGS([MPIF90], [$CANDIDATE_MPIF90])
    FC=${MPIF90}
fi

AC_PROG_FC

if test "x$f77_defined" = xno ; then
   # if no F77 , same as FC
   F77=${FC}
fi

AC_PROG_F77

# free format
AC_FC_FREEFORM

# preprocessor
AC_FC_PP_SRCEXT(for)

# openmp
AC_LANG_PUSH([Fortran])
AC_OPENMP
AC_LANG_POP([Fortran])

LDFLAGS="$LDFLAGS $OPENMP_FCFLAGS"

AC_CONFIG_FILES([
	Makefile
	src/Makefile
	bench/Makefile
	])
AC_OUTPUT
