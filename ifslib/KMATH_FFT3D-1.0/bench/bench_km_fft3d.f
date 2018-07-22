
!--------1---------2---------3---------4---------5---------6---------7---------8
!
!  Program  test_km_fft3d
!> @brief   test of KMATH FFT 3D module
!! @authors Toshiyuki Imamura (TI)
!! @date    2013/01/31 (NT)
!
!  (c) Copyright 2013 RIKEN. All rights reserved.
!
!--------1---------2---------3---------4---------5---------6---------7---------8

program bench_km_fft3d

  use kmath_fft3d_mod
  use kmath_time_mod
  use kmath_msg_mod
  use mpi

  implicit none

  ! constant
  integer,parameter   :: command_num = 11
  ! local variable
  complex(kind(0d0)), allocatable :: XA(:)
  integer             :: provided, ierr
  integer             :: comm_size, comm_rank
  integer             :: handle, nproc(3), nsize(3), nstage(3), inverse, i
  integer             :: nall
  character(100)      :: com(command_num)
  integer             :: com_in_num
  integer             :: check ! 0:no check , 1:check
  complex(kind(0d0)), allocatable :: XA2(:)
  integer             :: iargc

  call MPI_Init_thread(MPI_THREAD_MULTIPLE, provided, ierr)
  call MPI_Comm_size  (MPI_COMM_WORLD, comm_size, ierr)
  call MPI_Comm_rank  (MPI_COMM_WORLD, comm_rank, ierr)

  ! Get command line
  com_in_num = iargc()
  if(com_in_num < 6) then
    if(comm_rank == 0) then
      print*,'usage:'
      print*,'  mpirun -n N bench_km_fft3d NSIZE_X _Y _Z' // & 
         ' NPROC_X _Y _Z [NSTAGE_X _Y _Z INVERSE(*1) CHECK(*2)]'
      print*,'      (*1)INVERSE 0:FORWARD (default) ' // &
         '1:INVERSE 2:FORWARD->INVERSE'
      print*,'      (*2)CHECK 0:NO CHECK 1:CHECK (default) '
    end if
    call MPI_FINALIZE(ierr)
    stop
  end if
  do i=1,com_in_num
    call getarg(i,com(i))
  end do
  read(com(1),*) nsize(1)
  read(com(2),*) nsize(2)
  read(com(3),*) nsize(3)
  read(com(4),*) nproc(1)
  read(com(5),*) nproc(2)
  read(com(6),*) nproc(3)

  do i=1,3
    nstage(i) = 1
    if(com_in_num >= 6+i) then
      read(com(6+i),*) nstage(i)
    end if
  end do
  inverse = 0
  if(com_in_num >= 10) then
    read(com(10),*) inverse
  end if
  check = 1
  if(com_in_num >= 11) then
    read(com(11),*) check
  end if

  g_km_main = comm_rank == 0

  call MPI_Bcast(nsize,   3, MPI_Integer4, 0, MPI_COMM_WORLD, ierr)
  call MPI_Bcast(nproc,   3, MPI_Integer4, 0, MPI_COMM_WORLD, ierr)
  call MPI_Bcast(nstage,  3, MPI_Integer4, 0, MPI_COMM_WORLD, ierr)
  call MPI_Bcast(inverse, 1, MPI_Integer4, 0, MPI_COMM_WORLD, ierr)

  call KMATH_Time_Init
  call KMATH_FFT3D_Init(handle, MPI_COMM_WORLD, nsize, nproc, nstage)

  ! Setup dummy input data
  !

  ! read file
  nall = nsize(1)*nsize(2)*nsize(3)
  allocate(XA(nall))

  ! set random test data
  call bench_km_fft3d_setdata(XA, nall)

  ! preparation for check
  if(check == 1 .and. (inverse == 0 .or. inverse == 1)) then
    allocate(XA2(nall))
    call bench_km_dft3d(XA, XA2, nall, nsize, inverse)
  end if

  if(inverse == 0) then
    !FORWARD
    call bench_km_fft3d_f(XA, XA2, nall, handle, comm_rank, nsize, nproc, check)
  else if(inverse == 1) then
    !INVERSE
    call bench_km_fft3d_i(XA, XA2, nall, handle, comm_rank, nsize, nproc, check)
  else
    !FORWARD->INVERSE
    call bench_km_fft3d_fi(XA, nall, handle, comm_rank, nsize, nproc, check)
  end if

  ! Deallocate memory
  !
  deallocate(XA)
  if(check == 1 .and. (inverse == 0 .or. inverse == 1)) then
    deallocate(XA2)
  end if

  ! Finalize
  !
  call KMATH_FFT3D_Finalize(handle)
  call KMATH_Time_Finalize

  !!
  call MPI_FINALIZE(ierr)

end program bench_km_fft3d

! FORWARD bench program
subroutine bench_km_fft3d_f(XA, XA2, nall, handle, comm_rank, nsize, nproc, check)
  use kmath_fft3d_mod
  use kmath_time_mod
  use kmath_msg_mod
  use mpi

  implicit none

  complex(kind(0d0)),  intent(inout) :: XA(nall)
  complex(kind(0d0)),  intent(in)    :: XA2(nall)
  integer,          intent(in)    :: nall
  integer,          intent(in)    :: handle
  integer,          intent(in)    :: comm_rank
  integer,          intent(in)    :: nsize (3)
  integer,          intent(in)    :: nproc (3)
  integer,          intent(in)    :: check

  complex(kind(0d0)), allocatable :: XB(:),X0(:), F(:)
  double precision    :: max_real, max_imag
  integer             :: ierr
  integer             :: ix, iy, iz, nx, ny, nz, nbox
  integer             :: ipx, ipy, ipz

  logical             :: blank
  integer             :: i
  integer             :: nx2, ny2, nz2, nbox2
  integer             :: iproc(3),iproc2(3)
  integer             :: ipx2, ipy2, ipz2
  integer             :: iproc2y_x, iproc2y_z, ny2_x, last_y

  allocate(XB(nall))

  ! setup box F
  nx2 = nsize(1)/nproc(2)
  if (MOD(nsize(1),nproc(2)) /= 0) &
    nx2 = nx2 + 1
  ny2 = nsize(2)/nproc(1)
  if (MOD(nsize(2),nproc(1)) /= 0) &
    ny2 = ny2 + 1
  ny2_x = ny2
  if (MOD(ny2,nproc(3)) /= 0) then
    ny2 = ny2/nproc(3) + 1
  else
    ny2 = ny2/nproc(3)
  end if
  nz2 = nsize(3)
  nbox2 = nx2 * ny2 * nz2
  allocate(F(nbox2))

  ! setup box X0
  nx = nsize(1)/nproc(1)
  ny = nsize(2)/nproc(2)
  nz = nsize(3)/nproc(3)
  if (MOD(nsize(1),nproc(1)) /= 0) &
    nx = nx + 1
  if (MOD(nsize(2),nproc(2)) /= 0) &
    ny = ny + 1
  if (MOD(nsize(3),nproc(3)) /= 0) &
    nz = nz + 1
  nbox = nx * ny * nz
  allocate(X0(nbox))

  ipx = MOD(comm_rank, nproc(1))
  ipy = MOD(comm_rank / nproc(1), nproc(2))
  ipz = comm_rank / (nproc(1)*nproc(2))
  ipx = ipx * nx
  ipy = ipy * ny
  ipz = ipz * nz
  
  blank = .false.

  do ix = 0, nx-1
    do iy = 0, ny-1
      do iz = 0, nz-1
        if (ix+ipx < nsize(1) .and. &
            iy+ipy < nsize(2) .and. &
            iz+ipz < nsize(3)) then
          X0(ix + iy*nx + iz*nx*ny + 1) = &
               XA((ix+ipx) + (iy+ipy)*nsize(1) + (iz+ipz)*nsize(1)*nsize(2) + 1)
        else
          X0(ix + iy*nx + iz*nx*ny + 1) = 0
          blank = .true.
        end if
      end do
    end do
  end do

#if 0
  if (blank) &
    write(6,*) 'Bench_Km_FFT3D> inserted blank data. rank:', comm_rank
#endif

  ! Run FFT
  !
  call KMATH_FFT3D_Transform(handle, X0, F, .false.)

  ! Check values
  !
 
  iproc(1) = MOD(comm_rank, nproc(1))
  iproc(2) = MOD(comm_rank / nproc(1), nproc(2))
  iproc(3) = comm_rank / (nproc(1)*nproc(2))

  iproc2(1) = iproc(2)
  iproc2(2) = iproc(3) + iproc(1) * nproc(3)
  iproc2(3) = 0

  do i = 1, nall
    XA(i) = (0.d0, 0.d0)
  end do

  ipx2 = nx2 * iproc2(1)

  iproc2y_x = iproc2(2)/nproc(3)
  iproc2y_z = mod(iproc2(2),nproc(3))
  ipy2 = iproc2y_x * ny2_x
  last_y = 0
  do i = 1, iproc2y_z
    last_y = last_y + ny2
  end do
  if(last_y > ny2_x) last_y = ny2_x
  ipy2 = ipy2 + last_y    
  if(ipy2 > nsize(2)) ipy2 = nsize(2)

  ipz2 = 0

  do ix = 0, nx2-1
    do iy = 0, ny2-1
      do iz = 0, nz2-1
        if (ix+ipx2 < nsize(1) .and. &
            iy+ipy2 < nsize(2) .and. &
            iz+ipz2 < nsize(3) .and. &
            iy+last_y < ny2_x) then
          XA((ix+ipx2) + (iy+ipy2)*nsize(1) + (iz+ipz2)*nsize(1)*nsize(2) + 1) = &
               F(iz + ix*nz2 + iy*nz2*nx2 + 1)
        end if
      end do
    end do
  end do

  call MPI_Reduce(XA, XB, SIZE(XA), MPI_Double_complex, &
                  MPI_SUM, 0, MPI_COMM_WORLD, ierr)

  if (comm_rank == 0 .and. check == 1) then

    open(11,file='_out_fft3d',status='replace',form='formatted')
    do i = 1, nall
      write(11,*) XB(i)
    end do
    close(11)

    max_real = 0.d0
    max_imag = 0.d0

    open(11,file='_dif_fft3d_dft3d',status='replace',form='formatted')
    do i = 1, nall
      max_real = MAX(max_real, ABS(REAL(XB(i)-XA2(i))))
      max_imag = MAX(max_imag, ABS(IMAG(XB(i)-XA2(i))))
      write(11,*) XB(i)-XA2(i)
    end do
    write(11,*) '##Max Diff: Real:', max_real, ' Imag:', max_imag
    close(11)

  end if

  deallocate(XB,X0,F)

end subroutine bench_km_fft3d_f

! INVERSE program
subroutine bench_km_fft3d_i(XA, XA2, nall, handle, comm_rank, nsize, nproc, check)
  use kmath_fft3d_mod
  use kmath_time_mod
  use kmath_msg_mod
  use mpi

  implicit none

  complex(kind(0d0)),  intent(inout) :: XA(nall)
  complex(kind(0d0)),  intent(in)    :: XA2(nall)
  integer,          intent(in)    :: nall
  integer,          intent(in)    :: handle
  integer,          intent(in)    :: comm_rank
  integer,          intent(in)    :: nsize (3)
  integer,          intent(in)    :: nproc (3)
  integer,          intent(in)    :: check

  complex(kind(0d0)), allocatable :: XB(:),X0(:), F(:)
  double precision    :: max_real, max_imag
  integer             :: ierr
  integer             :: ix, iy, iz, nx, ny, nz, nbox
  integer             :: ipx, ipy, ipz

  logical             :: blank
  integer             :: i
  integer             :: nx2, ny2, nz2, nbox2
  integer             :: iproc(3),iproc2(3)
  integer             :: ipx2, ipy2, ipz2
  integer             :: iproc2y_x, iproc2y_z, ny2_x, last_y

  allocate(XB(nall))

  ! setup box F
  nx2 = nsize(1)/nproc(2)
  if (MOD(nsize(1),nproc(2)) /= 0) &
    nx2 = nx2 + 1
  ny2 = nsize(2)/nproc(1)
  if (MOD(nsize(2),nproc(1)) /= 0) &
    ny2 = ny2 + 1
  ny2_x = ny2
  if (MOD(ny2,nproc(3)) /= 0) then
    ny2 = ny2/nproc(3) + 1
  else
    ny2 = ny2/nproc(3)
  end if
  nz2 = nsize(3)
  nbox2 = nx2 * ny2 * nz2
  allocate(X0(nbox2))

  ! setup box X0
  nx = nsize(1)/nproc(1)
  ny = nsize(2)/nproc(2)
  nz = nsize(3)/nproc(3)
  if (MOD(nsize(1),nproc(1)) /= 0) &
    nx = nx + 1
  if (MOD(nsize(2),nproc(2)) /= 0) &
    ny = ny + 1
  if (MOD(nsize(3),nproc(3)) /= 0) &
    nz = nz + 1
  nbox = nx * ny * nz
  allocate(F(nbox))

  ! set input data
  iproc(1) = MOD(comm_rank, nproc(1))
  iproc(2) = MOD(comm_rank / nproc(1), nproc(2))
  iproc(3) = comm_rank / (nproc(1)*nproc(2))

  iproc2(1) = iproc(2)
  iproc2(2) = iproc(3) + iproc(1) * nproc(3)
  iproc2(3) = 0

  ipx2 = nx2 * iproc2(1)

  iproc2y_x = iproc2(2)/nproc(3)
  iproc2y_z = mod(iproc2(2),nproc(3))
  ipy2 = iproc2y_x * ny2_x
  last_y = 0
  do i = 1, iproc2y_z
    last_y = last_y + ny2
  end do
  if(last_y > ny2_x) last_y = ny2_x
  ipy2 = ipy2 + last_y    
  if(ipy2 > nsize(2)) ipy2 = nsize(2)

  ipz2 = 0

  blank = .false.

  do ix = 0, nx2-1
    do iy = 0, ny2-1
      do iz = 0, nz2-1
        if (ix+ipx2 < nsize(1) .and. &
            iy+ipy2 < nsize(2) .and. &
            iz+ipz2 < nsize(3) .and. &
            iy+last_y < ny2_x) then
          X0(iz + ix*nz2 + iy*nz2*nx2 + 1) = &
            XA((ix+ipx2) + (iy+ipy2)*nsize(1) + (iz+ipz2)*nsize(1)*nsize(2) + 1)
        else
          X0(iz + ix*nz2 + iy*nz2*nx2 + 1) = 0
          blank = .true.
        end if
      end do
    end do
  end do

#if 0
  if (blank) &
    write(6,*) 'Bench_Km_FFT3D> inserted blank data. rank:', comm_rank
#endif


  ! Run FFT
  !

  call KMATH_FFT3D_Transform(handle, X0, F, .true.)


  ! Check values
  !

  do i = 1, nall
    XA(i) = (0.d0, 0.d0)
  end do

  ipx = MOD(comm_rank, nproc(1))
  ipy = MOD(comm_rank / nproc(1), nproc(2))
  ipz = comm_rank / (nproc(1)*nproc(2))
  ipx = ipx * nx
  ipy = ipy * ny
  ipz = ipz * nz
  
  do ix = 0, nx-1
    do iy = 0, ny-1
      do iz = 0, nz-1
        if (ix+ipx < nsize(1) .and. &
            iy+ipy < nsize(2) .and. &
            iz+ipz < nsize(3)) then
          XA((ix+ipx) + (iy+ipy)*nsize(1) + (iz+ipz)*nsize(1)*nsize(2) + 1) = &
            F(ix + iy*nx + iz*nx*ny + 1)
        end if
      end do
    end do
  end do

  call MPI_Reduce(XA, XB, SIZE(XA), MPI_Double_complex, &
                  MPI_SUM, 0, MPI_COMM_WORLD, ierr)

  if (comm_rank == 0 .and. check == 1) then

    open(11,file='_out_fft3d',status='replace',form='formatted')
    do i = 1, nall
      write(11,*) XB(i)
    end do
    close(11)

    max_real = 0.d0
    max_imag = 0.d0

    open(11,file='_dif_fft3d_dft3d',status='replace',form='formatted')
    do i = 1, nall
      max_real = MAX(max_real, ABS(REAL(XB(i)-XA2(i))))
      max_imag = MAX(max_imag, ABS(IMAG(XB(i)-XA2(i))))
      write(11,*) XB(i)-XA2(i)
    end do
    write(11,*) '##Max Diff: Real:', max_real, ' Imag:', max_imag
    close(11)

  end if

  deallocate(XB,X0,F)

end subroutine bench_km_fft3d_i

! FORWARD->INVERSE program
subroutine bench_km_fft3d_fi(XA, nall, handle, comm_rank, nsize, nproc, check)
  use kmath_fft3d_mod
  use kmath_time_mod
  use kmath_msg_mod
  use mpi

  implicit none

  complex(kind(0d0)),  intent(inout) :: XA(nall)
  integer,          intent(in)    :: nall
  integer,          intent(in)    :: handle
  integer,          intent(in)    :: comm_rank
  integer,          intent(in)    :: nsize (3)
  integer,          intent(in)    :: nproc (3)
  integer,          intent(in)    :: check

  complex(kind(0d0)), allocatable :: XB(:),X0(:), F(:)
  double precision    :: max_real, max_imag
  integer             :: ierr
  integer             :: ix, iy, iz, nx, ny, nz, nbox
  integer             :: ipx, ipy, ipz

  logical             :: blank
  integer             :: i
  integer             :: nx2, ny2, nz2, nbox2
  integer             :: ny2_x

  complex(kind(0d0)), allocatable :: XA2(:)

  ! XA2 for check
  allocate(XA2(nall))

  allocate(XB(nall))

  ! setup box F
  nx2 = nsize(1)/nproc(2)
  if (MOD(nsize(1),nproc(2)) /= 0) &
    nx2 = nx2 + 1
  ny2 = nsize(2)/nproc(1)
  if (MOD(nsize(2),nproc(1)) /= 0) &
    ny2 = ny2 + 1
  ny2_x = ny2
  if (MOD(ny2,nproc(3)) /= 0) then
    ny2 = ny2/nproc(3) + 1
  else
    ny2 = ny2/nproc(3)
  end if
  nz2 = nsize(3)
  nbox2 = nx2 * ny2 * nz2
  allocate(F(nbox2))

  ! setup box X0
  nx = nsize(1)/nproc(1)
  ny = nsize(2)/nproc(2)
  nz = nsize(3)/nproc(3)
  if (MOD(nsize(1),nproc(1)) /= 0) &
    nx = nx + 1
  if (MOD(nsize(2),nproc(2)) /= 0) &
    ny = ny + 1
  if (MOD(nsize(3),nproc(3)) /= 0) &
    nz = nz + 1
  nbox = nx * ny * nz
  allocate(X0(nbox))

  ipx = MOD(comm_rank, nproc(1))
  ipy = MOD(comm_rank / nproc(1), nproc(2))
  ipz = comm_rank / (nproc(1)*nproc(2))
  ipx = ipx * nx
  ipy = ipy * ny
  ipz = ipz * nz
  
  blank = .false.

  do ix = 0, nx-1
    do iy = 0, ny-1
      do iz = 0, nz-1
        if (ix+ipx < nsize(1) .and. &
            iy+ipy < nsize(2) .and. &
            iz+ipz < nsize(3)) then
          X0(ix + iy*nx + iz*nx*ny + 1) = &
               XA((ix+ipx) + (iy+ipy)*nsize(1) + (iz+ipz)*nsize(1)*nsize(2) + 1)
        else
          X0(ix + iy*nx + iz*nx*ny + 1) = 0
          blank = .true.
        end if
      end do
    end do
  end do

#if 0
  if (blank) &
    write(6,*) 'Bench_Km_FFT3D> inserted blank data. rank:', comm_rank
#endif

  ! Run FFT
  !

  ! FORWARD
  call KMATH_FFT3D_Transform(handle, X0, F, .false.)
  ! INVERSE
  call KMATH_FFT3D_Transform(handle, F, X0, .true.)

  ! Check values
  !
  do i = 1, nall
    XA2(i) = (0.d0, 0.d0)
  end do

  ipx = MOD(comm_rank, nproc(1))
  ipy = MOD(comm_rank / nproc(1), nproc(2))
  ipz = comm_rank / (nproc(1)*nproc(2))
  ipx = ipx * nx
  ipy = ipy * ny
  ipz = ipz * nz
  
  do ix = 0, nx-1
    do iy = 0, ny-1
      do iz = 0, nz-1
        if (ix+ipx < nsize(1) .and. &
            iy+ipy < nsize(2) .and. &
            iz+ipz < nsize(3)) then
          XA2((ix+ipx) + (iy+ipy)*nsize(1) + (iz+ipz)*nsize(1)*nsize(2) + 1) = &
            X0(ix + iy*nx + iz*nx*ny + 1)
        end if
      end do
    end do
  end do

  call MPI_Reduce(XA2, XB, SIZE(XA2), MPI_Double_complex, &
                  MPI_SUM, 0, MPI_COMM_WORLD, ierr)

  if (comm_rank == 0 .and. check == 1) then

    max_real = 0.d0
    max_imag = 0.d0

#if 0
    open(11,file='_out_fft3d',status='replace',form='formatted')
    do i = 1, nall
      write(11,*) XB(i)
    end do
    close(11)
#endif

#if 0
    open(11,file='_dif_fft3d_dft3d',status='replace',form='formatted')
#endif
    do i = 1, nall
      max_real = MAX(max_real, ABS(REAL(XB(i)-XA(i))))
      max_imag = MAX(max_imag, ABS(IMAG(XB(i)-XA(i))))
#if 0
      write(11,*) XB(i)-XA(i)
#endif
    end do
#if 0
    write(11,*) '##Max Diff: Real:', max_real, ' Imag:', max_imag
    close(11)
#else
    write(6,*) '##Max Diff: Real:', max_real, ' Imag:', max_imag
#endif
    
  end if

  deallocate(XA2,XB,X0,F)

end subroutine bench_km_fft3d_fi

! set random data
subroutine bench_km_fft3d_setdata(XA, nall)
  implicit none

  complex(kind(0d0)),  intent(out) :: XA(nall)
  integer,          intent(in)    :: nall
  !
  integer             :: i
  double precision    :: r1, r2

  do i = 1,nall
      call random_number(r1)
      call random_number(r2)
      XA(i) = DCMPLX(r1, r2)
  end do

end subroutine bench_km_fft3d_setdata

! FFT for check
subroutine bench_km_dft3d(XA, XA2, nall, nsize, inverse)
  use KMATH_DFT_mod

  implicit none

  complex(kind(0d0)),  intent(in)  :: XA(nall)
  complex(kind(0d0)),  intent(out) :: XA2(nall)
  integer,          intent(in)     :: nall
  integer,          intent(in)     :: nsize(3)
  integer,          intent(in)     :: inverse
  !
  integer                          :: opt

  complex(kind(0d0)), allocatable :: X(:,:,:)
  complex(kind(0d0)), allocatable :: B(:,:,:)
  integer  :: L, M, N, ll, mm, nn

  allocate(X(1:nsize(1),1:nsize(2),1:nsize(3)))
  allocate(B(1:nsize(1),1:nsize(2),1:nsize(3)))

  opt = inverse
  if(opt /= 1) &
    opt = -1

  L = nsize(1)
  M = nsize(2)
  N = nsize(3)

  do nn = 0,N-1
    do mm = 0,M-1
      do ll = 0,L-1
        X(ll+1,mm+1,nn+1) = XA(ll + mm * L + nn * L * M + 1)
      end do
    end do
  end do

  do nn = 1, N
    do mm = 1, M
      call KMATH_DFT(X(1:L,mm,nn), B(1:L,mm,nn),L,opt)
    end do
  end do

  do nn = 1, N
    do ll = 1, L
      call KMATH_DFT(X(ll,1:M,nn), B(ll,1:M,nn),M,opt)
    end do
  end do

  do mm = 1, M
    do ll = 1, L
      call KMATH_DFT(X(ll,mm,1:N), B(ll,mm,1:N),N,opt)
    end do
  end do

  do nn = 0,N-1
    do mm = 0,M-1
      do ll = 0,L-1
        XA2(ll + mm * L + nn * L * M + 1) = B(ll+1,mm+1,nn+1)
      end do
    end do
  end do

  deallocate(X,B)

  return

end subroutine bench_km_dft3d

