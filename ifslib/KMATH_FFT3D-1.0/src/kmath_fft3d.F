
!--------1---------2---------3---------4---------5---------6---------7---------8
!
!  Module   kmath_fft3d_mod
!> @brief   3D fft module
!! @authors Toshiyuki Imamura (TI)
!! @date    2013/01/21 (NT)
!
!  (c) Copyright 2013 RIKEN. All rights reserved.
!
!--------1---------2---------3---------4---------5---------6---------7---------8

module kmath_fft3d_mod

  use kmath_transpose_mod
  use kmath_time_mod
  use kmath_msg_mod
  use omp_lib
  use mpi

  implicit none
  private


  ! public procedures
  !
  public :: KMATH_FFT3D_Init
  public :: KMATH_FFT3D_Finalize
  public :: KMATH_FFT3D_Transform


  ! constants
  !

  integer, parameter    :: MaxContexts = 100

  integer, parameter    :: X_ = 1
  integer, parameter    :: Y_ = 2
  integer, parameter    :: Z_ = 3

  integer, parameter    :: TNum = 5  ! Transpose number of times

  ! global variables
  !

  type s_context 
    logical             :: in_use = .false.
    integer             :: handle
    integer             :: comm
    integer             :: num_proc  (3)
    integer             :: box_size  (3)
    integer             :: num_proc_d(TNum)
    integer             :: lbox_size (TNum,3)
    integer             :: page_size (TNum)
    integer             :: block_size(TNum)
    integer             :: comm_axis (TNum)
    integer             :: transpose (TNum)
    logical             :: inverse
    complex(kind(0d0)), pointer :: B1(:)
    complex(kind(0d0)), pointer :: B2(:)
    complex(kind(0d0)), pointer :: B3(:)

  end type s_context

  type s_omp_buf
    complex(kind(0d0)), pointer :: X (:)
    complex(kind(0d0)), pointer :: B (:)
    integer                     :: n = 0
  end type s_omp_buf

  type(s_context),     save, target :: g_contexts(MaxContexts)
  type(s_omp_buf),     save, target :: g_omp_bufs(MaxContexts)

  !$omp threadprivate ( g_omp_bufs )

contains

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  KMATH_FFT3D_Init
  !> @brief      Initialize FFT 3D system
  !! @authors    NT
  !! @param[out] handle    handle for this FFT calculation
  !! @param[in]  comm      communicator
  !! @param[in]  box_size  box data size for each axies
  !! @param[in]  num_proc  number of process for each axies
  !! @param[in]  num_stage number of multi-stage transpose for each axies
  !! @date       2013/01/21 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine KMATH_FFT3D_Init(handle, comm, box_size, num_proc, num_stage)

    ! formal arguments
    integer,          intent(inout) :: handle
    integer,          intent(in)    :: comm
    integer,          intent(in)    :: box_size (3)
    integer,          intent(in)    :: num_proc (3)
    integer,          intent(in)    :: num_stage(3)

    ! local variables
    type(s_context),  pointer :: c
    type(s_omp_buf),  pointer :: b
    integer                   :: comm_size, comm_rank, n, ierr
    integer                   :: iproc(3), color(TNum)

    call KMATH_Time_Begin(1, 'KMATH_FFT3D_Init')

    ! search free handle value
    !
    do handle = 1, MaxContexts
      if (.not. g_contexts(handle)%in_use) &
        exit
    end do

    if (handle == MaxContexts) &
      call KMATH_Msg_Err( &
      'FFT3D_Init> Context handle reached upper limits.', '')


    c => g_contexts(handle)

    ! duplicate main communicator
    !
    call MPI_Comm_dup(comm, c%comm, ierr)
    if (ierr /= 0) &
      call KMATH_Msg_Err( &
      'FFT3D_Init> Duplicate MPI communicator was failed.', ierr)


    ! check the values
    !
    call MPI_Comm_size(c%comm, comm_size, ierr)
    call MPI_Comm_rank(c%comm, comm_rank, ierr)

    ! pass main rank info. to KMATH Msg module
    g_km_main = comm_rank == 0

    if (num_proc(X_)*num_proc(Y_)*num_proc(Z_) /= comm_size) &
      call KMATH_Msg_Err( &
      'FFT3D_Init> # of process is not nproc x*y*z. ', comm_size)

    if (MOD(box_size(X_), (2**1)) /= 0 .and. &
        MOD(box_size(X_), (3**1)) /= 0 .and. &
        MOD(box_size(X_), (5**1)) /= 0 .or.  &
        MOD(box_size(Y_), (2**1)) /= 0 .and. &
        MOD(box_size(Y_), (3**1)) /= 0 .and. &
        MOD(box_size(Y_), (5**1)) /= 0 .or.  &
        MOD(box_size(Z_), (2**1)) /= 0 .and. &
        MOD(box_size(Z_), (3**1)) /= 0 .and. &
        MOD(box_size(Z_), (5**1)) /= 0) &
      call KMATH_Msg_Err( &
      'FFT3D_Init> BOX_SIZE must be (2**P) * (3**Q) * (5**R).(P,Q,R>=0)', '')


    ! setup context variables
    !

    c%box_size = box_size
    c%num_proc = num_proc

    c%lbox_size (1,X_) = INT(CEILING(REAL(box_size(X_)) / REAL(num_proc(X_))))
    c%lbox_size (1,Y_) = INT(CEILING(REAL(box_size(Y_)) / REAL(num_proc(Y_))))
    c%lbox_size (1,Z_) = INT(CEILING(REAL(box_size(Z_)) / REAL(num_proc(Z_))))
    c%lbox_size (2,X_) = box_size(X_)
    c%lbox_size (2,Y_) = INT(CEILING(REAL(box_size(Y_)) / REAL(num_proc(Y_))))
    c%lbox_size (2,Z_) = INT(CEILING(REAL(box_size(Z_)) / REAL(num_proc(Z_))))
    c%lbox_size (2,Z_) = INT(CEILING(REAL(c%lbox_size(2,Z_)) / REAL(num_proc(X_))))
    c%lbox_size (3,X_) = INT(CEILING(REAL(box_size(X_)) / REAL(num_proc(Y_))))
    c%lbox_size (3,Y_) = box_size(Y_)
    c%lbox_size (3,Z_) = INT(CEILING(REAL(box_size(Z_)) / REAL(num_proc(Z_))))
    c%lbox_size (3,Z_) = INT(CEILING(REAL(c%lbox_size(3,Z_)) / REAL(num_proc(X_))))
    c%lbox_size (4,X_) = INT(CEILING(REAL(box_size(X_)) / REAL(num_proc(Y_))))
    c%lbox_size (4,Y_) = INT(CEILING(REAL(box_size(Y_)) / REAL(num_proc(X_))))
    c%lbox_size (4,Z_) = INT(CEILING(REAL(box_size(Z_)) / REAL(num_proc(Z_))))
    c%lbox_size (5,X_) = INT(CEILING(REAL(box_size(X_)) / REAL(num_proc(Y_))))
    c%lbox_size (5,Y_) = INT(CEILING(REAL(box_size(Y_)) / REAL(num_proc(X_))))
    c%lbox_size (5,Y_) = INT(CEILING(REAL(c%lbox_size (5,Y_)) / REAL(num_proc(Z_))))
    c%lbox_size (5,Z_) = box_size(Z_)
    
    c%page_size (1) = c%lbox_size(1,X_) * c%lbox_size(1,Y_)
    c%page_size (2) = c%lbox_size(2,Y_) * c%lbox_size(2,Z_)
    c%page_size (3) = c%lbox_size(3,Z_) * c%lbox_size(3,X_)
    c%page_size (4) = c%lbox_size(4,Z_) * c%lbox_size(4,X_)

    c%block_size(1) = INT(CEILING(REAL(c%lbox_size(1,Z_))/ REAL(c%num_proc(X_))))
    c%block_size(2) = INT(CEILING(REAL(c%lbox_size(2,X_))/ REAL(c%num_proc(Y_))))
    c%block_size(3) = INT(CEILING(REAL(c%lbox_size(3,Y_))/ REAL(c%num_proc(X_))))
    c%block_size(4) = INT(CEILING(REAL(c%lbox_size(4,Y_))/ REAL(c%num_proc(Z_))))

    c%block_size(1) = c%block_size(1) * c%page_size(1)
    c%block_size(2) = c%block_size(2) * c%page_size(2)
    c%block_size(3) = c%block_size(3) * c%page_size(3)
    c%block_size(4) = c%block_size(4) * c%page_size(4)

    c%num_proc_d(1) = c%num_proc(X_)
    c%num_proc_d(2) = c%num_proc(Y_)
    c%num_proc_d(3) = c%num_proc(X_)
    c%num_proc_d(4) = c%num_proc(Z_)

    n =        c%block_size(1) * c%num_proc(X_)
    n = MAX(n, c%block_size(2) * c%num_proc(Y_))
    n = MAX(n, c%block_size(3) * c%num_proc(X_))
    n = MAX(n, c%block_size(4) * c%num_proc(Z_))
    n = MAX(n, c%lbox_size(5,X_) * c%lbox_size(5,Y_) * c%lbox_size(5,Z_))

    allocate(c%B1(n), c%B2(n), c%B3(n), stat = ierr)

    if (ierr /= 0) &
      call KMATH_Msg_Err('FFT3D_Init> Memory allocation error.', ierr)

    !$omp parallel private ( b, n, ierr )

    b => g_omp_bufs(handle)

    n =     c%lbox_size(1,X_)*c%num_proc(X_)
    n = MAX(c%lbox_size(1,Y_)*c%num_proc(Y_), n)
    n = MAX(c%lbox_size(1,Z_)*c%num_proc(Z_), n)
    n = MAX(c%lbox_size(2,X_), n)
    n = MAX(c%lbox_size(2,Y_)*c%num_proc(Y_), n)
    n = MAX(c%lbox_size(2,Z_)*c%num_proc(Z_)*c%num_proc(X_), n)
    n = MAX(c%lbox_size(3,X_)*c%num_proc(Y_), n)
    n = MAX(c%lbox_size(3,Y_), n)
    n = MAX(c%lbox_size(3,Z_)*c%num_proc(Z_)*c%num_proc(X_), n)
    n = MAX(c%lbox_size(4,X_)*c%num_proc(Y_), n)
    n = MAX(c%lbox_size(4,Y_)*c%num_proc(X_), n)
    n = MAX(c%lbox_size(4,Z_)*c%num_proc(Z_), n)

    ! +70 of b%B for SSL2
#if defined(USESSL2)
    allocate(b%X(n), b%B(n*2+70), stat = ierr)
#else
    allocate(b%X(n), b%B(n*2), stat = ierr)
#endif
    if (ierr /= 0) &
      call KMATH_Msg_Err('FFT3D_Init> Memory allocation error.', ierr)

    !$omp end parallel


    ! initialize multi-stage transpose system
    !

    iproc(X_) = MOD(comm_rank, num_proc(X_))
    iproc(Y_) = MOD(comm_rank / num_proc(X_), num_proc(Y_))
    iproc(Z_) = comm_rank / (num_proc(X_)*num_proc(Y_))
    
    color(1) = iproc(Y_) + iproc(Z_) * c%num_proc(Y_)
    color(2) = iproc(X_) + iproc(Z_) * c%num_proc(X_)
    color(3) = iproc(Y_) + iproc(Z_) * c%num_proc(Y_)
    color(4) = iproc(X_) + iproc(Y_) * c%num_proc(X_)

    call MPI_Comm_split(c%comm, color(1), comm_rank, c%comm_axis(1), ierr)
    call MPI_Comm_split(c%comm, color(2), comm_rank, c%comm_axis(2), ierr)
    call MPI_Comm_split(c%comm, color(3), comm_rank, c%comm_axis(3), ierr)
    call MPI_Comm_split(c%comm, color(4), comm_rank, c%comm_axis(4), ierr)


    call KMATH_Transpose_Init(c%transpose (1), &
                              c%comm_axis (1), &
                              num_stage   (X_), &
                              c%block_size(1))

    call KMATH_Transpose_Init(c%transpose (2), &
                              c%comm_axis (2), &
                              num_stage   (Y_), &
                              c%block_size(2))

    call KMATH_Transpose_Init(c%transpose (3), &
                              c%comm_axis (3), &
                              num_stage   (X_), &
                              c%block_size(3))

    call KMATH_Transpose_Init(c%transpose (4), &
                              c%comm_axis (4), &
                              num_stage   (Z_), &
                              c%block_size(4))

    c%in_use = .true.
    c%handle = handle

    call KMATH_Msg_Out('FFT3D_Init>     Succeeded : handle', handle)

    call KMATH_Time_End(1)

    return

  end subroutine KMATH_FFT3D_Init


  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  KMATH_FFT3D_Finalize
  !> @brief      Finalize FFT 3D system
  !! @authors    NT
  !! @param[in]  handle   handle for this FFT calculation
  !! @date       2013/01/21 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine KMATH_FFT3D_Finalize(handle)

    ! formal arguments
    integer,          intent(in) :: handle

    ! local variables
    integer           :: ierr


    call KMATH_Time_Begin(2, 'KMATH_FFT3D_Finalize')


    call kf_check_handle(handle, 'KMATH_FFT3D_Finalize')
    
    ! Transpose Finalize
    !
    call KMATH_Transpose_Finalize(g_contexts(handle)%transpose(1))
    call KMATH_Transpose_Finalize(g_contexts(handle)%transpose(2))
    call KMATH_Transpose_Finalize(g_contexts(handle)%transpose(3))
    call KMATH_Transpose_Finalize(g_contexts(handle)%transpose(4))

    !$omp parallel

    deallocate(g_omp_bufs(handle)%B, g_omp_bufs(handle)%X)
    ! bug fix '14/3
    !   next initialization needed
    g_omp_bufs(handle)%n = 0

    !$omp end parallel

    g_contexts(handle)%in_use = .false.

    deallocate(g_contexts(handle)%B1, &
               g_contexts(handle)%B2, &
               g_contexts(handle)%B3)

    call MPI_Comm_free(g_contexts(handle)%comm_axis(1), ierr)
    call MPI_Comm_free(g_contexts(handle)%comm_axis(2), ierr)
    call MPI_Comm_free(g_contexts(handle)%comm_axis(3), ierr)
    call MPI_Comm_free(g_contexts(handle)%comm_axis(4), ierr)
    call MPI_Comm_free(g_contexts(handle)%comm,          ierr)

    call KMATH_Msg_Out('FFT3D_Finalize> Succeeded : handle', handle)

    call KMATH_Time_End(2)

    return

  end subroutine KMATH_FFT3D_Finalize

  subroutine KMATH_FFT3D_Transform(handle, X, F, inverse)
    implicit none

    ! formal arguments
    integer,                      intent(in)    :: handle
    complex(kind(0d0)),           intent(in)    :: X(:)
    complex(kind(0d0)),           intent(inout) :: F(:)
    logical,            optional, intent(in)    :: inverse
 
    logical :: inv

    if (present(inverse)) then
      inv = inverse
    else
      inv = .false.
    end if

    if(.not. inv) then
      call KMATH_FFT3D_Transform_F(handle, X, F)
    else
      call KMATH_FFT3D_Transform_I(handle, X, F)
    end if

  end subroutine

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  KMATH_FFT3D_Transform_F
  !> @brief      Execute 3D FFT (Forward)
  !! @authors    NT
  !! @param[in]  handle   handle for this FFT calculation
  !! @param[in]  X        input 3-dimentional data per node
  !! @param[out] F        output 3-dimentional data per node
  !! @param[in]  inverse  flag for inverse FFT
  !! @date       2013/01/21 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine KMATH_FFT3D_Transform_F(handle, X, F)
    implicit none

    ! formal arguments
    integer,                      intent(in)    :: handle
    complex(kind(0d0)),           intent(in)    :: X(:)
    complex(kind(0d0)),           intent(inout) :: F(:)

    ! local variables
    type(s_context),    pointer   :: c
    integer                       :: ix, iy, iz, wx, wy, wz

    integer                       :: iproc, num_proc, block_size

    call kf_check_handle(handle, 'KMATH_FFT3D_Transform')

    call KMATH_Time_Begin(3, 'KMATH_FFT3D_Transform')

    c => g_contexts(handle)

    c%inverse = .false.

    wx = c%lbox_size(1, X_)
    wy = c%lbox_size(1, Y_)
    wz = c%lbox_size(1, Z_)


    c%B1(1:wx*wy*wz) = X(1:wx*wy*wz)

    ! X-axis direction FFT
    !

    call kf_collect1d_fft1d_f(.true., 1, X_, c%B1, c)

    !direction change in this local matrix
    num_proc = c%num_proc(X_)
    wz = c%block_size(1) / c%page_size(1)
    block_size = c%block_size(1)

    !$omp parallel do private (iproc, iz, ix)
    do iy = 0, wy-1
      do iproc = 0, num_proc - 1
        do iz = 0, wz-1
          do ix = 0, wx-1
            if(iproc*wx+ix < c%lbox_size(2,X_)) then
              c%B2(iy+iz*wy+ix*wy*wz+iproc*block_size+1) = &
                c%B1(ix+iy*wx+iz*wx*wy+iproc*block_size+1)
            end if
          end do
        end do
      end do
    end do
    !$omp end parallel do

    call kf_collect1d_fft1d_f(.true., 2, Y_, c%B2, c) 

    !direction change in this local matrix
    wx = c%block_size(2) / c%page_size(2)
    wy = c%lbox_size(2, Y_)
    wz = c%lbox_size(2, Z_)

    num_proc = c%num_proc(Y_)
    block_size = c%block_size(2)

    !$omp parallel do private (iproc, iz, iy)
    do ix = 0, wx-1
      do iproc = 0, num_proc - 1
        do iz = 0, wz-1
          do iy = 0, wy-1
            if(iproc*wy+iy < c%lbox_size(3,Y_)) then
              c%B1(iz+ix*wz+iy*wz*wx+iproc*block_size+1) = &
                c%B2(iy+iz*wy+ix*wy*wz+iproc*block_size+1)
            end if
          end do
        end do
      end do
    end do
    !$omp end parallel do

    call kf_collect1d_fft1d_f(.false., 3, Z_, c%B1, c)

    !direction change in this local matrix
    wx = c%lbox_size(3, X_)
    wy = c%block_size(3) / c%page_size(3)
    wz = c%lbox_size(3, Z_)

    num_proc = c%num_proc(X_)
    block_size = c%block_size(3)

    !$omp parallel do private (iproc, ix, iz)
    do iy = 0, wy-1
      do iproc = 0, num_proc - 1
        do ix = 0, wx-1
          do iz = 0, wz-1
            if(iproc*wz+iz < c%lbox_size(4,Z_)) then
              c%B2(iz+iproc*wz+ix*c%lbox_size(4,Z_)+iy*c%lbox_size(4,Z_)*wx+1) = &
                c%B1(iz+ix*wz+iy*wz*wx+iproc*block_size+1)
            end if
          end do
        end do
      end do
    end do
    !$omp end parallel do

    call kf_collect1d_fft1d_f(.true., 4, Z_, c%B2, c)

    !direction change in this local matrix
    wx = c%lbox_size(4, X_)
    wy = c%block_size(4) / c%page_size(4)
    wz = c%lbox_size(4, Z_)

    num_proc = c%num_proc(Z_)
    block_size = c%block_size(4)

    !$omp parallel do private (iproc, ix, iz)
    do iy = 0, wy-1
      do iproc = 0, num_proc - 1
        do ix = 0, wx-1
          do iz = 0, wz-1
            if(iproc*wz+iz < c%lbox_size(5,Z_)) then
              F(iz+iproc*wz+ix*c%lbox_size(5,Z_)+iy*c%lbox_size(5,Z_)*wx+1) = &
                c%B2(iz+ix*wz+iy*wz*wx+iproc*block_size+1)
            end if
          end do
        end do
      end do
    end do
    !$omp end parallel do

    call KMATH_Time_End(3)

    return
  end subroutine KMATH_FFT3D_Transform_F

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  KMATH_FFT3D_Transform_I
  !> @brief      Execute 3D FFT (Inverse)
  !! @authors    NT
  !! @param[in]  handle   handle for this FFT calculation
  !! @param[in]  X        input 3-dimentional data per node
  !! @param[out] F        output 3-dimentional data per node
  !! @param[in]  inverse  flag for inverse FFT
  !! @date       2013/01/21 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine KMATH_FFT3D_Transform_I(handle, X, F)
    implicit none

    ! formal arguments
    integer,                      intent(in)    :: handle
    complex(kind(0d0)),           intent(in)    :: X(:)
    complex(kind(0d0)),           intent(inout) :: F(:)

    ! local variables
    type(s_context),    pointer   :: c
    integer                       :: ix, iy, iz, wx, wy, wz

    integer                       :: iproc, num_proc, block_size

    call kf_check_handle(handle, 'KMATH_FFT3D_Transform')

    call KMATH_Time_Begin(3, 'KMATH_FFT3D_Transform')

    c => g_contexts(handle)

    c%inverse = .true.

    wx = c%lbox_size(5, X_)
    wy = c%lbox_size(5, Y_)
    wz = c%lbox_size(5, Z_)

    c%B1(1:wx*wy*wz) = X(1:wx*wy*wz)

    !direction change in this local matrix
    wx = c%lbox_size(4, X_)
    wy = c%block_size(4) / c%page_size(4)
    wz = c%lbox_size(4, Z_)

    num_proc = c%num_proc(Z_)
    block_size = c%block_size(4)


    !$omp parallel do private (iproc, ix, iz)
    do iy = 0, wy-1
      do iproc = 0, num_proc - 1
        do ix = 0, wx-1
          do iz = 0, wz-1
            if(iproc*wz+iz < c%lbox_size(5,Z_)) then
              c%B2(iz+ix*wz+iy*wz*wx+iproc*block_size+1) = &
                c%B1(iz+iproc*wz+ix*c%lbox_size(5,Z_)+iy*c%lbox_size(5,Z_)*wx+1)
            end if
          end do
        end do
      end do
    end do
    !$omp end parallel do

    call kf_collect1d_fft1d_i(.true., 4, Z_, c%B2, c)

    !direction change in this local matrix
    wx = c%lbox_size(3, X_)
    wy = c%block_size(3) / c%page_size(3)
    wz = c%lbox_size(3, Z_)

    num_proc = c%num_proc(X_)
    block_size = c%block_size(3)

    !$omp parallel do private (iproc, ix, iz)
    do iy = 0, wy-1
      do iproc = 0, num_proc - 1
        do ix = 0, wx-1
          do iz = 0, wz-1
            if(iproc*wz+iz < c%lbox_size(4,Z_)) then
              c%B1(iz+ix*wz+iy*wz*wx+iproc*block_size+1) = &
                c%B2(iz+iproc*wz+ix*c%lbox_size(4,Z_)+iy*c%lbox_size(4,Z_)*wx+1)
            end if
          end do
        end do
      end do
    end do
    !$omp end parallel do

    call kf_collect1d_fft1d_i(.false., 3, Z_, c%B1, c)


    !direction change in this local matrix
    wx = c%block_size(2) / c%page_size(2)
    wy = c%lbox_size(2, Y_)
    wz = c%lbox_size(2, Z_)

    num_proc = c%num_proc(Y_)
    block_size = c%block_size(2)

    !$omp parallel do private (iproc, iz, iy)
    do ix = 0, wx-1
      do iproc = 0, num_proc - 1
        do iy = 0, wy-1
          do iz = 0, wz-1
            if(iproc*wy+iy < c%lbox_size(3,Y_)) then
              c%B2(iy+iz*wy+ix*wy*wz+iproc*block_size+1) = &
                c%B1(iz+ix*wz+iy*wz*wx+iproc*block_size+1)
            end if
          end do
        end do
      end do
    end do
    !$omp end parallel do

    call kf_collect1d_fft1d_i(.true., 2, Y_, c%B2, c) 


    !direction change in this local matrix
    wx = c%lbox_size(1, X_)
    wy = c%lbox_size(1, Y_)

    num_proc = c%num_proc(X_)
    wz = c%block_size(1) / c%page_size(1)
    block_size = c%block_size(1)

   !$omp parallel do private (iproc, iz, iy)
    do ix = 0, wx-1
      do iproc = 0, num_proc - 1
        do iz = 0, wz-1
          do iy = 0, wy-1
            if(iproc*wx+ix < c%lbox_size(2,X_)) then
              c%B1(ix+iy*wx+iz*wx*wy+iproc*block_size+1) = &
                c%B2(iy+iz*wy+ix*wy*wz+iproc*block_size+1)
            end if
          end do
        end do
      end do
    end do
    !$omp end parallel do

    call kf_collect1d_fft1d_i(.true., 1, X_, c%B1, c)

    wx = c%lbox_size(1, X_)
    wy = c%lbox_size(1, Y_)
    wz = c%lbox_size(1, Z_)

    F(1:wx*wy*wz) = c%B1(1:wx*wy*wz)

    call KMATH_Time_End(3)


    return
  end subroutine KMATH_FFT3D_Transform_I

  !===== Private ==2=========3=========4=========5=========6=========7=========8

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine    kf_collect1d_fft1d_f
  !> @brief        Collection 1D data and FFT 1D in sequential
  !! @authors      NT
  !!       [in]    tno transpose number
  !! @param[in]    iaxis target axis
  !jfe-tec
  !! @param[inout] B     input data
  !! @param[inout] c     context
  !! @date         2013/02/13 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine kf_collect1d_fft1d_f(fft_flag, tno, iaxis, B, c)

    ! formal arguments
    logical,            intent(in)    :: fft_flag
    integer,            intent(in)    :: tno
    integer,            intent(in)    :: iaxis
    complex(kind(0d0)), intent(inout) :: B(:)
    type(s_context),    intent(inout) :: c

    ! local variables
    integer             :: num_fft, num_proc, ifft, iproc
    integer             :: isrc0, isrc, idst, block_size, lbox_size

    ! Multi-stage transpose
    !
      
    call KMATH_Time_Begin(10, 'Multi-stage transpose')

    call KMATH_Transpose_Exec(c%transpose(tno), B, c%B3)

    ! FFT
    !

    call KMATH_Time_Next(11, 'FFT 1D (s)', 10)

    lbox_size  = c%lbox_size (tno, iaxis)
    block_size = c%block_size(tno)
    num_fft    = c%block_size(tno) / c%lbox_size(tno, iaxis)
    num_proc   = c%num_proc_d  (tno)

#if !defined(NOFFTOMP) && !defined(USEFFTW) 
    !$omp parallel do private ( isrc0, iproc, isrc, idst )
    !
#endif

    do ifft = 0, num_fft - 1

      isrc0 = ifft * lbox_size

      do iproc = 0, num_proc - 1

        isrc = isrc0 + iproc * block_size
        idst =         iproc * lbox_size

        g_omp_bufs(c%handle)%X(idst+1:idst+lbox_size) = &
             c%B3(isrc+1:isrc+lbox_size)

      end do

      if(fft_flag) then
#if !defined(USEFFTW) && !defined(USESSL2)
        call kf_zfft1d(iaxis, c)
#endif
#if defined(USEFFTW)
        call kf_zfft1d_fftw(iaxis, c)
#endif
#if defined(USESSL2)
        call kf_zfft1d_ssl2(iaxis, c)
#endif
      end if

      do iproc = 0, num_proc - 1

        isrc = isrc0 + iproc * block_size
        idst =         iproc * lbox_size

        B(isrc+1:isrc+lbox_size) = &
             g_omp_bufs(c%handle)%X(idst+1:idst+lbox_size)

      end do

!*****

    end do

#if !defined(NOFFTOMP) && !defined(USEFFTW) 
    !
    !$omp end parallel do
#endif


!*****

    ! Multi-stage transpose
    !

    call KMATH_Time_End(11)

    return

  end subroutine kf_collect1d_fft1d_f

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine    kf_collect1d_fft1d_i
  !> @brief        Collection 1D data and FFT 1D in sequential
  !! @authors      NT
  !!       [in]    tno transpose number
  !! @param[in]    iaxis target axis
  !! @param[inout] B     input data
  !! @param[inout] c     context
  !! @date         2013/02/13 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine kf_collect1d_fft1d_i(fft_flag, tno, iaxis, B, c)

    ! formal arguments
    logical,            intent(in)    :: fft_flag
    integer,            intent(in)    :: tno
    integer,            intent(in)    :: iaxis
    complex(kind(0d0)), intent(inout) :: B(:)
    type(s_context),    intent(inout) :: c

    ! local variables
    integer             :: num_fft, num_proc, ifft, iproc
    integer             :: isrc0, isrc, idst, block_size, lbox_size

    ! FFT
    !

    call KMATH_Time_Begin(11, 'FFT 1D (s)')

    lbox_size  = c%lbox_size (tno, iaxis)
    block_size = c%block_size(tno)
    num_fft    = c%block_size(tno) / c%lbox_size(tno, iaxis)
    num_proc   = c%num_proc_d  (tno)


#if !defined(NOFFTOMP) && !defined(USEFFTW) 
    !$omp parallel do private ( isrc0, iproc, isrc, idst )
    !
#endif

    do ifft = 0, num_fft - 1

      isrc0 = ifft * lbox_size

      do iproc = 0, num_proc - 1

        isrc = isrc0 + iproc * block_size
        idst =         iproc * lbox_size

        g_omp_bufs(c%handle)%X(idst+1:idst+lbox_size) = &
             B(isrc+1:isrc+lbox_size)

      end do

      if(fft_flag) then
#if !defined(USEFFTW) && !defined(USESSL2)
        call kf_zfft1d(iaxis, c)
#endif
#if defined(USEFFTW)
        call kf_zfft1d_fftw(iaxis, c)
#endif
#if defined(USESSL2)
        call kf_zfft1d_ssl2(iaxis, c)
#endif
      end if

      do iproc = 0, num_proc - 1

        isrc = isrc0 + iproc * block_size
        idst =         iproc * lbox_size

        c%B3(isrc+1:isrc+lbox_size) = &
             g_omp_bufs(c%handle)%X(idst+1:idst+lbox_size)

      end do

!*****

    end do

#if !defined(NOFFTOMP) && !defined(USEFFTW) 
    !
    !$omp end parallel do
#endif

!*****

    ! Multi-stage transpose
    !

    call KMATH_Time_Next(12, 'Multi-stage transpose (Reverse)', 11)

    call KMATH_Transpose_Exec(c%transpose(tno), c%B3, B)

    call KMATH_Time_End(12)

    return

  end subroutine kf_collect1d_fft1d_i

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine    kf_zfft1d
  !> @brief        execute ZFFT1D
  !! @authors      NT
  !! @param[in]    iaxis target axis
  !! @param[inout] A     input data
  !! @param[in]    c     context
  !! @date         2013/01/25 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

#if !defined(USEFFTW) && !defined(USESSL2)

  subroutine kf_zfft1d(iaxis, c)

    !!!!use kmath_dft_mod

    ! formal arguments
    integer,            intent(in)    :: iaxis
    type(s_context),    intent(in)    :: c

    ! local variables
    type(s_omp_buf),    pointer :: buf
    integer :: n, opt


    call KMATH_Time_Begin(30, 'FFT (zfft1d)')


    n = c%box_size(iaxis)
    
    if (.not. c%inverse) then
      opt = -1
    else
      opt = 1
    end if

    buf => g_omp_bufs(c%handle)

    if (n /= buf%n) then
      call ZFFT1D(buf%X, n, 0, buf%B)
      buf%n = n
    end if
    call ZFFT1D(buf%X, n, opt, buf%B)

    !!!!call KMATH_dft(buf%X, buf%B, n, opt)


    call KMATH_Time_End(30)

    return

  end subroutine kf_zfft1d

#endif

#if defined(USEFFTW)

  ! subroutine with fftw
  subroutine kf_zfft1d_fftw(iaxis, c)

    use, intrinsic :: iso_c_binding
    implicit none
    include 'fftw3.f03'

    ! formal arguments
    integer,            intent(in)    :: iaxis
    type(s_context),    intent(in)    :: c

    ! local variables
    type(s_omp_buf),    pointer :: buf
    integer :: n

    type(C_PTR) :: plan


    call KMATH_Time_Begin(30, 'FFT (zfft1d)')

    n = c%box_size(iaxis)
    
    buf => g_omp_bufs(c%handle)

    if (.not. c%inverse) then
      plan = fftw_plan_dft_1d(n, buf%X, buf%X, FFTW_FORWARD,FFTW_ESTIMATE)
    else
      plan = fftw_plan_dft_1d(n, buf%X, buf%X, FFTW_BACKWARD,FFTW_ESTIMATE)
    end if

    call fftw_execute_dft(plan, buf%X, buf%X)

    if (c%inverse) then
      buf%X = buf%X / dble(n)
    end if

    call fftw_destroy_plan(plan)

    call KMATH_Time_End(30)

    return

  end subroutine kf_zfft1d_fftw
  
#endif

#if defined(USESSL2)
  ! subroutine with ssl2
  subroutine kf_zfft1d_ssl2(iaxis, c)

    implicit none

    ! formal arguments
    integer,            intent(in)    :: iaxis
    type(s_context),    intent(in)    :: c

    ! local variables
    type(s_omp_buf),    pointer :: buf
    integer :: n, opt
    integer :: ICON
    integer :: isw

    call KMATH_Time_Begin(30, 'FFT (zfft1d)')

    n = c%box_size(iaxis)
    
    if (.not. c%inverse) then
      opt = 1
    else
      opt = -1
    end if

    buf => g_omp_bufs(c%handle)

    if (n /= buf%n) then
      isw = 1
      call DVCFM1(buf%X, n, isw, opt, buf%B, ICON)
      buf%n = n
    else
      isw = 0
      call DVCFM1(buf%X, n, isw, opt, buf%B, ICON)
    end if

    if (c%inverse) then
      buf%X = buf%X / dble(n)
    end if

    call KMATH_Time_End(30)

    return

  end subroutine kf_zfft1d_ssl2

#endif

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  kf_check_handle
  !> @brief      Check handle validation
  !! @authors    NT
  !! @param[in]  handle    handle
  !! @param[in]  caller    string means caller
  !! @date       2013/01/22 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine kf_check_handle(handle, caller)

    ! formal arguments
    integer,      intent(in)    :: handle
    character(*), intent(in)    :: caller


    if (handle <= 0 .or. handle > MaxContexts) &
      call KMATH_Msg_Err(caller//'> Invalid handle', handle)

    if (.not. g_contexts(handle)%in_use) &
      call KMATH_Msg_Err(caller//'> Invalid handle', handle)


    return

  end subroutine kf_check_handle


end module kmath_fft3d_mod

