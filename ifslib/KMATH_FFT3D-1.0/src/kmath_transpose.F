
!--------1---------2---------3---------4---------5---------6---------7---------8
!
!  Module   kmath_transpose_mod
!> @brief   1D multi-stage transpose module
!! @authors Toshiyuki Imamura (TI)
!! @date    2013/01/29 (NT)
!
!  (c) Copyright 2013 RIKEN. All rights reserved.
!
!--------1---------2---------3---------4---------5---------6---------7---------8

module kmath_transpose_mod

  use kmath_msg_mod
  use mpi

  implicit none
  private


  ! public procedures
  !
  public :: KMATH_Transpose_Init
  public :: KMATH_Transpose_Finalize
  public :: KMATH_Transpose_Exec


  ! constants
  !
  integer, parameter    :: MaxContexts = 300
  integer, parameter    :: MaxStages   = 20


  ! global variables
  !

  type s_context 

    integer             :: num_stage
    integer             :: page_size
    integer             :: num_proc
    integer             :: comm_stg (MaxStages)
    integer             :: trans_stg(MaxStages)
    logical             :: in_use = .false.

    complex(kind(0d0)), pointer :: B(:)

  end type s_context

  type(s_context),     save, target :: g_contexts(MaxContexts)

contains

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  KMATH_Transpose_Init
  !> @brief      Initialize 1D Multi-stage transpose system
  !! @authors    NT
  !! @param[out] handle    handle for this transposition
  !! @param[in]  comm      communicator of nodes in the axis
  !! @param[in]  num_stage number of multi-stage transpose
  !! @param[in]  page_size transpose page size
  !! @date       2013/01/21 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

   subroutine KMATH_Transpose_Init(handle, comm, num_stage, page_size)

    ! formal arguments
    integer,          intent(inout) :: handle
    integer,          intent(in)    :: comm
    integer,          intent(in)    :: num_stage
    integer,          intent(in)    :: page_size

    ! local variables
    type(s_context),  pointer :: c
    integer           :: i, j, ic, istage, nstep, ierr, rank, pfr(100)
    character(100)    :: str

    integer, allocatable :: color(:)


    ! search free handle value
    !
    do handle = 1, MaxContexts
      if (.not. g_contexts(handle)%in_use) &
        exit
    end do

    if (handle == MaxContexts) &
      call KMATH_Msg_Err( &
      'Transpose_Init> Context handle reached upper limits.', '')


    c => g_contexts(handle)
    c%num_stage = num_stage
    c%page_size = page_size

    call MPI_Comm_size(comm, c%num_proc, ierr)
    call MPI_Comm_rank(comm, rank,       ierr)

    ! compute multi-stage transpose size for each stage
    !
    call kt_prime_factor(c%num_proc, pfr)
    
    c%trans_stg(:) = 1
    do i = 1, 100
      if (pfr(i) == 0) &
        exit
      istage = MOD(i-1,c%num_stage) + 1
      c%trans_stg(istage) = c%trans_stg(istage) * pfr(i)
    end do

    if (c%num_stage > MaxStages) &
      call KMATH_Msg_Err('Transpose_Init> num_stage reached upper limit', &
                         MaxStages)

    if (c%num_stage > 1 .and. c%trans_stg(c%num_stage) == 1) &
      call KMATH_Msg_Err('Transpose_Init> There is 1x1 transpose size', '')

      
    ! split communicator by stage 
    !

    nstep = c%num_proc

    allocate(color(0:c%num_proc-1))

    do i = 1, c%num_stage

      nstep = nstep / c%trans_stg(i)

      ic = 0

      do j = 1, c%num_proc
        color(MOD(ic,c%num_proc)+ic/c%num_proc) = (j-1) / c%trans_stg(i)
        ic = ic + nstep
      end do
      
      call MPI_Comm_split(comm, color(rank), rank, c%comm_stg(i), ierr)

    end do

    deallocate(color)

         
    ! allocate working memory
    !

    allocate(c%B(c%page_size * c%num_proc), stat=ierr)
    if (ierr /= 0) &
      call KMATH_Msg_Err('Transpose_Init> Memory allocation error.', ierr)


    c%in_use = .true.

    call KMATH_Msg_Out('Transpose_Init> Succeeded : handle', handle)
    do i = 1, c%num_stage
      write(str,'(a,i2,a,i3)') &
                       '  Stage:', i, '  transpose size:', c%trans_stg(i)
      call KMATH_Msg_Out(str, '')
    end do

    return

  end subroutine KMATH_Transpose_Init

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  KMATH_Transpose_Finalize
  !> @brief      Finalize multi-stage transpose system
  !! @authors    NT
  !! @param[in]  handle   handle for this FFT calculation
  !! @date       2013/01/29 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine KMATH_Transpose_Finalize(handle)

    ! formal arguments
    integer,          intent(in) :: handle

    ! local variables
    type(s_context),  pointer    :: c
    integer                      :: i, ierr


    call kt_check_handle(handle, 'KMATH_Transpose_Finalize')
    
    c => g_contexts(handle)

    do i = 1, c%num_stage
      call MPI_Comm_Free(c%comm_stg(i), ierr)
    end do

    deallocate(c%B)

    c%in_use = .false.

    call KMATH_Msg_Out('Transpose_Finalize> Succeeded : handle', handle)

    return

  end subroutine KMATH_Transpose_Finalize


  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  KMATH_Transpose_Exec
  !> @brief      Execute multi-stage transpose
  !! @authors    NT
  !! @param[in]  handle   handle for this FFT calculation
  !! @param[in]  A        input buffer
  !! @param[in]  B        output buffer
  !! @date       2013/01/29 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine KMATH_Transpose_Exec(handle, A, B)

    ! formal arguments
    integer,            intent(in)    :: handle
    complex(kind(0d0)), intent(in)    :: A(:)
    complex(kind(0d0)), intent(inout) :: B(:)

    ! local variables
    type(s_context),    pointer :: c
    integer             :: ierr, i, j, jj, k, kk
    integer             :: num_stage, page_size, num_proc
    integer             :: num_group, trans_size, block_size


    call kt_check_handle(handle, 'KMATH_Transpose_Exec')

    c => g_contexts(handle)

    num_stage = c%num_stage
    page_size = c%page_size
    num_proc  = c%num_proc

    B(1:num_proc*page_size) = A(1:num_proc*page_size)

    do i = 1, num_stage

      trans_size = c%trans_stg(i)
      num_group  = num_proc / trans_size
      block_size = page_size * num_group

      ! transpose 
      call MPI_Alltoall(B,   block_size, MPI_Double_complex, &
                        c%B, block_size, MPI_Double_complex, &
                        c%comm_stg(i), ierr)

      ! sort
      do j = 0, num_proc-1
          
        jj = j*page_size

        k = MOD(j*num_group,num_proc) + j*num_group/num_proc
        kk = k*page_size

        B(jj+1:jj+page_size) = c%B(kk+1:kk+page_size)

      end do

    end do

    return

  end subroutine KMATH_Transpose_Exec


  !===== Private ==2=========3=========4=========5=========6=========7=========8

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  kt_prime_factor
  !> @brief      calculate prime factors
  !! @authors    NT
  !! @date       2013/01/28 (NT)
  !! @param[in]  value  target value
  !! @param[out] factors  prime factors
  !! @param[in]  odiv     divisor [optional]
  !! @param[in]  oifactor index of factors array [optional]
  !! @note       Huge prime value cause Stack Overflow.(Segmentation fault)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  recursive subroutine kt_prime_factor(value, factors, odiv, oifactor)

    ! formal arguments
    integer, intent(in)           :: value
    integer, intent(inout)        :: factors(:)
    integer, intent(in), optional :: odiv
    integer, intent(in), optional :: oifactor

    ! local variables
    integer  :: div, ifactor

    if (present(odiv)) then
      div = odiv
    else
      div = 2
    end if

    if (present(oifactor)) then
      ifactor = oifactor
    else
      ifactor = 0
      factors(:) = 0
    end if

    if (value == 0 .or. value == 1) then
      return

    else if (MOD(value, div) == 0) then

      ifactor = ifactor + 1
      if (SIZE(factors) < ifactor) &
        call KMATH_Msg_Err('Prime_Factor> Overflow', '')
      factors(ifactor) = div

      call kt_prime_factor(value / div, factors, div, ifactor)

    else
      call kt_prime_factor(value, factors, div + 1, ifactor)

    end if

    return

  end subroutine kt_prime_factor


  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  kt_check_handle
  !> @brief      Check handle validation
  !! @authors    NT
  !! @param[in]  handle    handle
  !! @param[in]  caller    caller
  !! @date       2013/01/22 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine kt_check_handle(handle, caller)

    ! formal arguments
    integer,      intent(in)    :: handle
    character(*), intent(in)    :: caller


    if (handle <= 0 .or. handle > MaxContexts) &
      call KMATH_Msg_Err(caller//'> Invalid handle', handle)

    if (.not. g_contexts(handle)%in_use) &
      call KMATH_Msg_Err(caller//'> Invalid handle', handle)


    return

  end subroutine kt_check_handle


end module kmath_transpose_mod
