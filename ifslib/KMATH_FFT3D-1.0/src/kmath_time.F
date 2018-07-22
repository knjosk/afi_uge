
!--------1---------2---------3---------4---------5---------6---------7---------8
!
!  Module   kmath_time_mod
!> @brief   Time measurement module
!! @authors Toshiyuki Imamura (TI)
!! @date    2013/01/24 (NT)
!
!  (c) Copyright 2013 RIKEN. All rights reserved.
!
!--------1---------2---------3---------4---------5---------6---------7---------8

module kmath_time_mod

  use kmath_msg_mod
!$  use omp_lib

  implicit none
  private


  ! public procedures
  !
  public :: KMATH_Time_Init
  public :: KMATH_Time_Finalize
  public :: KMATH_Time_Begin
  public :: KMATH_Time_End
  public :: KMATH_Time_Next


  ! constants
  !
  
  integer, public, parameter  :: KT_MaxID = 1000


  ! global variables
  !

  type s_time
    character(50)     :: name
    integer           :: istep
    integer*8         :: t1
    integer*8         :: t2
    integer           :: cnt
    double precision  :: v
  end type s_time

  logical             :: g_init = .false.

  type(s_time), save, target, allocatable :: g_times(:)

  !$omp threadprivate ( g_times )

contains

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  KMATH_Time_Init
  !> @brief      Initialize time measurement module
  !! @authors    NT
  !! @date       2013/01/24 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine KMATH_Time_Init

    ! local variables
    integer :: ierr


    if (g_init) &
      return


    !$omp parallel private ( ierr )
    !

    allocate(g_times(KT_MaxId), stat=ierr)
    if (ierr /= 0) &
      call KMATH_MSG_Err('Time_Init> Memory allocation error.', ierr)

    g_times(:)%name  = ''
    g_times(:)%istep = 0
    g_times(:)%t1    = 0
    g_times(:)%t2    = 0
    g_times(:)%cnt   = 0
    g_times(:)%v     = 0.d0

    !
    !$omp end parallel

    g_init = .true.

    return

  end subroutine KMATH_Time_Init


  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  KMATH_Time_Finalize
  !> @brief      Finalize time measurement module
  !! @authors    NT
  !! @date       2013/01/24 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine KMATH_Time_Finalize

    ! local variables
    integer        :: i, ierr
    character(200) :: str


    if (.not. g_init) &
      return

    call KMATH_MSG_Out('Time_Finalize> :', '')


    !$omp parallel private ( i, str, ierr )
    !


    !$omp critical

    ! output results
    !
    
!$    write(str, '(a,i3,a)')  'Thread ID  : ',    &
!$                            omp_get_thread_num(), &
!$                            '----------------------------------'
    call KMATH_MSG_Out(str, '')

    do i = 1, KT_MaxId
      if (g_times(i)%istep /= 2) &
        cycle

      write(str, '(a,a)')        '  Name       : ', &
           TRIM(g_times(i)%name)
      call KMATH_MSG_Out(str, '')

      write(str, '(a,f8.3,f8.3)')'  Time (avg.): ', &
           g_times(i)%v, &
           g_times(i)%v/DBLE(g_times(i)%cnt)
      call KMATH_MSG_Out(str, '')

      write(str, '(a,i8)')       '  Count      : ', &
           g_times(i)%cnt
      call KMATH_MSG_Out(str, '')
    end do

    !$omp end critical


    ! deallocate memory
    !
    deallocate(g_times, stat=ierr)
    if (ierr /= 0) &
      call KMATH_MSG_Err('Time_Finalize> Memory deallocation error.', ierr)


    !
    !$omp end parallel

    g_init = .false.

    return

  end subroutine KMATH_Time_Finalize


  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  KMATH_Time_Begin
  !> @brief      Begin time measurement of new id
  !! @authors    NT
  !! @param[in]  id    measurement id
  !! @param[in]  name  measurement name
  !! @date       2013/01/24 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine KMATH_Time_Begin(id, name)

    ! formal arguments
    integer,                intent(in) :: id
    character(*), optional, intent(in) :: name

    ! local variables
    type(s_time), pointer   :: t
    integer                 :: cnt, cnt_rate


    if (.not. g_init .or. id < 1 .or. id > KT_MaxId) &
      return

    call SYSTEM_CLOCK(cnt, cnt_rate)

    t => g_times(id)

    if (present(name)) &
      t%name  = name
    t%istep = 1
    t%t1 = cnt  * INT(1000000,kind=8)
    t%t1 = t%t1 / INT(cnt_rate,kind=8)

    return

  end subroutine KMATH_Time_Begin


  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  KMATH_Time_End
  !> @brief      End time measurement of ID
  !! @authors    NT
  !! @param[in]  id    measurement id
  !! @date       2013/01/24 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine KMATH_Time_End(id)

    ! formal arguments
    integer,     optional :: id

    ! local variables
    type(s_time), pointer :: t
    integer               :: idb, ide, i
    integer               :: cnt, cnt_rate, cnt_max


    if (.not. g_init) &
      return

    if (present(id)) then
      if (id < 1 .or. id > KT_MaxId) &
        return 
      idb = id
      ide = id
    else
      idb = 1
      ide = KT_MaxId
    end if

    call SYSTEM_CLOCK(cnt, cnt_rate, cnt_max)

    do i = idb, ide

      if (g_times(i)%istep /= 1) &
        cycle
      
      t => g_times(i)

      t%istep = 2
      t%t2  = cnt  * INT(1000000,kind=8)
      t%t2  = t%t2 / INT(cnt_rate,kind=8)
      t%cnt = t%cnt + 1
      t%v   = t%v + (t%t2 - t%t1) / DBLE(1000000)

    end do

    return

  end subroutine KMATH_Time_End


  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  KMATH_Time_Next
  !> @brief      End time measurement and Begin new measurement of new ID
  !! @authors    NT
  !! @param[in]  id     measurement id
  !! @param[in]  name   measurement name [optional]
  !! @param[in]  prv_id measurement id for finish [optional]
  !! @date       2013/01/24 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine KMATH_Time_Next(id, name, prv_id)

    ! formal arguments
    integer,                intent(in) :: id
    character(*), optional, intent(in) :: name
    integer,      optional, intent(in) :: prv_id


    call KMATH_Time_End(prv_id)
    call KMATH_Time_Begin(id, name)

    return

  end subroutine KMATH_Time_Next

end module kmath_time_mod
