
!--------1---------2---------3---------4---------5---------6---------7---------8
!
!  Module   kmath_prop_mod
!> @brief   Property module
!! @authors Toshiyuki Imamura (TI)
!! @date    2013/01/29 (NT)
!
!  (c) Copyright 2013 RIKEN. All rights reserved.
!
!--------1---------2---------3---------4---------5---------6---------7---------8

module kmath_prop_mod

  use kmath_msg_mod

  implicit none
  private


  ! public procedures
  !
  public :: KMATH_Prop_Put
  public :: KMATH_Prop_Get


  ! global variables
  !
  type s_keyval
    
    character(100)          :: key
    character(1000)         :: val
    type(s_keyval), pointer :: next

  end type s_keyval

  type s_keyval_hdr
    
    type(s_keyval), pointer :: kv => null()

  end type s_keyval_hdr

  type(s_keyval_hdr), allocatable, target :: g_keyvals(:)


  ! constants
  !
  integer,  parameter :: HashTableSize = 10007

contains

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  KMATH_Prop_Put
  !> @brief      Put the property value on memory
  !! @authors    NT
  !! @param[in]  key     property key
  !! @param[in]  val     property value
  !! @date       2013/01/29 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine KMATH_Prop_Put(key, val)

    ! formal arguments
    character(*),   intent(in)    :: key
    character(*),   intent(in)    :: val

    ! local variables
    type(s_keyval), pointer       :: kv, kvp
    integer                       :: hash, ierr


    if (.not. allocated(g_keyvals)) &
      call kp_init

    hash = kp_get_hash(key)

    kv => g_keyvals(hash)%kv
    
    if (.not. associated(kv)) then

      allocate(kv, stat=ierr)
      if (ierr /= 0) &
        goto 100
      g_keyvals(hash)%kv => kv
      
      kv%key = key
      kv%val = val
      kv%next => null()

    else

      do while(associated(kv)) 

        if (kv%key == key) then
          kv%val = val
          return

        else
          kvp => kv
          kv => kv%next

        end if
          
      end do

      allocate(kv, stat=ierr)
      if (ierr /= 0) &
        goto 100
      kv%key = key
      kv%val = val
      kv%next => null()
      kvp => kv

    end if
    
    
    return

100 call KMATH_Msg_Err('KMATH_Prop_Put> Memory allocation error.', ierr)


  end subroutine KMATH_Prop_Put


  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  KMATH_Prop_Get
  !> @brief      Get the property value from memory
  !! @authors    NT
  !! @param[in]  key     property key
  !! @param[out] val     property value
  !! @param[out] ofound  property was found or not (optional)
  !! @date       2013/01/29 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine KMATH_Prop_Get(key, val, ofound)

    ! formal arguments
    character(*),            intent(in)    :: key
    character(*),            intent(out)   :: val
    logical,       optional, intent(out)   :: ofound

    ! local variables
    type(s_keyval), pointer  :: kv
    integer                  :: hash


    if (.not. allocated(g_keyvals)) then
      if (present(ofound)) ofound = .false.
      return
    end if

    
    hash = kp_get_hash(key)

    kv => g_keyvals(hash)%kv
    
    if (.not. associated(kv)) then

      if (present(ofound)) ofound = .false.
      return

    else

      do while(associated(kv)) 

        if (kv%key == key) then
          val = kv%val
          if (present(ofound)) ofound = .true.
          return

        else
          kv => kv%next

        end if

      end do

    end if

    return

  end subroutine KMATH_Prop_Get


  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  kp_init
  !> @brief      initialize internal stats
  !! @authors    NT
  !! @date       2013/01/29 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine kp_init

    ! local variables
    integer :: ierr


    allocate(g_keyvals(HashTableSize), stat=ierr)
    if (ierr /= 0) &
      call KMATH_Msg_Err('Kp_Init> Memory allocation error.', ierr)

    return

  end subroutine kp_init


  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Function    kp_get_hash
  !> @brief      get the hash value
  !! @authors    NT
  !! @date       2013/01/29 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  function kp_get_hash(key)

    ! return value
    integer :: kp_get_hash

    ! formal arguments
    character(*), intent(in) :: key

    ! local variables
    integer       :: i, l


    kp_get_hash = 0

    l = LEN_TRIM(key)
    do i = 1, 1
      kp_get_hash = kp_get_hash + ICHAR(key(i:i))
    end do

    kp_get_hash = MOD(kp_get_hash, HashTableSize)

    return

  end function kp_get_hash


end module kmath_prop_mod
