
!--------1---------2---------3---------4---------5---------6---------7---------8
!
!  Module   kmath_msg_mod
!> @brief   Message output, error output module
!! @authors Toshiyuki Imamura (TI)
!! @date    2013/01/21 (NT)
!
!  (c) Copyright 2013 RIKEN. All rights reserved.
!
!--------1---------2---------3---------4---------5---------6---------7---------8

module kmath_msg_mod

  implicit none
  private


  ! public procedures
  !
  public :: KMATH_Msg_Out
  public :: KMATH_Msg_Err


  ! interfaces
  !
  interface KMATH_Msg_Out
    module procedure \
    km_out_i, \
    km_out_r, \
    km_out_c, \
    km_out_s
  end interface

  interface KMATH_Msg_Err
    module procedure \
    km_err_i, \
    km_err_r, \
    km_err_c, \
    km_err_s
  end interface


  ! global variables
  !
  logical, public    :: g_km_main = .true.


  ! parameters
  !
  integer, parameter :: StdOut = 6
  integer, parameter :: StdErr = 7

contains

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  km_out_i
  !> @brief      Output message with integer value 
  !! @authors    NT
  !! @param[in]  msg      message string
  !! @param[in]  val      additional information
  !! @date       2013/01/22 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine km_out_i(msg, val)

    ! formal arguments
    character(*),  intent(in)    :: msg
    integer,       intent(in)    :: val

    ! local variables
    character(1000) :: str

    
    write(str,*) TRIM(msg), ": ", val
    call km_out__(str)

    return

  end subroutine km_out_i


  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  km_out_r
  !> @brief      Output message with real value 
  !! @authors    NT
  !! @param[in]  msg      message string
  !! @param[in]  val      additional information
  !! @date       2013/01/22 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine km_out_r(msg, val)

    ! formal arguments
    character(*),     intent(in)    :: msg
    double precision, intent(in)    :: val

    ! local variables
    character(1000) :: str

    
    write(str,*) TRIM(msg), ": ", val
    call km_out__(str)

    return

  end subroutine km_out_r


  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  km_out_c
  !> @brief      Output message with complex value 
  !! @authors    NT
  !! @param[in]  msg      message string
  !! @param[in]  val      additional information
  !! @date       2013/01/22 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine km_out_c(msg, val)

    ! formal arguments
    character(*),       intent(in)    :: msg
    complex(kind(0d0)), intent(in)    :: val

    ! local variables
    character(1000) :: str

    
    write(str,*) TRIM(msg), ": ", val
    call km_out__(str)

    return

  end subroutine km_out_c


  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  km_out_s
  !> @brief      Output message with string value 
  !! @authors    NT
  !! @param[in]  msg      message string
  !! @param[in]  val      additional information
  !! @date       2013/01/22 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine km_out_s(msg, val)

    ! formal arguments
    character(*), intent(in)    :: msg
    character(*), intent(in)    :: val

    ! local variables
    character(1000) :: str

    
    if (LEN_TRIM(val) > 0) then
      write(str,*) TRIM(msg), ": ", TRIM(val)
    else
      write(str,*) TRIM(msg)
    end if
    call km_out__(str)

    return

  end subroutine km_out_s


  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  km_err_i
  !> @brief      Output error message with integer value 
  !! @authors    NT
  !! @param[in]  msg      message string
  !! @param[in]  val      additional information
  !! @date       2013/01/22 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine km_err_i(msg, val)

    ! formal arguments
    character(*),  intent(in)    :: msg
    integer,       intent(in)    :: val

    ! local variables
    character(1000) :: str

    
    write(str,*) TRIM(msg), ": ", val
    call km_err__(str)

    return

  end subroutine km_err_i


  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  km_err_r
  !> @brief      Output error message with real value 
  !! @authors    NT
  !! @param[in]  msg      message string
  !! @param[in]  val      additional information
  !! @date       2013/01/22 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine km_err_r(msg, val)

    ! formal arguments
    character(*),     intent(in)    :: msg
    double precision, intent(in)    :: val

    ! local variables
    character(1000) :: str

    
    write(str,*) TRIM(msg), ": ", val
    call km_err__(str)

    return

  end subroutine km_err_r


  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  km_err_c
  !> @brief      Output error message with complex value 
  !! @authors    NT
  !! @param[in]  msg      message string
  !! @param[in]  val      additional information
  !! @date       2013/01/22 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine km_err_c(msg, val)

    ! formal arguments
    character(*),       intent(in)    :: msg
    complex(kind(0d0)), intent(in)    :: val

    ! local variables
    character(1000) :: str

    
    write(str,*) TRIM(msg), ": ", val
    call km_err__(str)

    return

  end subroutine km_err_c


  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  km_err_s
  !> @brief      Output error message with string value 
  !! @authors    NT
  !! @param[in]  msg      message string
  !! @param[in]  val      additional information
  !! @date       2013/01/22 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine km_err_s(msg, val)

    ! formal arguments
    character(*), intent(in)    :: msg
    character(*), intent(in)    :: val

    ! local variables
    character(1000) :: str

    
    if (LEN_TRIM(val) > 0) then
      write(str,*) TRIM(msg), ": ", TRIM(val)
    else
      write(str,*) TRIM(msg)
    end if
    call km_err__(str)

    return

  end subroutine km_err_s


  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  km_out__
  !> @brief      Output message
  !! @authors    NT
  !! @param[in]  msg      message string
  !! @date       2013/01/22 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine km_out__(msg)

    ! formal arguments
    character(*), intent(in)    :: msg


#ifdef DEBUG
    if (g_km_main) &
      write(StdOut,*) 'KMATH Msg: '//TRIM(msg)
#endif

  end subroutine km_out__


  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  km_err__
  !> @brief      Output error message
  !! @authors    NT
  !! @param[in]  msg      message string
  !! @date       2013/01/22 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine km_err__(msg)

#ifdef __INTEL_COMPILER
    use IFCORE
#endif

    ! formal arguments
    character(*), intent(in)    :: msg

    ! local variables
    integer                     :: ierr


    if (g_km_main) then
#ifdef __INTEL_COMPILER
      call TracebackQQ(TRIM(msg), -1)
#else
      write(StdOut,*) 'KMATH ERR: '//TRIM(msg)
      write(StdOut,*) ''
#endif
    end if

    call mpi_finalize(ierr)
    stop

  end subroutine km_err__

end module kmath_msg_mod
