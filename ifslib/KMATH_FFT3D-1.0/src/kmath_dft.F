
!--------1---------2---------3---------4---------5---------6---------7---------8
!
!  Module   kmath_dft_mod
!> @brief   Discrete Fourier Transform module
!! @authors Toshiyuki Imamura (TI)
!! @date    2013/01/31 (NT)
!
!  (c) Copyright 2013 RIKEN. All rights reserved.
!
!--------1---------2---------3---------4---------5---------6---------7---------8

module kmath_dft_mod

  implicit none
  private

  ! constants
  !
  double precision,   parameter   :: PI = 4 * ATAN(1d0)
  complex(kind(0d0)), parameter   :: i  = (0.0,1.0)


  ! public procedures
  !
  public :: KMATH_DFT

contains

  !======1=========2=========3=========4=========5=========6=========7=========8
  !
  !  Subroutine  KMATH_DFT
  !> @brief      execute discrete fourier transform
  !! @authors    NT
  !! @param[in]  X   input/output array
  !! @param[in]  B   working array
  !! @param[in]  N   size of input array
  !! @param[in]  OPT -1:forward transform  1:inverse transform
  !! @date       2013/01/31 (NT)
  !
  !======1=========2=========3=========4=========5=========6=========7=========8

  subroutine KMATH_DFT(X, B, N, opt)

    ! formal arguments
    complex(kind(0d0)), intent(inout) :: X(:)
    complex(kind(0d0)), intent(inout) :: B(:)
    integer,            intent(in)    :: N
    integer,            intent(in)    :: opt

    ! local variables
    complex(kind(0d0))  :: W
    double precision    :: a
    integer             :: j, k


    if (opt == -1) then

      ! forward transform
      !

      a = -2.d0 * PI / DBLE(N)
      W = COS(a) + i * SIN(a)

      do j = 1, N
        B(j) = (0, 0)
        do k = 1, N
          B(j) = B(j) + X(k)*(W**((j-1)*(k-1)))
        end do
      end do

    else

      ! inverse transform
      !

      a = 2.d0 * PI / DBLE(N)
      W = COS(a) + i * SIN(a)

      do j = 1, N
        B(j) = (0, 0)
        do k = 1, N
          B(j) = B(j) + X(k)*(W**((j-1)*(k-1)))
        end do
        B(j) = B(j) / N
      end do

    end if

    X(1:N) = B(1:N)

    return

  end subroutine KMATH_DFT


end module kmath_dft_mod
