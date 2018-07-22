**************************************
*  spline_interp.f Ver.1.1 '07.09.21 *
*      for peachgk_md.f              *
*            by G.Kikugawa           *
*   modified by D. Torii in Jan2008  *
**************************************
c
c description:
c   methods for spline interpolation of electric interaction
c
c note: this routine is for spline interpolation of electric interaction
c       Spline order is limited to 3rd order
c
      subroutine cal_spl_coeff(spltbl,ncell,spltbl_int,natural)

      implicit none

c     include 'md_common.h'

c ARGUMENT:
c     INPUT
      real*8:: spltbl(0:4,0:*) ! interpolation data array
      real*8:: spltbl_int
      integer:: ncell,natural

c LOCAL:
      real*8:: h(0:ncell)
      real*8:: alpha(0:ncell),l(0:ncell),mu(0:ncell),z(0:ncell)
      real*8:: mu2,z2

      integer:: i

c     +     +     +     +     +     +     +

      h(0) = spltbl_int
      do i = 1, ncell-1
         h(i) = spltbl_int
         alpha(i) = 3.0d0 / h(i) * (spltbl(1,i+1) - spltbl(1,i))
     &            - 3.0d0 / h(i-1) * (spltbl(1,i) - spltbl(1,i-1))
      end do

      if(natural) then
        mu(0) = 0.0d0
        z(0)  = 0.0d0
      else
        mu(0) = 0.5d0
        z(0)  = ( (spltbl(1,1) - spltbl(1,0)) / h(0)
     &           - spltbl(2,0) ) * 1.5d0 / h(0)
      end if
      do i = 1, ncell-1
         l(i)  = 2.0d0 * (h(i-1)+h(i)) - h(i-1)*mu(i-1)
         mu(i) = h(i) / l(i)
         z(i)  = (alpha(i) - h(i-1)*z(i-1)) / l(i)
      end do

      if(natural) then
        spltbl(3,ncell) = 0.0d0
      else
        mu2 = 2.0d0
        z2  = ( - (spltbl(1,ncell) - spltbl(1,ncell-1)) / h(ncell-1)
     &           + spltbl(2,ncell) ) * 3.0d0 / h(ncell-1)
        spltbl(3,ncell) = (z2 - z(ncell-1)) / (mu2 - mu(ncell-1))
      end if
      do i = ncell-1, 0, -1
         spltbl(3,i) = z(i) - mu(i)*spltbl(3,i+1)
         spltbl(2,i) = (spltbl(1,i+1) - spltbl(1,i)) / h(i)
     &            - (spltbl(3,i+1) + 2.0d0*spltbl(3,i)) * h(i) / 3.0d0
         spltbl(4,i) = (spltbl(3,i+1) - spltbl(3,i)) / (3.0d0 * h(i))
      end do

c     +     +     +     +     +     +     +

      return
      end
