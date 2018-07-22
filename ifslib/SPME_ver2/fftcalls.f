      subroutine get_fftdims(nfft1,nfft2,nfft3,
     $       nfftdim1,nfftdim2,nfftdim3,nfftable,nffwork,
     $       sizfftab,sizffwrk)
      implicit none
      integer nfft1,nfft2,nfft3,nfftdim1,nfftdim2,nfftdim3,
     $       nfftable,nffwork,sizfftab,sizffwrk
      integer n,nfftmax

      nfftmax = max(nfft1,nfft2,nfft3)
      nfftdim1 = nfft1
      n = nfft1/2
      if ( nfft1 .eq. 2*n )nfftdim1 = nfft1+1
      nfftdim2 = nfft2
      n = nfft2/2
      if ( nfft2 .eq. 2*n )nfftdim2 = nfft2+1
      nfftdim3 = nfft3
      n = nfft3/2
      if ( nfft3 .eq. 2*n )nfftdim3 = nfft3+1

      nfftable = 2*nfft1+256 + 2*nfft2+256 + 2*nfft3+256
      nffwork = 2*nfftmax

      sizfftab = nfftable
      sizffwrk  = nffwork
      return
      end
c---------------------------------------------------------------
      subroutine fft_setup(array,fftable,ffwork,
     $      nfft1,nfft2,nfft3,nfftdim1,nfftdim2,nfftdim3,
     $      nfftable,nffwork)
      implicit none

      double precision array(*),fftable(*),ffwork(*)
      integer nfft1,nfft2,nfft3,nfftdim1,nfftdim2,nfftdim3
      integer nfftable,nffwork

      integer isign,inc1,inc2,inc3
      double precision scale, dummy
      integer isys(0:1)

      isign = 0
      scale = 0.d0
      isys(0) = 1

c     call ZZFFT3D(isign,nfft1,nfft2,nfft3,scale,
      call CCFFT3D(isign,nfft1,nfft2,nfft3,scale,
     $   dummy, 1, 1, dummy, 1, 1,
     $   fftable,dummy,isys)

      return
      end
c-----------------------------------------------------------
      subroutine fft_forward(array,fftable,ffwork,
     $      nfft1,nfft2,nfft3,nfftdim1,nfftdim2,nfftdim3,
     $      nfftable,nffwork)
      implicit none

      double precision array(*),fftable(*),ffwork(*)
      integer nfft1,nfft2,nfft3,nfftdim1,nfftdim2,nfftdim3

      integer isign,inc1,inc2,inc3
      double precision scale
      integer nfftable,nffwork
      integer isys(0:1)

      isign = 1
      scale = 1.d0
      isys(0) = 1

c     call ZZFFT3D(isign,nfft1,nfft2,nfft3,scale,
      call CCFFT3D(isign,nfft1,nfft2,nfft3,scale,
     $   array,nfftdim1,nfftdim2,array,nfftdim1,nfftdim2,
     $   fftable,ffwork,isys)

      return
      end
c-----------------------------------------------------------
      subroutine fft_back(array,fftable,ffwork,
     $      nfft1,nfft2,nfft3,nfftdim1,nfftdim2,nfftdim3,
     $      nfftable,nffwork)
      implicit none

      double precision array(*),fftable(*),ffwork(*)
      integer nfft1,nfft2,nfft3,nfftdim1,nfftdim2,nfftdim3
      integer nfftable,nffwork

      integer isign,inc1,inc2,inc3
      double precision scale
      integer isys(0:1)

      isign = -1
      scale = 1.d0
      isys(0) = 1

c     call ZZFFT3D(isign,nfft1,nfft2,nfft3,scale,
      call CCFFT3D(isign,nfft1,nfft2,nfft3,scale,
     $   array,nfftdim1,nfftdim2,array,nfftdim1,nfftdim2,
     $   fftable,ffwork,isys)

      return
      end
c-----------------------------------------------------------
