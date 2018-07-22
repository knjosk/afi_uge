c---------------------------------------------------------------------
      subroutine spme_init(numatoms,order,nfft1,nfft2,nfft3,
     $     bsp_mod,fftable)

      implicit none
      integer numatoms,order,nfft1,nfft2,nfft3
      double precision bsp_mod(max(nfft1,nfft2,nfft3),3)
      double precision fftable(*)

      integer sizfftab,sizffwrk,siztheta,sizheap
c     integer*8 siz_Q,sizstack
      double precision ffwork(0)

c     call pmesh_kspace_get_sizes(
c    $     nfft1,nfft2,nfft3,numatoms,order,
c    $     sizfftab,sizffwrk,siztheta,siz_Q,sizheap,sizstack)
      call pmesh_kspace_setup(
     $    bsp_mod(1,1),bsp_mod(1,2),bsp_mod(1,3),
     $    fftable,ffwork,
     $    nfft1,nfft2,nfft3,order,sizfftab,sizffwrk)
c     if ( siz_Q .gt. MAXT )then
c      write(6,*)'fft needs more room'
c      stop
c     endif

      return
      end
c---------------------------------------------------------------------
      subroutine spme_init_sp(numatoms,order,nfft1,nfft2,nfft3,
     $                        bsp_mod,fftable,
     $                        cutoff,ewaldcof,nsp,spltbl_int,spltbl)

      implicit none

      integer numatoms,order,nfft1,nfft2,nfft3
      double precision bsp_mod(max(nfft1,nfft2,nfft3),3)
      double precision fftable(*)
      integer sizfftab,sizffwrk,siztheta,sizheap
      double precision ffwork(0)

      integer nsp,i,j
      double precision cutoff,ewaldcof,spltbl_int,spltbl(0:8,0:nsp-1),
     &                 erf,exp,x,fac,pi,atan,
     &                 spltbl_erf(0:4,0:nsp-1),spltbl_exp(0:4,0:nsp-1)

      call pmesh_kspace_setup(
     $    bsp_mod(1,1),bsp_mod(1,2),bsp_mod(1,3),
     $    fftable,ffwork,
     $    nfft1,nfft2,nfft3,order,sizfftab,sizffwrk)

      pi = 4.0d0 * atan(1.0d0)
      fac = (2.d0/sqrt(pi))*ewaldcof
      call nsp_to_spltbl_int(nsp,cutoff,spltbl_int)

!$omp parallel do
      do j=0,nsp-1
      do i=0,8
        spltbl(i,j) = 0.0d0
      end do
      end do
!$omp parallel do
      do j=0,nsp-1
      do i=0,4
        spltbl_erf(i,j) = 0.0d0    !! f(x) = - erf(r*ewaldcof)
        spltbl_exp(i,j) = 0.0d0    !! f(x) = fac*exp(-(r*ewaldcof)**2)
      end do
      end do

      do j=0,nsp-1
        spltbl_erf(0,j) = DBLE(j)*spltbl_int
        spltbl_exp(0,j) = spltbl_erf(0,j)
        x = spltbl_erf(0,j)*ewaldcof
        spltbl_erf(1,j) = - erf(x)
        spltbl_exp(1,j) = fac * exp(-x**2)
      end do

c     if(.not.natural) then   ! clamped boundary condition
        spltbl_erf(2,0)     = - 2.0d0 * ewaldcof / sqrt(pi)
        spltbl_erf(2,nsp-1) = spltbl_erf(2,0) 
     &                      * exp(-(ewaldcof*spltbl_erf(0,nsp-1))**2)
        spltbl_exp(2,0)     = 0.0d0
        spltbl_exp(2,nsp-1) = - 2.0d0 * ewaldcof**2
     &                      * spltbl_exp(0,nsp-1)*spltbl_exp(1,nsp-1)
c     end if

      call cal_spl_coeff(spltbl_erf,nsp-1,spltbl_int,0)
      call cal_spl_coeff(spltbl_exp,nsp-1,spltbl_int,0)

      do j=0,nsp-1
        spltbl(0,j) = spltbl_erf(0,j)
      do i=1,4
        spltbl(5-i,j) = spltbl_erf(i,j)
        spltbl(9-i,j) = spltbl_exp(i,j)
      end do
      end do

      return
      end
