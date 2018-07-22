c---------------------------------------------------------------------
c   SPME samle program - usage of the subroutine 'spme_all_sp'
c         Two TIP3P water molecules with total of 6 point charges 
c         (3 charges on each molecule) in a cubic basic cell.
c---------------------------------------------------------------------
      implicit none
      integer numcgs,iorder,nfft1,nfft2,nfft3,nsp
      double precision cutoff,ewaldcof
      parameter(numcgs=6,
     &          cutoff=0.6386d-8,      ! [m]
     &          ewaldcof=0.6194d9,   ! [1/m]
     &          iorder=8,
     &          nfft1=9,nfft2=9,nfft3=9,
     &          nsp=204)
      double precision x(numcgs),y(numcgs),z(numcgs),cg(numcgs)
      double precision flg_minimg(numcgs,numcgs)
      double precision box(3)
      double precision rec_ene,self_ene,dir_ene
      double precision rfx(numcgs),rfy(numcgs),rfz(numcgs)
      double precision dfx(numcgs),dfy(numcgs),dfz(numcgs)
      double precision rec_vir(6),dir_vir(6)
      double precision bsp_mod(max(nfft1,nfft2,nfft3),3)
      double precision fftable(2*(nfft1+nfft2+nfft3)+256*3)
      double precision q(2,2*(nfft1/2)+1,2*(nfft2/2)+1,2*(nfft3/2)+1)
      double precision spdr,spcof(9,nsp)
c
      integer nstep,nummols,i,j,n
      double precision cgh,cgo,factor,ENE,FORCE_x(numcgs),
     &                 FORCE_y(numcgs),FORCE_z(numcgs),VIR(6)
      double precision rfx_correc,rfy_correc,rfz_correc
c
c-----data input
      cgh =  0.417d0 * 1.602177d-19   ! [C]
      cgo = -0.834d0 * 1.602177d-19   ! [C]
      cg(1) = cgo
      cg(2) = cgh
      cg(3) = cgh
      cg(4) = cgo
      cg(5) = cgh
      cg(6) = cgh
c
      box(1) = 37.8d-10   ! [m]
      box(2) = 37.8d-10
      box(3) = 37.8d-10
c
c-----this is to obtain the results in [J] for energy and virials 
c-----and in [N] for forces
      factor = sqrt( 8.9875517880d9 )
      do i = 1,numcgs
        cg(i) = cg(i)*factor
      end do
c
c-----definition of the array 'flg_minimg'
      do j = 1,numcgs
      do i = 1,numcgs
        flg_minimg(i,j) = 1.0d0
      end do
      end do
      nummols = numcgs/3
      do n = 1,nummols              ! no interaction among charges
     &                              ! in the same molecule
        do j = 3*(n-1)+1,3*n
        do i = 3*(n-1)+1,3*n
          flg_minimg(i,j) = 0.0d0
        end do
        end do
      end do
c
c-----initialization
      call spme_init_sp(numcgs,iorder,nfft1,nfft2,nfft3,bsp_mod,
     &                  fftable,cutoff,ewaldcof,nsp,spdr,spcof)
c
      nstep = 0
 1000 nstep = nstep + 1
c
c-----data input
      x(1) =  2.789d-10   ! [m]
      y(1) =  2.193d-10
      z(1) =  1.355d-10
      x(2) =  2.708d-10
      y(2) =  1.279d-10
      z(2) =  1.084d-10
      x(3) =  2.375d-10
      y(3) =  2.690d-10
      z(3) =  0.649d-10
      x(4) = 10.921d-10
      y(4) = 23.971d-10
      z(4) = 28.236d-10
      x(5) = 10.145d-10
      y(5) = 23.491d-10
      z(5) = 28.526d-10
      x(6) = 11.572d-10
      y(6) = 23.292d-10
      z(6) = 28.061d-10
c
c-----calculation of Coulomb interaction
      call spme_all_sp(numcgs,x,y,z,cg,flg_minimg,box,
     &              rec_ene,rfx,rfy,rfz,rec_vir,self_ene,
     &              dir_ene,dfx,dfy,dfz,dir_vir,
     &              cutoff,ewaldcof,iorder,nfft1,nfft2,nfft3,
     &              bsp_mod,fftable,q,spdr,spcof)
c
c-----conservation of momentum
      rfx_correc = 0.0d0
      rfy_correc = 0.0d0
      rfz_correc = 0.0d0
      do i = 1,numcgs
        rfx_correc = rfx_correc + rfx(i)
        rfy_correc = rfy_correc + rfy(i)
        rfz_correc = rfz_correc + rfz(i)
      end do
      rfx_correc = rfx_correc / DBLE(numcgs)
      rfy_correc = rfy_correc / DBLE(numcgs)
      rfz_correc = rfz_correc / DBLE(numcgs)
      do i = 1,numcgs
        rfx(i) = rfx(i) - rfx_correc
        rfy(i) = rfy(i) - rfy_correc
        rfz(i) = rfz(i) - rfz_correc
      end do
c
c-----sum up to obtain the total
      ENE = rec_ene + self_ene + dir_ene         ! [J]
      do i = 1,numcgs
        FORCE_x(i) = rfx(i) + dfx(i)             ! [N]
        FORCE_y(i) = rfy(i) + dfy(i)
        FORCE_z(i) = rfz(i) + dfz(i)
      end do
      do i = 1,6
        VIR(i) = rec_vir(i) + dir_vir(i)         ! [J]
      end do
c
c-----output
      write(*,*)
      write(*,'(a,e11.4)') 'ENE = ',ENE
      write(*,*)
      write(*,'(a2,3a13)') 'i','FORCE_x(i)','FORCE_y(i)','FORCE_z(i)'
      do i = 1,numcgs
        write(*,'(i2,3e13.4)') i,FORCE_x(i),FORCE_y(i),FORCE_z(i)
      end do
      write(*,*)
      write(*,'(a2,a11)') 'i','VIR(i)'
      do i = 1,6
        write(*,'(i2,e13.4)') i,VIR(i)
      end do
c
c     go to 1000
c
      stop
      end
c---------------------------------------------------------------------
c     The result should look something like this:
c---------------------------------------------------------------------
c [MHa000@afivis *]$ ifort -openmp -save sample_spme_all_sp.f -lscs_mp -lifs
c [MHa000@afivis *]$ export OMP_NUM_THREADS=2
c [MHa000@afivis *]$ time dplace -x2 ./a.out
c  
c ENE = -0.8423E-22
c  
c  i   FORCE_x(i)   FORCE_y(i)   FORCE_z(i)
c  1  -0.1303E-12   0.1999E-11   0.9921E-12
c  2   0.1454E-12  -0.9663E-12  -0.5848E-12
c  3   0.3888E-13  -0.1055E-11  -0.4607E-12
c  4  -0.6437E-12   0.1629E-11   0.3420E-12
c  5   0.3384E-12  -0.8516E-12  -0.1513E-12
c  6   0.2513E-12  -0.7549E-12  -0.1373E-12
c  
c  i     VIR(i)
c  1   0.7641E-22
c  2  -0.7860E-22
c  3  -0.7204E-22
c  4  -0.4110E-22
c  5  -0.9583E-23
c  6   0.4892E-22
c 
c real    0m0.147
c user    0m0.020
c sys     0m0.040
c [MHa000@afivis *]$ 
