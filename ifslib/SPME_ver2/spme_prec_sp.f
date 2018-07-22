      subroutine spme_prec_sp(numatoms, x, y, z, cg, flg_minimg, box, 
     &   cutoff, ewaldcof, iorder, nfft1, nfft2, nfft3, nsp, 
     &   prec_ene, prec_fx, prec_fy, prec_fz, prec_vir, 
     &   prec_rec_ene, prec_rfx, prec_rfy, prec_rfz, prec_rec_vir, 
     &   prec_dir_ene, prec_dfx, prec_dfy, prec_dfz, prec_dir_vir, Q)
      implicit real*8(a-h,o-z)

      dimension x(*),y(*),z(*),cg(*),flg_minimg(*),box(3),Q(*)
      dimension recip(3,3),mlimit(3),force(3,numatoms)
      dimension rfx_exact(numatoms),dfx_exact(numatoms),
     &          rfy_exact(numatoms),dfy_exact(numatoms),
     &          rfz_exact(numatoms),dfz_exact(numatoms),
     &          rvir_exact(6),dvir_exact(6)
      dimension rfx(numatoms),dfx(numatoms),
     &          rfy(numatoms),dfy(numatoms),
     &          rfz(numatoms),dfz(numatoms),
     &          rec_vir(6),dir_vir(6)
      dimension fx_exact(numatoms),
     &          fy_exact(numatoms),
     &          fz_exact(numatoms),
     &          vir_exact(6)
      dimension fx(numatoms),
     &          fy(numatoms),
     &          fz(numatoms),
     &          vir(6)

      dimension bsp_mod(max(nfft1,nfft2,nfft3),3)
      dimension fftable(2*(nfft1+nfft2+nfft3)+256*3)
      dimension spltbl(0:8,0:nsp-1)

      volume = box(1)*box(2)*box(3)
      do 133 j = 1,3
       do 132 i = 1,3
        recip(i,j) = 0.d0
132    continue
133   continue
      recip(1,1) = 1.d0/box(1)
      recip(2,2) = 1.d0/box(2)
      recip(3,3) = 1.d0/box(3)



c--1. find exact force, energy and virial for given ewaldcof
      tolerance_exact = 1.0d-18
c----1.1 find 'cutoff_exact', cutoff in real space
c     write(*,'(e12.4,2x,a)') ewaldcof,'1.1'
      call find_cutoff(cutoff_exact,tolerance_exact,ewaldcof)
c----1.2 find 'expmax', cutoff in reciprocal space
c     write(*,'(e12.4,2x,a)') ewaldcof,'1.2'
      call find_maxexp(ewaldcof,tolerance_exact,expmax)
c----1.3 find the number of reciprocal space vectors needed
c     write(*,'(e12.4,2x,a)') ewaldcof,'1.3'
      eigmin = 1.d0
      call get_mlim(expmax,mlimit,eigmin,box,recip)
      if(mlimit(1)*mlimit(2)*mlimit(3).eq.0) then
C       print *,'failed to obtain exact reciprocal sums!'
C       print *,'one of mlimit is zero.'
C       go to 801
      end if
c----1.4 find exact force, potential and virial in reciprocal space
c     write(*,'(e12.4,2x,a)') ewaldcof,'1.4'
      write(*,'(2a)') 'calculating the "exact" energy, force, and ',
     &                'virial for the reciprocal space ...'
      call reg_kspace(numatoms,x,y,z,cg,ewaldcof,
     $       rene_exact,force,recip,
     $       expmax,mlimit,volume,box,rvir_exact)
!$omp parallel do
      do 200 i = 1,numatoms
       rfx_exact(i) = force(1,i)
       rfy_exact(i) = force(2,i)
       rfz_exact(i) = force(3,i)
200   continue
c----1.5 get self value
c     write(*,'(e12.4,2x,a)') ewaldcof,'1.5'
      call spme_self(cg,numatoms,self_ene_exact,ewaldcof)
c----1.6 get exact direct sum
c     write(*,'(e12.4,2x,a)') ewaldcof,'1.6'
      write(*,'(2a)') 'calculating the "exact" energy, force, and ',
     &                'virial for the real space ...'
      call spme_direct(numatoms,x,y,z,cg,cutoff_exact,ewaldcof,box,
     $      dene_exact,dfx_exact,dfy_exact,dfz_exact,dvir_exact,
     $      flg_minimg)
c----1.7 sum up
c     write(*,'(e12.4,2x,a)') ewaldcof,'1.7'
!$omp parallel do
      do i=1,numatoms
        fx_exact(i) = rfx_exact(i) + dfx_exact(i)
        fy_exact(i) = rfy_exact(i) + dfy_exact(i)
        fz_exact(i) = rfz_exact(i) + dfz_exact(i)
      end do
      do i=1,6
        vir_exact(i) = rvir_exact(i) + dvir_exact(i)
      end do
      ene_exact = rene_exact + self_ene_exact + dene_exact


c     write(*,'(e12.4,2x,a)') ewaldcof,'2.1'
      call spme_init_sp(numatoms,iorder,nfft1,nfft2,nfft3,
     $     bsp_mod,fftable,cutoff,ewaldcof,nsp,spltbl_int,spltbl)

c     write(*,'(e12.4,2x,a)') ewaldcof,'2.2'
      write(*,'(2a)') 'calculating the energy, force, and ',
     &                'virial to be compared ...'
      call spme_all_sp(numatoms,x,y,z,cg,flg_minimg,box,
     $   rec_ene,rfx,rfy,rfz,rec_vir,self_ene,
     $   dir_ene,dfx,dfy,dfz,dir_vir,
     $   cutoff,ewaldcof,iorder,nfft1,nfft2,nfft3,
     $   bsp_mod,fftable,Q,spltbl_int,spltbl)

c     write(*,'(e12.4,2x,a)') ewaldcof,'2.3'
      call rms_correction(numatoms,rfx,rfy,rfz)

!$omp parallel do
      do i=1,numatoms
        fx(i) = rfx(i) + dfx(i)
        fy(i) = rfy(i) + dfy(i)
        fz(i) = rfz(i) + dfz(i)
      end do
      do i=1,6
        vir(i) = rec_vir(i) + dir_vir(i)
      end do
      ene = rec_ene + self_ene + dir_ene



c     write(*,'(e12.4,2x,a)') ewaldcof,'3.1'
      call comp_pme(numatoms,ene,fx,fy,fz,vir,
     &          ene_exact,fx_exact,fy_exact,fz_exact,vir_exact,
     &          prec_ene, prec_fx, prec_fy, prec_fz, prec_vir)

c     write(*,'(e12.4,2x,a)') ewaldcof,'3.2'
      call comp_pme(numatoms,rec_ene,rfx,rfy,rfz,rec_vir,
     &       rene_exact,rfx_exact,rfy_exact,rfz_exact,rvir_exact,
     &       prec_rec_ene, prec_rfx, prec_rfy, prec_rfz, prec_rec_vir)

c     write(*,'(e12.4,2x,a)') ewaldcof,'3.3'
      call comp_pme(numatoms,dir_ene,dfx,dfy,dfz,dir_vir,
     &       dene_exact,dfx_exact,dfy_exact,dfz_exact,dvir_exact,
     &       prec_dir_ene, prec_dfx, prec_dfy, prec_dfz, prec_dir_vir)

      return
      end
