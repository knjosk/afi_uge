c---------------------------------------------------------------------
      subroutine spme_all(numatoms,x,y,z,cg,flg_minimg,box,
     $   rec_ene,rfx,rfy,rfz,rec_vir,self_ene,
     $   dir_ene,dfx,dfy,dfz,dir_vir,
     $   cutoff,ewaldcof,order,nfft1,nfft2,nfft3,
     $   bsp_mod,fftable,Q)

      implicit none
      integer numatoms,i,j
      double precision x(*),y(*),z(*),cg(*),box(3)
      double precision flg_minimg(*)
      double precision dir_ene,rec_ene,self_ene
      double precision dir_vir(6),rec_vir(6)
      double precision dfx(*),dfy(*),dfz(*)
      double precision rfx(*),rfy(*),rfz(*)
      double precision cutoff,ewaldcof
      double precision bsp_mod(*)
      double precision fftable(*)
      double precision Q(*)
      integer order,nfft1,nfft2,nfft3

      double precision recip(3,3),volume

      if (ewaldcof.eq.0.d0) then
!$omp parallel do
        do 52 i = 1,numatoms
          rfx(i) = 0.d0
          rfy(i) = 0.d0
          rfz(i) = 0.d0
52      continue
        do 53 i = 1,6
          rec_vir(i) = 0.d0
53      continue
        rec_ene = 0.d0
        self_ene = 0.d0
        go to 300
      end if

      volume = box(1)*box(2)*box(3)
      do 133 j = 1,3
       do 132 i = 1,3
        recip(i,j) = 0.d0
132    continue
133   continue
      recip(1,1) = 1.d0/box(1)
      recip(2,2) = 1.d0/box(2)
      recip(3,3) = 1.d0/box(3)

c get recip sum

c     write(6,*)'doing recip'

c     call do_pmesh_kspace(
      call spme_recip(
     $   numatoms,x,y,z,cg,recip,volume,
     $   rec_ene,rfx,rfy,rfz,rec_vir,
     $   ewaldcof,order,nfft1,nfft2,nfft3,
     $   bsp_mod,fftable,Q)
c     write(6,*)'pme_rec_ene = ',rec_ene

c get self and adjustment values

      call spme_self(cg,numatoms,self_ene,ewaldcof)
c     write(6,*)'self ene = ',self_ene

c get direct sum
c     write(6,*)'doing direct'
  300 call spme_direct(numatoms,x,y,z,cg,cutoff,ewaldcof,box,
     $      dir_ene,dfx,dfy,dfz,dir_vir,flg_minimg)
c     write(6,*)'dir ene = ',dir_ene

      return
      end
c---------------------------------------------------------------------
      subroutine spme_all_sp(numatoms,x,y,z,cg,flg_minimg,box,
     $   rec_ene,rfx,rfy,rfz,rec_vir,self_ene,
     $   dir_ene,dfx,dfy,dfz,dir_vir,
     $   cutoff,ewaldcof,order,nfft1,nfft2,nfft3,
     $   bsp_mod,fftable,Q,spltbl_int,spltbl)

      implicit none
      integer numatoms,i,j
      double precision x(*),y(*),z(*),cg(*),box(3)
      double precision flg_minimg(*)
      double precision dir_ene,rec_ene,self_ene
      double precision dir_vir(6),rec_vir(6)
      double precision dfx(*),dfy(*),dfz(*)
      double precision rfx(*),rfy(*),rfz(*)
      double precision cutoff,ewaldcof
      double precision bsp_mod(*)
      double precision fftable(*)
      double precision Q(*)
      double precision spltbl_int,spltbl(*)
      integer order,nfft1,nfft2,nfft3

      double precision recip(3,3),volume

      if (ewaldcof.eq.0.d0) then
!$omp parallel do
        do 52 i = 1,numatoms
          rfx(i) = 0.d0
          rfy(i) = 0.d0
          rfz(i) = 0.d0
52      continue
        do 53 i = 1,6
          rec_vir(i) = 0.d0
53      continue
        rec_ene = 0.d0
        self_ene = 0.d0
        go to 300
      end if

      volume = box(1)*box(2)*box(3)
      do 133 j = 1,3
       do 132 i = 1,3
        recip(i,j) = 0.d0
132    continue
133   continue
      recip(1,1) = 1.d0/box(1)
      recip(2,2) = 1.d0/box(2)
      recip(3,3) = 1.d0/box(3)

c get recip sum

c     write(6,*)'doing recip'

c     call do_pmesh_kspace(
      call spme_recip(
     $   numatoms,x,y,z,cg,recip,volume,
     $   rec_ene,rfx,rfy,rfz,rec_vir,
     $   ewaldcof,order,nfft1,nfft2,nfft3,
     $   bsp_mod,fftable,Q)
c     write(6,*)'pme_rec_ene = ',rec_ene

c get self and adjustment values

      call spme_self(cg,numatoms,self_ene,ewaldcof)
c     write(6,*)'self ene = ',self_ene

c get direct sum
c     write(6,*)'doing direct'
  300 call spme_direct_sp(numatoms,x,y,z,cg,cutoff,ewaldcof,box,
     $      dir_ene,dfx,dfy,dfz,dir_vir,flg_minimg,spltbl_int,spltbl)
c     write(6,*)'dir ene = ',dir_ene

      return
      end
