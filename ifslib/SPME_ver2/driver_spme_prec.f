      implicit none
      integer numatoms,order,nsp,nfft1,nfft2,nfft3
      double precision cutoff,ewaldcof
c     parameter(numatoms=9225,
      parameter(numatoms=3075,
     $          cutoff=16.0d-10,
     $          ewaldcof=0.3d10,
     $          order= 8,
     $          nfft1=64,nfft2=64,nfft3=64,
     $          nsp=100)
      double precision x(numatoms),y(numatoms),z(numatoms)
      double precision cg(numatoms)
      double precision flg_minimg(numatoms,numatoms)
      double precision box(3),cgh,cgo
      integer numwats,i,j,n,numreps,numatomsd
      double precision Q(2,2*(nfft1/2)+1,2*(nfft2/2)+1,2*(nfft3/2)+1)
      double precision prec_ene, prec_fx, prec_fy, prec_fz, prec_vir,
     &   prec_rec_ene, prec_rfx, prec_rfy, prec_rfz, prec_rec_vir,
     &   prec_dir_ene, prec_dfx, prec_dfy, prec_dfz, prec_dir_vir

      cgh = 0.417d0 * 1.602177d-19*sqrt(8.99d9)
      cgo = -2.d0*cgh
      open(unit=8,file='~/PUB_PME_release/small1000.pdb',status='old')
      read(8,25)box(1),numatomsd
25    format(21x,f9.3,i6)
      write(6,*)'box = ',box(1),' numatomsd = ',numatomsd
      box(2) = box(1)
      box(3) = box(1)
      if (numatoms .ne. numatomsd) then
        write(6,*) 'numatoms = ',numatoms,'numatomsd = ',numatomsd
        stop
      end if

      n = 0
      numwats = numatoms/3
      do 100 i = 1,numwats
       read(8,20)x(n+1),y(n+1),z(n+1)
       read(8,20)x(n+2),y(n+2),z(n+2)
       read(8,20)x(n+3),y(n+3),z(n+3)
       cg(n+1) = cgo
       cg(n+2) = cgh
       cg(n+3) = cgh
       n = n+3
100   continue
20    format(30x,3f8.3)

      do j=1,numatoms
      do i=1,numatoms
        flg_minimg(i,j) = 1.0d0
      end do
      end do
      do n=1,numwats
        do j=(n-1)*3+1,n*3
        do i=(n-1)*3+1,n*3
          flg_minimg(i,j) = 0.0d0
        end do
        end do
      end do

      do i=1,numatoms
        x(i) = x(i) * 1.0d-10
        y(i) = y(i) * 1.0d-10
        z(i) = z(i) * 1.0d-10
      end do
      box(1) = box(1) * 1.0d-10
      box(2) = box(2) * 1.0d-10
      box(3) = box(3) * 1.0d-10

c     call spme_prec(numatoms, x, y, z, cg, flg_minimg, box, 
c    &   cutoff, ewaldcof, order, nfft1, nfft2, nfft3, 
c    &   prec_ene, prec_fx, prec_fy, prec_fz, prec_vir, 
c    &   prec_rec_ene, prec_rfx, prec_rfy, prec_rfz, prec_rec_vir, 
c    &   prec_dir_ene, prec_dfx, prec_dfy, prec_dfz, prec_dir_vir, Q)

      call spme_prec_sp(numatoms, x, y, z, cg, flg_minimg, box, 
     &   cutoff, ewaldcof, order, nfft1, nfft2, nfft3, nsp, 
     &   prec_ene, prec_fx, prec_fy, prec_fz, prec_vir, 
     &   prec_rec_ene, prec_rfx, prec_rfy, prec_rfz, prec_rec_vir, 
     &   prec_dir_ene, prec_dfx, prec_dfy, prec_dfz, prec_dir_vir, Q)

      write(6,'(3f9.4)') prec_ene,prec_rec_ene,prec_dir_ene
      write(6,'(3f9.4)') prec_fx,prec_rfx,prec_dfx
      write(6,'(3f9.4)') prec_fy,prec_rfy,prec_dfy
      write(6,'(3f9.4)') prec_fz,prec_rfz,prec_dfz
      write(6,'(3f9.4)') prec_vir,prec_rec_vir,prec_dir_vir
      write(6,*)

      stop
      end
