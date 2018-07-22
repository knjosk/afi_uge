      implicit none
      integer numatoms,order,nfft1,nfft2,nfft3
      double precision cutoff,ewaldcof
      parameter(numatoms=9225,
c     parameter(numatoms=3075,
     $          cutoff=23.0d-10,
     $          ewaldcof=0.1d10,
     $          order= 4,
     $          nfft1=8,nfft2=8,nfft3=8)
      double precision x(numatoms),y(numatoms),z(numatoms)
      double precision cg(numatoms)
      double precision flg_minimg(numatoms,numatoms)
      double precision box(3),cgh,cgo
      double precision dir_ene,rec_ene,self_ene
      double precision dir_vir(6),rec_vir(6)
      double precision dfx(numatoms),dfy(numatoms),dfz(numatoms)
      double precision rfx(numatoms),rfy(numatoms),rfz(numatoms)
      integer numwats,i,j,n,numreps,numatomsd

      double precision bsp_mod(max(nfft1,nfft2,nfft3),3)
      double precision fftable(2*(nfft1+nfft2+nfft3)+256*3)
      double precision Q(2,2*(nfft1/2)+1,2*(nfft2/2)+1,2*(nfft3/2)+1)

      open(unit=10,file='output.d',form='unformatted')
      open(unit=11,file='output.txt',form='formatted')

      cgh = 0.417d0 * 1.602177d-19*sqrt(8.99d9)
      cgo = -2.d0*cgh
      open(unit=8,file='~/PUB_PME_release/small3000.pdb',status='old')
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

c init our stuff

      call spme_init(numatoms,order,nfft1,nfft2,nfft3,
     $     bsp_mod,fftable)

      write(6,*)'done setting up'

      numreps = 0
 1000 numreps = numreps + 1
      print *,'numreps = ',numreps

      call spme_all(numatoms,x,y,z,cg,flg_minimg,box,
     $   rec_ene,rfx,rfy,rfz,rec_vir,self_ene,
     $   dir_ene,dfx,dfy,dfz,dir_vir,
     $   cutoff,ewaldcof,order,nfft1,nfft2,nfft3,
     $   bsp_mod,fftable,Q)

c     if(numreps.lt.5) go to 1000
c     if(numreps.lt.2) go to 1000

      write(10)numatoms
      write(10)cutoff,ewaldcof,box
      write(10)self_ene,dir_ene,rec_ene
      write(10)dir_vir,rec_vir
      write(10)(dfx(i),i=1,numatoms)
      write(10)(dfy(i),i=1,numatoms)
      write(10)(dfz(i),i=1,numatoms)
      write(10)(rfx(i),i=1,numatoms)
      write(10)(rfy(i),i=1,numatoms)
      write(10)(rfz(i),i=1,numatoms)
      close(10)

      write(11,*)numatoms
      write(11,*)cutoff,ewaldcof
      write(11,*)box
      write(11,*)
      write(11,*)dir_ene,rec_ene
      write(11,*)self_ene
      write(11,*)
      write(11,*)dir_vir(1),rec_vir(1)
      write(11,*)dir_vir(2),rec_vir(2)
      write(11,*)dir_vir(3),rec_vir(3)
      write(11,*)dir_vir(4),rec_vir(4)
      write(11,*)dir_vir(5),rec_vir(5)
      write(11,*)dir_vir(6),rec_vir(6)
      write(11,*)
      do i=1,100
        write(11,*)dfx(i),rfx(i)
        write(11,*)dfy(i),rfy(i)
        write(11,*)dfz(i),rfz(i)
        write(11,*)
      end do
      close(11)

      stop
      end
