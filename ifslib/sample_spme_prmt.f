c---------------------------------------------------------------------
c   SPME samle program - usage of the subroutine 'spme_prmt'
c         Two TIP3P water molecules with total of 6 point charges 
c         (3 charges on each molecule) in a cubic basic cell.
c         Revised in May 2008.
c---------------------------------------------------------------------
      implicit none
      integer numcgs,nfft_max
      parameter (numcgs=6,nfft_max=1024)
      integer iorder,nfft1,nfft2,nfft3
      double precision cutoff,cutoff_min,ewaldcof
      double precision x(numcgs),y(numcgs),z(numcgs),cg(numcgs)
      double precision flg_minimg(numcgs,numcgs)
      double precision box(3)
      double precision q(2,2*(nfft_max/2)+1,
     &                     2*(nfft_max/2)+1,
     &                     2*(nfft_max/2)+1)
      double precision prec_req,prec_ene,prec_fx,prec_fy,prec_fz,
     &                 prec_vir,time
c
      integer nummols,i,j,n
      double precision cgh,cgo
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
      prec_req = -6.0d0      ! common logarithm of precision required
      cutoff_min = 2.0d-10   ! largest distance between charges that
     &                       ! do not interact each other [m]
c
      call spme_prmt(numcgs,x,y,z,cg,flg_minimg,box,
     &               prec_req,cutoff_min,cutoff,ewaldcof,
     &               iorder,nfft1,nfft2,nfft3,
     &               prec_ene,prec_fx,prec_fy,prec_fz,prec_vir,
     &               time,q)
c
      write(*,*)
      write(*,'(a)')         'Best combination of parameters are : '
      write(*,'(a11,e10.4)') 'cutoff = ', cutoff       ! [m]
      write(*,'(a11,e10.4)') 'ewaldcof = ', ewaldcof   ! [1/m]
      write(*,'(a11,i4)')    'iorder = ', iorder
      write(*,'(a11,i4)')    'nfft1 = ', nfft1
      write(*,'(a11,i4)')    'nfft2 = ', nfft2
      write(*,'(a11,i4)')    'nfft3 = ', nfft3
      write(*,*)
      write(*,'(2a)')        'With these parameters, ',
     &                       'resulted precisions are : '
      write(*,'(a14,f8.4)')  'for FORCE_x : ', prec_fx
      write(*,'(a14,f8.4)')  'for FORCE_y : ', prec_fy
      write(*,'(a14,f8.4)')  'for FORCE_z : ', prec_fz
      write(*,'(a14,f8.4)')  'for ENE : ', prec_ene
      write(*,'(a14,f8.4)')  'for VIR : ', prec_vir
      write(*,*)
      write(*,'(a,e10.4)')   'Avarage time for spme_all = ', time
c
      stop
      end
c---------------------------------------------------------------------
c     The result should look something like this:
c---------------------------------------------------------------------
c [MHa000@afivis *]$ ifort -openmp -save sample_spme_prmt.f -lscs_mp -lifs
c [MHa000@afivis *]$ export OMP_NUM_THREADS=2
c [MHa000@afivis *]$ time dplace -x2 ./a.out
c Number of CPUs =   2
c examining: stability of the executing host ...
c examining: ewaldcof = 0.2606E+10
c examining: ewaldcof = 0.2630E+10
c examining: ewaldcof = 0.2655E+10
c examining: ewaldcof = 0.2679E+10
c examining: ewaldcof = 0.2704E+10
c examining: ewaldcof = 0.2729E+10
c examining: ewaldcof = 0.2754E+10
c examining: ewaldcof = 0.2780E+10
c examining: ewaldcof = 0.2805E+10
c examining: ewaldcof = 0.2831E+10
c examining: ewaldcof = 0.2858E+10
c examining: ewaldcof = 0.2884E+10
c examining: ewaldcof = 0.2911E+10
c examining: ewaldcof = 0.2938E+10
c examining: ewaldcof = 0.2965E+10
c examining: ewaldcof = 0.2992E+10
c examining: ewaldcof = 0.3020E+10
c examining: ewaldcof = 0.3048E+10
c examining: ewaldcof = 0.3076E+10
c examining: ewaldcof = 0.3105E+10
c examining: ewaldcof = 0.2582E+10
c examining: ewaldcof = 0.2559E+10
c examining: ewaldcof = 0.2535E+10
c examining: ewaldcof = 0.2512E+10
c examining: ewaldcof = 0.2489E+10
c examining: ewaldcof = 0.2466E+10
c examining: ewaldcof = 0.2443E+10
c examining: ewaldcof = 0.2421E+10
c examining: ewaldcof = 0.2399E+10
c examining: ewaldcof = 0.2377E+10
c examining: ewaldcof = 0.2355E+10
c examining: ewaldcof = 0.2333E+10
c examining: ewaldcof = 0.2312E+10
c examining: ewaldcof = 0.2291E+10
c examining: ewaldcof = 0.2270E+10
c examining: ewaldcof = 0.2249E+10
c examining: ewaldcof = 0.2228E+10
c examining: ewaldcof = 0.2208E+10
c examining: ewaldcof = 0.2188E+10
c examining: ewaldcof = 0.2168E+10
c examining: ewaldcof = 0.2148E+10
c examining: ewaldcof = 0.2128E+10
c examining: ewaldcof = 0.2109E+10
c examining: ewaldcof = 0.2089E+10
c examining: ewaldcof = 0.2070E+10
c examining: ewaldcof = 0.2051E+10
c examining: ewaldcof = 0.2032E+10
c examining: ewaldcof = 0.2014E+10
c examining: ewaldcof = 0.1995E+10
c examining: ewaldcof = 0.1977E+10
c examining: ewaldcof = 0.1959E+10
c examining: ewaldcof = 0.1941E+10
c examining: ewaldcof = 0.1923E+10
c examining: ewaldcof = 0.1905E+10
c examining: ewaldcof = 0.1888E+10
c examining: ewaldcof = 0.1871E+10
c examining: ewaldcof = 0.1854E+10
c examining: ewaldcof = 0.1837E+10
c examining: ewaldcof = 0.1820E+10
c examining: ewaldcof = 0.1803E+10
c examining: ewaldcof = 0.1786E+10
c examining: ewaldcof = 0.1770E+10
c examining: ewaldcof = 0.1754E+10
c examining: ewaldcof = 0.1738E+10
c examining: ewaldcof = 0.1722E+10
c examining: ewaldcof = 0.1706E+10
c examining: ewaldcof = 0.1690E+10
c examining: ewaldcof = 0.1675E+10
c examining: ewaldcof = 0.1660E+10
c examining: ewaldcof = 0.1644E+10
c examining: ewaldcof = 0.1629E+10
c examining: ewaldcof = 0.1614E+10
c examining: ewaldcof = 0.1600E+10
c examining: ewaldcof = 0.1585E+10
c examining: ewaldcof = 0.1570E+10
c examining: ewaldcof = 0.1556E+10
c examining: ewaldcof = 0.1542E+10
c examining: ewaldcof = 0.1528E+10
c examining: ewaldcof = 0.1514E+10
c examining: ewaldcof = 0.1500E+10
c examining: ewaldcof = 0.1486E+10
c examining: ewaldcof = 0.1472E+10
c examining: ewaldcof = 0.1459E+10
c examining: ewaldcof = 0.1445E+10
c examining: ewaldcof = 0.1432E+10
c examining: ewaldcof = 0.1419E+10
c examining: ewaldcof = 0.1406E+10
c examining: ewaldcof = 0.1393E+10
c examining: ewaldcof = 0.1380E+10
c examining: ewaldcof = 0.1368E+10
c examining: ewaldcof = 0.1355E+10
c examining: ewaldcof = 0.1343E+10
c examining: ewaldcof = 0.1330E+10
c examining: ewaldcof = 0.1318E+10
c examining: ewaldcof = 0.1306E+10
c examining: ewaldcof = 0.1294E+10
c examining: ewaldcof = 0.1282E+10
c examining: ewaldcof = 0.1271E+10
c examining: ewaldcof = 0.1259E+10
c examining: ewaldcof = 0.1247E+10
c examining: ewaldcof = 0.1236E+10
c examining: ewaldcof = 0.1225E+10
c examining: ewaldcof = 0.1213E+10
c examining: ewaldcof = 0.1202E+10
c examining: ewaldcof = 0.1191E+10
c examining: ewaldcof = 0.1180E+10
c examining: ewaldcof = 0.1169E+10
c examining: ewaldcof = 0.1159E+10
c examining: ewaldcof = 0.1148E+10
c examining: ewaldcof = 0.1138E+10
c examining: ewaldcof = 0.1127E+10
c examining: ewaldcof = 0.1117E+10
c examining: ewaldcof = 0.1107E+10
c examining: ewaldcof = 0.1096E+10
c examining: ewaldcof = 0.1086E+10
c examining: ewaldcof = 0.1076E+10
c examining: ewaldcof = 0.1067E+10
c examining: ewaldcof = 0.1057E+10
c examining: ewaldcof = 0.1047E+10
c examining: ewaldcof = 0.1038E+10
c examining: ewaldcof = 0.1028E+10
c examining: ewaldcof = 0.1019E+10
c examining: ewaldcof = 0.1009E+10
c examining: ewaldcof = 0.1000E+10
c examining: ewaldcof = 0.9908E+09
c examining: ewaldcof = 0.9817E+09
c examining: ewaldcof = 0.9727E+09
c examining: ewaldcof = 0.9638E+09
c examining: ewaldcof = 0.9550E+09
c examining: ewaldcof = 0.9462E+09
c examining: ewaldcof = 0.9376E+09
c examining: ewaldcof = 0.9290E+09
c examining: ewaldcof = 0.9204E+09
c examining: ewaldcof = 0.9120E+09
c examining: ewaldcof = 0.9036E+09
c examining: ewaldcof = 0.8954E+09
c examining: ewaldcof = 0.8872E+09
c examining: ewaldcof = 0.8790E+09
c examining: ewaldcof = 0.8710E+09
c examining: ewaldcof = 0.8630E+09
c examining: ewaldcof = 0.8551E+09
c examining: ewaldcof = 0.8472E+09
c examining: ewaldcof = 0.8395E+09
c examining: ewaldcof = 0.8318E+09
c examining: ewaldcof = 0.8241E+09
c examining: ewaldcof = 0.8166E+09
c examining: ewaldcof = 0.8091E+09
c examining: ewaldcof = 0.8017E+09
c examining: ewaldcof = 0.7943E+09
c examining: ewaldcof = 0.7870E+09
c examining: ewaldcof = 0.7798E+09
c examining: ewaldcof = 0.7727E+09
c examining: ewaldcof = 0.7656E+09
c examining: ewaldcof = 0.7586E+09
c examining: ewaldcof = 0.7516E+09
c examining: ewaldcof = 0.7447E+09
c examining: ewaldcof = 0.7379E+09
c examining: ewaldcof = 0.7311E+09
c examining: ewaldcof = 0.7244E+09
c examining: ewaldcof = 0.7178E+09
c examining: ewaldcof = 0.7112E+09
c examining: ewaldcof = 0.7047E+09
c examining: ewaldcof = 0.6982E+09
c examining: ewaldcof = 0.6918E+09
c examining: ewaldcof = 0.6855E+09
c examining: ewaldcof = 0.6792E+09
c examining: ewaldcof = 0.6730E+09
c examining: ewaldcof = 0.6668E+09
c examining: ewaldcof = 0.6607E+09
c examining: ewaldcof = 0.6546E+09
c examining: ewaldcof = 0.6486E+09
c examining: ewaldcof = 0.6427E+09
c examining: ewaldcof = 0.6368E+09
c examining: ewaldcof = 0.6310E+09
c examining: ewaldcof = 0.6252E+09
c examining: ewaldcof = 0.6194E+09
c examining: ewaldcof = 0.6138E+09
c examining: ewaldcof = 0.6081E+09
c examining: ewaldcof = 0.6026E+09
c examining: ewaldcof = 0.5970E+09
c examining: ewaldcof = 0.5916E+09
c examining: ewaldcof = 0.5861E+09
c examining: ewaldcof = 0.5808E+09
c examining: ewaldcof = 0.5754E+09
c examining: ewaldcof = 0.5702E+09
c examining: ewaldcof = 0.5649E+09
c examining: ewaldcof = 0.5598E+09
c examining: ewaldcof = 0.5546E+09
c examining: ewaldcof = 0.5495E+09
c examining: ewaldcof = 0.5445E+09
c examining: ewaldcof = 0.5395E+09
c examining: ewaldcof = 0.5346E+09
c examining: ewaldcof = 0.5297E+09
c examining: ewaldcof = 0.5248E+09
c examining: ewaldcof = 0.5200E+09
c examining: ewaldcof = 0.5152E+09
c examining: ewaldcof = 0.5105E+09
c examining: ewaldcof = 0.5058E+09
c examining: ewaldcof = 0.5012E+09
c examining: ewaldcof = 0.4966E+09
c examining: ewaldcof = 0.4920E+09
c examining: ewaldcof = 0.4875E+09
c examining: ewaldcof = 0.4831E+09
c examining: ewaldcof = 0.4786E+09
c examining: ewaldcof = 0.4742E+09
c examining: ewaldcof = 0.4699E+09
c examining: ewaldcof = 0.4656E+09
c examining: ewaldcof = 0.4613E+09
c examining: ewaldcof = 0.4571E+09
c examining: ewaldcof = 0.4529E+09
c examining: ewaldcof = 0.4487E+09
c examining: ewaldcof = 0.4446E+09
c examining: ewaldcof = 0.4406E+09
c examining: ewaldcof = 0.4365E+09
c examining: ewaldcof = 0.4325E+09
c examining: ewaldcof = 0.4285E+09
c examining: ewaldcof = 0.4246E+09
c examining: ewaldcof = 0.4207E+09
c examining: ewaldcof = 0.4169E+09
c examining: ewaldcof = 0.4130E+09
c examining: ewaldcof = 0.4093E+09
c examining: ewaldcof = 0.4055E+09
c examining: ewaldcof = 0.4018E+09
c examining: ewaldcof = 0.3981E+09
c  
c Best combination of parameters are : 
c   cutoff = 0.6368E-08
c ewaldcof = 0.6252E+09
c   iorder =    8
c    nfft1 =    9
c    nfft2 =    9
c    nfft3 =    9
c  
c With these parameters, resulted precisions are : 
c for FORCE_x :  -6.0326
c for FORCE_y :  -6.2243
c for FORCE_z :  -6.0990
c     for ENE :  -6.1212
c     for VIR :  -4.9949
c  
c Avarage time for spme_all = 0.6748E-03
c 
c real    30m46.306s
c user    61m14.604s
c sys     0m11.231s
c [MHa000@afivis *]$ 
