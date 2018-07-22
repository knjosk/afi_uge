c---------------------------------------------------------------------
      subroutine spme_prmt(numatoms,x,y,z,cg,flg_minimg,box,
     $     tolerance_org,cutoff_min_org,cutoff_best,ewaldcof_best,
     $     iorder_best,nfft1_best,nfft2_best,nfft3_best,
     $     tol_ene_best,tol_fx_best,tol_fy_best,tol_fz_best,
     $     tol_vir_best,time_best,Q)
      implicit real*8(a-h,o-z)
      INCLUDE 'omp_lib.h'
      parameter(jmax=512)
      parameter(iorder_nchoice=11) ! from 4 to 24, only 2*i
      parameter(nfft_nchoice=81) ! from 8 to 1024, only 2**i*3**j*5**k

      dimension x(*),y(*),z(*),cg(*),flg_minimg(*),box(3),Q(*)
      dimension recip(3,3),
     &          ewaldcof(-jmax:jmax),
     &          cutoff(5,-jmax:jmax),
     &          iorder(5,-jmax:jmax),
     &          nfft1(5,-jmax:jmax),
     &          nfft2(5,-jmax:jmax),
     &          nfft3(5,-jmax:jmax),
     &          tol_ene(5,-jmax:jmax),
     &          tol_fx(5,-jmax:jmax),
     &          tol_fy(5,-jmax:jmax),
     &          tol_fz(5,-jmax:jmax),
     &          tol_vir(5,-jmax:jmax),
     &          dir_time(5,-jmax:jmax),
     &          rec_time(5,-jmax:jmax),
     &          ttl_time(5,-jmax:jmax),
     &          iflag(5,-jmax:jmax)
      dimension calib_cutoff(2*jmax),
     &          calib_time(2*jmax),
     &          i_calib_cutoff(2*jmax),
     &          i_calib_time(2*jmax),
     &          ii_store(2*jmax),
     &          jj_store(2*jmax),
     &          i_best(-jmax:jmax)
      real*4    etime,tm(2)
      dimension k1_ltd(iorder_nchoice,0:nfft_nchoice*3),
     &          k2_ltd(iorder_nchoice,0:nfft_nchoice*3),
     &          k3_ltd(iorder_nchoice,0:nfft_nchoice*3)
      dimension nerrors(3,-jmax:jmax)

      dimension iorder_choice(iorder_nchoice)
      dimension nfft_choice(nfft_nchoice)
      data iorder_choice/4,6,8,10,12,14,16,18,20,22,24/
      data nfft_choice/8,9,10,12,15,16,18,20,24,25,
     &                 27,30,32,36,40,45,48,50,54,60,
     &                 64,72,75,80,81,90,96,100,108,120,
     &                 125,128,135,144,150,160,162,180,192,200,
     &                 216,225,240,243,250,256,270,288,300,320,
     &                 324,360,375,384,400,405,432,450,480,486,
     &                 500,512,540,576,600,625,640,648,675,720,
     &                 729,750,768,800,810,864,900,960,972,1000,1024/

C     open(unit=8,file='time_rvsd.txt')
C     open(unit=10,file='time.txt')
C     open(unit=11,file='nerrors.txt')

c$omp parallel
c$omp single
      ncpus = omp_get_num_threads()
      write(*,'(a,i3)') 'Number of CPUs = ',ncpus
c$omp end single
c$omp end parallel

c$omp parallel do
      do j=-jmax,jmax
      do i=1,5
        cutoff(i,j) = 0.0d0
        iorder(i,j) = 0
        nfft1(i,j) = 0
        nfft2(i,j) = 0
        nfft3(i,j) = 0
        tol_ene(i,j) = 0.0d0
        tol_fx(i,j) = 0.0d0
        tol_fy(i,j) = 0.0d0
        tol_fz(i,j) = 0.0d0
        tol_vir(i,j) = 0.0d0
        dir_time(i,j) = 0.0d0
        rec_time(i,j) = 0.0d0
        ttl_time(i,j) = 0.0d0
        iflag(i,j) = 0
      end do
        ewaldcof(j) = 0.0d0
        i_best(j) = 0
        do i=1,3
          nerrors(i,j) = 0
        end do
      end do
c$omp parallel do
      do j=1,2*jmax
        calib_cutoff(j) = 0.0d0
        calib_time(j) = 0.0d0
        i_calib_cutoff(j) = 0
        i_calib_time(j) = 0
        ii_store(j) = 0
        jj_store(j) = 0
      end do
c$omp parallel do
      do j=0,nfft_nchoice*3
      do i=1,iorder_nchoice
        k1_ltd(i,j) = 0
        k2_ltd(i,j) = 0
        k3_ltd(i,j) = 0
      end do
      end do

      volume = box(1)*box(2)*box(3)
      do 133 j = 1,3
       do 132 i = 1,3
        recip(i,j) = 0.d0
132    continue
133   continue
      recip(1,1) = 1.d0/box(1)
      recip(2,2) = 1.d0/box(2)
      recip(3,3) = 1.d0/box(3)

c--1. find standard time for given system and cpus

c-----1.1 determine cutoff_std and nfft_std

C     ncount_get_std_time = 1

      cutoff_std = 0.0d0
      std_dir_time_ave = 0.0d0
        cutoff_std = 0.3d0 * min(box(1),box(2),box(3))
        call get_std_dir_time(numatoms,x,y,z,cg,cutoff_std,box,
     $                        flg_minimg,std_dir_time_ave,10)
C       write(*,'(i4,4x,a,e10.4,e14.6)') ncount_get_std_time,
C    &           'cutoff_std = ',cutoff_std,std_dir_time_ave
C       ncount_get_std_time = ncount_get_std_time + 1
      do while(std_dir_time_ave.lt.0.2d0)
        call next_value(cutoff_std,10)
        call get_std_dir_time(numatoms,x,y,z,cg,cutoff_std,box,
     $                        flg_minimg,std_dir_time_ave,10)
C       write(*,'(i4,4x,a,e10.4,e14.6)') ncount_get_std_time,
C    &           'cutoff_std = ',cutoff_std,std_dir_time_ave
C       ncount_get_std_time = ncount_get_std_time + 1
      end do
C     write(*,*)

      k_std = 4 - 1
      std_rec_time_ave = 0.0d0
      do while(std_rec_time_ave.lt.0.2d0 .and. k_std.lt.nfft_nchoice)
        k_std = k_std + 1
        nfft_std = nfft_choice(k_std)
        call get_std_rec_time(numatoms,x,y,z,cg,recip,volume,
     $                        nfft_std,box,std_rec_time_ave,Q,10)
C       write(*,'(i4,4x,a,i4,e14.6)')    ncount_get_std_time,
C    &           'nfft_std = ',nfft_std,std_rec_time_ave
C       ncount_get_std_time = ncount_get_std_time + 1
      end do
C     write(*,*)

C     print *,'cutoff_std and nfft_std obtained successfully.'
C     print *,'cutoff_std = ',cutoff_std
C     print *,'  nfft_std = ',nfft_std
C     print *,''

c-----1.2 and then, examine stability of the system, 
c-------------------get std_time and determine prec_std

  150 std_dir_time_ave = 0.0d0
      std_rec_time_ave = 0.0d0
      ncount_dir00 = 0
      ncount_dir05 = 0
      ncount_dir10 = 0
      ncount_dir20 = 0
      ncount_dir50 = 0
      ncount_rec00 = 0
      ncount_rec05 = 0
      ncount_rec10 = 0
      ncount_rec20 = 0
      ncount_rec50 = 0
      t1 = etime(tm)
      t2 = 0.0d0
      i = 0
        call get_std_dir_time(numatoms,x,y,z,cg,cutoff_std,box,
     $                        flg_minimg,std_dir_time_tmp_old,1)
        call get_std_rec_time(numatoms,x,y,z,cg,recip,volume,
     $                        nfft_std,box,std_rec_time_tmp_old,Q,1)
      write(*,'(a)') 'examining: stability of the executing host ...'
c     do while((t2-t1).lt.5.0d0*60.0d0 .or. i.lt.10)
      do while(i.lt.400)
        i = i + 1
        call get_std_dir_time(numatoms,x,y,z,cg,cutoff_std,box,
     $                        flg_minimg,std_dir_time_tmp,1)
        std_dir_time_ave = std_dir_time_ave + std_dir_time_tmp
        call get_std_rec_time(numatoms,x,y,z,cg,recip,volume,
     $                        nfft_std,box,std_rec_time_tmp,Q,1)
        std_rec_time_ave = std_rec_time_ave + std_rec_time_tmp
        std_dir_time_ratio=max((std_dir_time_tmp/std_dir_time_tmp_old),
     &                         (std_dir_time_tmp_old/std_dir_time_tmp))
        std_rec_time_ratio=max((std_rec_time_tmp/std_rec_time_tmp_old),
     &                         (std_rec_time_tmp_old/std_rec_time_tmp))
C       write(*,'(i5,2(e14.6,f8.4))') 
C    &          i,std_dir_time_tmp,std_dir_time_ratio,
C    &            std_rec_time_tmp,std_rec_time_ratio
        if(std_dir_time_ratio.gt.1.05d0) ncount_dir50 = ncount_dir50+1
        if(std_dir_time_ratio.gt.1.02d0) ncount_dir20 = ncount_dir20+1
        if(std_dir_time_ratio.gt.1.01d0) ncount_dir10 = ncount_dir10+1
        if(std_dir_time_ratio.gt.1.005d0)ncount_dir05 = ncount_dir05+1
        if(std_rec_time_ratio.gt.1.05d0) ncount_rec50 = ncount_rec50+1
        if(std_rec_time_ratio.gt.1.02d0) ncount_rec20 = ncount_rec20+1
        if(std_rec_time_ratio.gt.1.01d0) ncount_rec10 = ncount_rec10+1
        if(std_rec_time_ratio.gt.1.005d0)ncount_rec05 = ncount_rec05+1
        std_dir_time_tmp_old = std_dir_time_tmp
        std_rec_time_tmp_old = std_rec_time_tmp
        t2 = etime(tm)
      end do
      ie = i
      std_dir_time = std_dir_time_ave / DBLE(ie)
      std_rec_time = std_rec_time_ave / DBLE(ie)
C     print *,''
C     print *,'std_time obtained successfully.'
C     print *,'std_dir_time = ',std_dir_time
C     print *,'std_rec_time = ',std_rec_time

C     write(*,*) 'ncount_50',ncount_dir50,ncount_rec50
C     write(*,*) 'ncount_20',ncount_dir20,ncount_rec20
C     write(*,*) 'ncount_10',ncount_dir10,ncount_rec10
C     write(*,*) 'ncount_05',ncount_dir05,ncount_rec05
C     print *,''

      if( max(ncount_dir50/40,ncount_rec50/4) .ge. 2 ) then
        print *,'ERROR: The executing host seems too unstable.'
        print *,'       Try executing it later or on another host.'
        stop
      else if( max(ncount_dir10/40,ncount_rec10/4) .ge. 2 ) then
        print *,'WARNING: The executing host seems a bit unstable,'
        print *,'         and the result will be less reliable.'
        print *,'         Try executing it later or on another host,'
        print *,'         if you need more reliability.'
      end if

      prec_std = 1.05d0

c     go to 150

c-----measure time_self
      nrepeats = 2
      n = 0
      t1 = etime(tm)
      t2 = 0.0d0
c     do while((t2-t1).lt.DBLE(nrepeats)*0.5d0)
      do while((t2-t1).lt.0.2d0)
        do i=1,nrepeats
          call spme_self(cg,numatoms,dummy,recip(1,1))
        end do
        n = n + 1
        t2 = etime(tm)
      end do
      time_self = (t2 - t1) / DBLE(n) / DBLE(nrepeats)

      cutoff_min = cutoff_min_org
      call next_value(cutoff_min,0)
      do while(cutoff_min_org.gt.cutoff_min)
        call next_value(cutoff_min,1)
      end do

      tolerance = min(tolerance_org,-1.0d0)

      cutoff_org = 0.35d0 * volume**(1.0d0/3.0d0)
      cutoff_org = max(cutoff_org,cutoff_min)
      call find_ewaldcof(cutoff_org,10.0d0**tolerance,ewaldcof_org)
C     write(*,*) 'ewaldcof_org = ',ewaldcof_org
C     write(*,*)
      call next_value(ewaldcof_org,0)

  200 j = 0
      js = j
      je = j
      n = 0
      n_cutoff_min = 0
      ttl_time_min = 1.0d100
      ttl_time_old = 1.0d100
      ewaldcof(j) = ewaldcof_org
      cutoff_ic = 0.0d0
      do while(n.lt.50 .and. j.lt.jmax .and. n_cutoff_min.lt.20 
     &         .and. (ttl_time_min*2.0d0.gt.ttl_time_old.or.n.lt.10) )

C       write(*,'(a,i5)')    'j = ',j
        write(*,'(a,e10.4)') 'examining: ewaldcof = ',ewaldcof(j)

        call find_prmt(numatoms,x,y,z,cg,flg_minimg,box,recip,volume,
     $     tolerance,cutoff_min,cutoff(1,j),ewaldcof(j),iorder(1,j),
     $     nfft1(1,j),nfft2(1,j),nfft3(1,j),
     $     tol_ene(1,j),tol_fx(1,j),tol_fy(1,j),tol_fz(1,j),
     $     tol_vir(1,j),dir_time(1,j),rec_time(1,j),ttl_time(1,j),
     $     std_dir_time,std_rec_time,cutoff_std,nfft_std,prec_std,
     $     cutoff_ic,k1_ltd,k2_ltd,k3_ltd,
     $     iorder_choice,iorder_nchoice,nfft_choice,nfft_nchoice,Q,
     $     nerrors(1,j))
C       write(11,'(i5,e12.4,3i4)') j,ewaldcof(j),(nerrors(i,j),i=1,3)
        if(ttl_time(1,j).gt.1.0d99) then
          if(j.eq.0) then
            print *,'ERROR: Impossible to achieve such precision!'
            stop
          else
C           write(*,*) 'impossible to achieve such precision with this'
C           write(*,*) 'ewaldcof. exit from the first do-while loop.'
            go to 300
          end if
        end if
        ttl_time_old = ttl_time(1,j)
        call next_value(cutoff_ic,3)
        je = j

        if(ttl_time_min.gt.ttl_time(1,j)) then
          ttl_time_min = ttl_time(1,j)
          j_ttl_time_min = j
          n = 0
        end if
        n = n+1

        do i=1,5
          if(cutoff(i,j) .ne. cutoff_min) n_cutoff_min = 0
        end do
        n_cutoff_min = n_cutoff_min + 1

        ewaldcof_tmp = ewaldcof(j)
        j = j+1
        call next_value(ewaldcof_tmp,1)
        ewaldcof(j) = ewaldcof_tmp
      end do
C     write(*,*) 'exit from first do-while loop'
C     if(n.ge.50) then
C       write(*,*) 'because examining ewaldcof > j_ttl_time_min + much'
C       write(*,*) 'j_ttl_time_min = ',j_ttl_time_min
C     else if(j.ge.jmax) then
C       write(*,*) 'because examining ewaldcof >= jmax'
C     else if(ttl_time_min*2.0d0.le.ttl_time_old) then
C       write(*,*) 'because ttl_time_old > ttl_time_min*2 AND n >= 10'
C       write(*,*) 'ttl_time_old = ',ttl_time_old,' at j = ',
C    &             j,ewaldcof(j)
C       write(*,*) 'ttl_time_min = ',ttl_time_min,' at j = ',
C    &             j_ttl_time_min,ewaldcof(j_ttl_time_min)
C     else if(n_cutoff_min.ge.20) then
C       write(*,*) 'because all cutoffs = cutoff_min for the last 20 ',
C    &             'ewaldcofs'
C       write(*,*) 'n_cutoff_min = ',n_cutoff_min
C     else
C       write(*,*) 'with something unexpected happening!'
C       write(*,*) j,ewaldcof(j),n,ttl_time_min,ttl_time_old
C       stop
C     end if


  300 j = -1
      n = j_ttl_time_min
      ttl_time_old = ttl_time(1,0)
      ewaldcof_tmp = ewaldcof(0)
      call next_value(ewaldcof_tmp,-1)
      ewaldcof(j) = ewaldcof_tmp
      cutoff_ic = 0.0d0
      do while(n.lt.50 .and. j.gt.-jmax 
     &         .and. (ttl_time_min*2.0d0.gt.ttl_time_old.or.n.lt.10) )

C       write(*,'(a,i5)')    'j = ',j
        write(*,'(a,e10.4)') 'examining: ewaldcof = ',ewaldcof(j)

        call find_prmt(numatoms,x,y,z,cg,flg_minimg,box,recip,volume,
     $     tolerance,cutoff_min,cutoff(1,j),ewaldcof(j),iorder(1,j),
     $     nfft1(1,j),nfft2(1,j),nfft3(1,j),
     $     tol_ene(1,j),tol_fx(1,j),tol_fy(1,j),tol_fz(1,j),
     $     tol_vir(1,j),dir_time(1,j),rec_time(1,j),ttl_time(1,j),
     $     std_dir_time,std_rec_time,cutoff_std,nfft_std,prec_std,
     $     cutoff_ic,k1_ltd,k2_ltd,k3_ltd,
     $     iorder_choice,iorder_nchoice,nfft_choice,nfft_nchoice,Q,
     $     nerrors(1,j))
C       write(11,'(i5,e12.4,3i4)') j,ewaldcof(j),(nerrors(i,j),i=1,3)
        if(ttl_time(1,j).gt.1.0d99) then
C         write(*,*) 'impossible to achieve such precision with this'
C         write(*,*) 'ewaldcof. exit from the second do-while loop.'
          go to 400
        end if
        ttl_time_old = ttl_time(1,j)
        call next_value(cutoff_ic,-1)
        js = j

        if(ttl_time_min.gt.ttl_time(1,j)) then
          ttl_time_min = ttl_time(1,j)
          j_ttl_time_min = j
          n = 0
        end if
        n = n+1

        ewaldcof_tmp = ewaldcof(j)
        j = j-1
        call next_value(ewaldcof_tmp,-1)
        ewaldcof(j) = ewaldcof_tmp
      end do
C     write(*,*) 'exit from second do-while loop'
C     if(n.ge.50) then
C       write(*,*) 'because examining ewaldcof < j_ttl_time_min - much'
C       write(*,*) 'j_ttl_time_min = ',j_ttl_time_min
C     else if(j.le.-jmax) then
C       write(*,*) 'because examining ewaldcof <= -jmax'
C     else if(ttl_time_min*2.0d0.le.ttl_time_old) then
C       write(*,*) 'because ttl_time_old > ttl_time_min*2 AND n >= 10'
C       write(*,*) 'ttl_time_old = ',ttl_time_old,' at j = ',
C    &             j,ewaldcof(j)
C       write(*,*) 'ttl_time_min = ',ttl_time_min,' at j = ',
C    &             j_ttl_time_min,ewaldcof(j_ttl_time_min)
C     else
C       write(*,*) 'with something unexpected happening!'
C       write(*,*) j,ewaldcof(j),n,ttl_time_min,ttl_time_old
C       stop
C     end if

c-----time adjustment dir
c-----This part is based on the idea that dir_time should be 
c-----independent on ewaldcofs if the cutoffs are the same. 
c-----This routine searches the same cutoffs for all the ewaldcofs 
c-----examined, averages their dir_time, overwrites them with 
c-----the new dir_time. 
c-----After that, dir_time for all cutoffs are re-ordered by the 
c-----bubble sort in such a way that the smaller the cutoff, 
c-----the smaller the dir_time. 

  400 continue

C     write(*,*)
C     write(*,*) 'jsje = ',js,je

C     do j=js,je
C       write(10,'(i5,2e12.4,4i5,3e15.7)')j,ewaldcof(j),
C    &            cutoff(1,j),
C    &            iorder(1,j),nfft1(1,j),
C    &            nfft2(1,j),nfft3(1,j),
C    &            dir_time(1,j),rec_time(1,j),
C    &            ttl_time(1,j)
C     end do

!$omp parallel do
      do j=-jmax,jmax
      do i=1,5
        iflag(i,j) = 0
      end do
      end do

      k=1
      do j=je,js,-1
      do i=1,5
        if(iflag(i,j).eq.0) then
          calib_cutoff(k) = cutoff(i,j)
          calib_time(k) = 0.0d0
          ncount = 0
          do jj=j,js,-1
            iflag_local = 0
          do ii=1,5
            if(cutoff(ii,jj) .eq. calib_cutoff(k)) then
              iflag(ii,jj) = 1
              if(iflag_local.eq.0) then
                ncount = ncount + 1
                calib_time(k) = calib_time(k)+dir_time(ii,jj)
                iflag_local = 1
              end if
            end if
          end do
          end do
          calib_time(k) = calib_time(k)/DBLE(ncount)
        k=k+1
        end if
      end do
      end do

      ndata = k - 1
C     write(*,*) 'ndata = ',ndata
      call bubble_sort(ndata,calib_time,i_calib_time)
      call bubble_sort(ndata,calib_cutoff,i_calib_cutoff)
C     do n=1,ndata
C       write(*,'(i5,a,e10.4,i5,a,i6,a,e13.7,i5,a)') 
C    &      n,':',calib_cutoff(n),i_calib_cutoff(n),':',
C    &      n,':',calib_time(n),i_calib_time(n),':'
C     end do

C     write(*,*)
C     do i=1,5
C       write(*,*) cutoff(i,je),dir_time(i,je)
C     end do

!$omp parallel do
      do j=js,je
      do i=1,5
        do k=1,ndata
          if(calib_cutoff(i_calib_cutoff(k)).eq.cutoff(i,j)) then
            dir_time(i,j) = calib_time(i_calib_time(k))
          end if
        end do
      end do
      end do

C     write(*,*)
C     do i=1,5
C       write(*,*) cutoff(i,je),dir_time(i,je)
C     end do

c-----time adjustment rec
c-----This part is based on the idea that rec_time should be 
c-----independent on ewaldcofs if the parameter sets (iorder, nfft1, 
c-----nfft2, nfft3) are the same. This routine searches the same 
c-----parameter sets for all the ewaldcofs examined, averages their 
c-----rec_time, overwrites them with the new rec_time. 

!$omp parallel do
      do j=-jmax,jmax
      do i=1,5
        iflag(i,j) = 0
      end do
      end do

      do j=js,je
      do i=1,5
        if(iflag(i,j).eq.0) then
          iorder_tmp = iorder(i,j)
          nfft1_tmp = nfft1(i,j)
          nfft2_tmp = nfft2(i,j)
          nfft3_tmp = nfft3(i,j)
          calib_time_tmp = 0.0d0
          ncount = 0
          ncount2 = 0
          do jj=j,je
            iflag_local = 0
          do ii=1,5
            if(iorder(ii,jj) .eq. iorder_tmp .and.
     &         nfft1(ii,jj) .eq. nfft1_tmp .and.
     &         nfft2(ii,jj) .eq. nfft2_tmp .and.
     &         nfft3(ii,jj) .eq. nfft3_tmp ) then
              ncount2 = ncount2 + 1
              ii_store(ncount2) = ii
              jj_store(ncount2) = jj
              iflag(ii,jj) = 1
              if(iflag_local.eq.0) then
                ncount = ncount + 1
                calib_time_tmp = calib_time_tmp+rec_time(ii,jj)
                iflag_local = 1
              end if
            end if
          end do
          end do
          calib_time_tmp = calib_time_tmp/DBLE(ncount)
          do k=1,ncount2
            ii = ii_store(k)
            jj = jj_store(k)
            rec_time(ii,jj) = calib_time_tmp
          end do
        end if
      end do
      end do

C     write(*,*)
C     do i=1,5
C       write(*,*) iorder(i,je),nfft1(i,je),nfft2(i,je),
C    &             nfft3(i,je),rec_time(i,je)
C     end do

c-----sum up adjusted rec_time and dir_time

      do j=js,je
        ttl_time_min = 1.0d100
C       write(*,*)
      do i=1,5
        ttl_time(i,j) = dir_time(i,j) + rec_time(i,j)

C       write(*,'(i5,2e12.4,4i5,3e15.7,i4)')j,ewaldcof(j),
C    &            cutoff(i,j),
C    &            iorder(i,j),nfft1(i,j),
C    &            nfft2(i,j),nfft3(i,j),
C    &            dir_time(i,j),rec_time(i,j),
C    &            ttl_time(i,j),i_best(j)

        if(ttl_time_min.gt.ttl_time(i,j)) then
          ttl_time_min = ttl_time(i,j)
          i_best(j) = i
        end if
      end do
C       write(8,'(i5,2e12.4,4i5,3e15.7)')j,ewaldcof(j),
C    &            cutoff(i_best(j),j),
C    &            iorder(i_best(j),j),nfft1(i_best(j),j),
C    &            nfft2(i_best(j),j),nfft3(i_best(j),j),
C    &            dir_time(i_best(j),j),rec_time(i_best(j),j),
C    &            ttl_time(i_best(j),j)
      end do

c-----find ewaldcof that needs least cpu time

      ttl_time_min = 1.0d100
      j_best_s = 0
      j_best_e = 0
      do j=js,je
        if(ttl_time_min.gt.ttl_time(i_best(j),j)) then
          ttl_time_min = ttl_time(i_best(j),j)
          j_best_s = j
          j_best_e = j
        else if(ttl_time_min.eq.ttl_time(i_best(j),j)) then
          j_best_e = j
        end if
      end do

      j_best = (j_best_s + j_best_e) / 2

      j_sign = 1
      do while(ttl_time(i_best(j_best),j_best).ne.ttl_time_min)
        j_best = j_best + j_sign
        j_sign = sign((abs(j_sign)+1),-j_sign)
      end do

C     write(*,*)
C     do j=j_best_s,j_best_e
C       write(*,'(i5,2e12.4,4i5,3e15.7)')j,ewaldcof(j),
C    &            cutoff(i_best(j),j),
C    &            iorder(i_best(j),j),nfft1(i_best(j),j),
C    &            nfft2(i_best(j),j),nfft3(i_best(j),j),
C    &            dir_time(i_best(j),j),rec_time(i_best(j),j),
C    &            ttl_time(i_best(j),j)
C     end do
C     write(*,*) 'j_best = ',j_best
C     write(*,*)
C     write(*,*) time_self


      cutoff_best = cutoff(i_best(j_best),j_best)
      ewaldcof_best = ewaldcof(j_best)
      iorder_best = iorder(i_best(j_best),j_best)
      nfft1_best = nfft1(i_best(j_best),j_best)
      nfft2_best = nfft2(i_best(j_best),j_best)
      nfft3_best = nfft3(i_best(j_best),j_best)
      tol_ene_best = tol_ene(i_best(j_best),j_best)
      tol_fx_best = tol_fx(i_best(j_best),j_best)
      tol_fy_best = tol_fy(i_best(j_best),j_best)
      tol_fz_best = tol_fz(i_best(j_best),j_best)
      tol_vir_best = tol_vir(i_best(j_best),j_best)
      time_best = (ttl_time(i_best(j_best),j_best) + time_self)

C     close(8)
C     close(10)
C     close(11)

      return
      end
c---------------------------------------------------------------------
      subroutine find_prmt(numatoms,x,y,z,cg,flg_minimg,box,
     $     recip,volume,
     $     tolerance,cutoff_min,cutoff_best,ewaldcof,iorder_best,
     $     nfft1_best,nfft2_best,nfft3_best,tol_ene_best,
     $     tol_fx_best,tol_fy_best,tol_fz_best,
     $     tol_vir_best,dir_time_best,rec_time_best,ttl_time_best,
     $     std_dir_time,std_rec_time,cutoff_std,nfft_std,prec_std,
     $     cutoff_ic,k1_ltd,k2_ltd,k3_ltd,
     $     iorder_choice,iorder_nchoice,
     $     nfft_choice,nfft_nchoice,Q,nerrors)
      implicit real*8(a-h,o-z)

      parameter (nrepeats=2,nrepeats_std_time=4,
     &           ltd_step=6)   ! to be equal or grater than 2
      dimension x(*),y(*),z(*),cg(*),flg_minimg(*),box(3),Q(*),
     $      iorder_choice(iorder_nchoice),nfft_choice(nfft_nchoice),
     $      cutoff_best(5),iorder_best(5),
     $      nfft1_best(5),nfft2_best(5),nfft3_best(5),tol_ene_best(5),
     $      tol_fx_best(5),tol_fy_best(5),tol_fz_best(5),
     $      tol_vir_best(5),dir_time_best(5),rec_time_best(5),
     $      ttl_time_best(5)
      dimension recip(3,3),mlimit(3),force(3,numatoms)
      dimension rfx_exact(numatoms),dfx_exact(numatoms),
     &          rfy_exact(numatoms),dfy_exact(numatoms),
     &          rfz_exact(numatoms),dfz_exact(numatoms),
     &          rvir_exact(6),dvir_exact(6)
      dimension rfx(numatoms),dfx(numatoms,5),
     &          rfy(numatoms),dfy(numatoms,5),
     &          rfz(numatoms),dfz(numatoms,5),
     &          rvir(6),dvir(6,5),dene(5)
      dimension dfx_tmp(numatoms),dfx_ic(numatoms),dfx_old(numatoms),
     &          dfy_tmp(numatoms),dfy_ic(numatoms),dfy_old(numatoms),
     &          dfz_tmp(numatoms),dfz_ic(numatoms),dfz_old(numatoms),
     &          dvir_tmp(6),dvir_ic(6),dvir_old(6)
      dimension fx_exact(numatoms),
     &          fy_exact(numatoms),
     &          fz_exact(numatoms),
     &          vir_exact(6)
      dimension fx(numatoms),
     &          fy(numatoms),
     &          fz(numatoms),
     &          vir(6)
      dimension dtol_exp(5)
      dimension flag(5),flag_old(5),flag_ic(5),dtor_exp(5)
      dimension cutoff(5),iorder(5,5),nfft1(5,5),nfft2(5,5),
     &          nfft3(5,5),rec_time(5,5),dir_time(5),ttl_time(25)
      dimension tolene_tmp(5),tolx_tmp(5),toly_tmp(5),tolz_tmp(5),
     &          tolvir_tmp(5),tolxyz(5),tolxyz_old(20)
      dimension tolene(5,5),tolx(5,5),toly(5,5),tolz(5,5),tolvir(5,5)
      dimension tolmax(0:nfft_nchoice*3,iorder_nchoice),
     &          time_trans(iorder_nchoice),
     &          k1_next(iorder_nchoice),k2_next(iorder_nchoice),
     &          k3_next(iorder_nchoice),i_flag(5,iorder_nchoice),
     &          i_flag_tover(5)
      dimension k1_ltd(iorder_nchoice,0:nfft_nchoice*3),
     &          k2_ltd(iorder_nchoice,0:nfft_nchoice*3),
     &          k3_ltd(iorder_nchoice,0:nfft_nchoice*3),
     &          mode_ltd(iorder_nchoice),ncount(iorder_nchoice)
      dimension min_time_array(25)
      dimension nerrors(3),dir_time_ratio(5),
     &          dir_time_cal(5),i_dir_time_cal(5)

      do i=1,5
        cutoff(i) = 1.0d100
        dir_time(i) = 1.0d100
        do j=1,5
          iorder(j,i) = 100
          nfft1(j,i) = 1000000
          nfft2(j,i) = 1000000
          nfft3(j,i) = 1000000
          rec_time(j,i) = 1.0d100
        end do
      end do

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
        go to 801
      end if
c----1.4 find exact force, potential and virial in reciprocal space
c     write(*,'(e12.4,2x,a)') ewaldcof,'1.4'
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
      call spme_self(cg,numatoms,self_ene,ewaldcof)
c----1.6 get exact direct sum
c     write(*,'(e12.4,2x,a)') ewaldcof,'1.6'
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
      ene_exact = rene_exact + self_ene + dene_exact

c--2. find real-space cutoff values for 5 tolerances (dtol_exp)
      do i=1,5
        dtol_exp(i) = tolerance-0.05d0*DBLE(i)
      end do
c----2.1 find cutoff that is supposed to satisfy dtol_exp(3)
c     write(*,'(e12.4,2x,a)') ewaldcof,'2.1'
      if(cutoff_ic.eq.0.0d0) then
c       tol_tmp = 10.0d0**(dtol_exp(5)-1.5d0)
        tol_tmp = 10.0d0**tolerance
        call find_cutoff(cutoff_ic,tol_tmp,ewaldcof)
        call next_value(cutoff_ic,0)
      end if
      cutoff_ic = max(cutoff_ic,cutoff_min)

  598 do i=1,5
        dir_time_ratio(i) = 0.0d0
      end do
      dir_time_sum = 0.0d0
      nerrors(1) = 0
  599 do i=1,5
        dir_time(i) = 1.0d100
      end do
c----2.2 get std_dir_time for calibration
c     write(*,'(e12.4,2x,a)') ewaldcof,'2.2'
      call get_std_dir_time(numatoms,x,y,z,cg,cutoff_std,box,
     $                     flg_minimg,std_dir_time1,nrepeats_std_time)
c----2.3 calculate dir sum to find resulted tolerance for given cutoff
c     write(*,'(e12.4,2x,a)') ewaldcof,'2.3'
      call find_dir_error(numatoms,x,y,z,cg,cutoff_ic,
     $                    ewaldcof,box,flg_minimg,
     $          dene_tmp,dfx_tmp,dfy_tmp,dfz_tmp,dvir_tmp,self_ene,
     $          rene_exact,rfx_exact,rfy_exact,rfz_exact,rvir_exact,
     $          ene_exact,fx_exact,fy_exact,fz_exact,vir_exact,
     &          tolene_tmp(1),tolx_tmp(1),toly_tmp(1),tolz_tmp(1),
     &          tolvir_tmp(1),time_tmp,nrepeats)
      tolxyz(1) = max(tolx_tmp(1),toly_tmp(1),tolz_tmp(1))
      do i=1,5
        flag(i) = tolxyz(1) - dtol_exp(i)
        flag_ic(i) = flag(i)
      end do
      dir_time_ic = time_tmp
      dene_ic = dene_tmp
!$omp parallel do
      do n=1,numatoms
        dfx_ic(n) = dfx_tmp(n)
        dfy_ic(n) = dfy_tmp(n)
        dfz_ic(n) = dfz_tmp(n)
      end do
      do n=1,6
        dvir_ic(n) = dvir_tmp(n)
      end do
C     write(*,'(i2,e12.4,e15.7,2f10.5)')
C    &                   0,cutoff_ic,dir_time_ic,dtol_exp(5),tolxyz(1)
c----2.4 try larger cutoffs to find proper one for every dtol_exp
c     write(*,'(e12.4,2x,a)') ewaldcof,'2.4'
      cutoff_tmp = cutoff_ic
      do i=1,20
        tolxyz_old(i) = 1.0d100
      end do
      ie = 0
      do while(flag(5).gt.0.0d0)
        call next_value(cutoff_tmp,1)
        if(cutoff_tmp.ge.cutoff_exact) then
C         print *,'examining cutoff_tmp is now .ge. cutoff_exact.'
          if(ie.eq.0 .and. flag(1).gt.0.0d0) then
C           print *,'impossible to achieve such precision! (dir1)'
C           print *,'try loosening the required tolerance.'
C           print *,ewaldcof,cutoff_tmp,tolxyz(1)
            go to 801
          else
C           print *,'impossible to achieve such precision '
C           print *,'for all 5 dtol_exp! (dir1)'
            go to 600
          end if
        end if
        call find_dir_error(numatoms,x,y,z,cg,cutoff_tmp,
     $                    ewaldcof,box,flg_minimg,
     $          dene_tmp,dfx_tmp,dfy_tmp,dfz_tmp,dvir_tmp,self_ene,
     $          rene_exact,rfx_exact,rfy_exact,rfz_exact,rvir_exact,
     $          ene_exact,fx_exact,fy_exact,fz_exact,vir_exact,
     &          tolene_tmp(1),tolx_tmp(1),toly_tmp(1),tolz_tmp(1),
     &          tolvir_tmp(1),time_tmp,nrepeats)
        do i=20,2,-1
          tolxyz_old(i) = tolxyz_old(i-1)
        end do
        tolxyz_old(1) = tolxyz(1)
        tolxyz(1) = max(tolx_tmp(1),toly_tmp(1),tolz_tmp(1))
C       print *,cutoff_tmp,tolxyz(1)
        tolxyz_old_min = 1.0d100
        n_tolxyz_old_min = 20
        do i=20,1,-1
          if(tolxyz_old(i).le.tolxyz_old_min) then
            tolxyz_old_min = tolxyz_old(i)
            n_tolxyz_old_min = i
          end if
        end do
        if(tolxyz(1).le.tolxyz_old_min) then
          tolxyz_old_min = tolxyz(1)
          n_tolxyz_old_min = 0
        end if
        if(n_tolxyz_old_min.eq.20) then
          if(ie.eq.0 .and. flag(1).gt.0.0d0) then
C           print *,'impossible to achieve such precision! (dir2)'
C           print *,'try loosening the required tolerance.'
C           print *,ewaldcof,cutoff_tmp,tolxyz(1)
            go to 801
          else
C           print *,'impossible to achieve such precision '
C           print *,'for all 5 dtol_exp! (dir2)'
            go to 600
          end if
        end if
        do i=ie+1,5
          flag_old(i) = flag(i)
          flag(i) = tolxyz(1) - dtol_exp(i)
          if(flag(i)*flag_old(i).lt.0.0d0) then
            cutoff(i) = cutoff_tmp
            dir_time(i) = time_tmp
            dene(i) = dene_tmp
!$omp       parallel do
            do n=1,numatoms
              dfx(n,i) = dfx_tmp(n)
              dfy(n,i) = dfy_tmp(n)
              dfz(n,i) = dfz_tmp(n)
            end do
            do n=1,6
              dvir(n,i) = dvir_tmp(n)
            end do
C           write(*,'(i2,e12.4,e15.7,2f10.5)')
C    &                   i,cutoff(i),dir_time(i),dtol_exp(i),tolxyz(1)
            ie = i
          end if
        end do
      end do
c----2.5 try smaller cutoffs to find proper one for every dtol_exp
c     write(*,'(e12.4,2x,a)') ewaldcof,'2.5'
  600 do i=1,5
        flag(i) = flag_ic(i)
      end do
      time_tmp = dir_time_ic
      dene_tmp = dene_ic
!$omp parallel do
      do n=1,numatoms
        dfx_tmp(n) = dfx_ic(n)
        dfy_tmp(n) = dfy_ic(n)
        dfz_tmp(n) = dfz_ic(n)
      end do
      do n=1,6
        dvir_tmp(n) = dvir_ic(n)
      end do

      cutoff_tmp = cutoff_ic
      do while(flag(1).lt.0.0d0)
        cutoff_old = cutoff_tmp
        time_old = time_tmp
        dene_old = dene_tmp
!$omp   parallel do
        do n=1,numatoms
          dfx_old(n) = dfx_tmp(n)
          dfy_old(n) = dfy_tmp(n)
          dfz_old(n) = dfz_tmp(n)
        end do
        do n=1,6
          dvir_old(n) = dvir_tmp(n)
        end do
        call next_value(cutoff_tmp,-1)
                      if(cutoff_tmp.lt.cutoff_min) then
                        do i=1,5
                          if(flag(i).lt.0.0d0) then
                            cutoff(i) = cutoff_old
                            dir_time(i) = time_old
                            dene(i) = dene_old
!$omp                       parallel do
                            do n=1,numatoms
                              dfx(n,i) = dfx_old(n)
                              dfy(n,i) = dfy_old(n)
                              dfz(n,i) = dfz_old(n)
                            end do
                            do n=1,6
                              dvir(n,i) = dvir_old(n)
                            end do
                            ie = max(ie,i)
C           write(*,'(i2,e12.4,e15.7,a)')
C    &         i,cutoff(i),dir_time(i),'  best cutoff .lt. cutoff_min'
                          end if
                        end do
                        go to 601
                      end if
        call find_dir_error(numatoms,x,y,z,cg,cutoff_tmp,
     $                    ewaldcof,box,flg_minimg,
     $          dene_tmp,dfx_tmp,dfy_tmp,dfz_tmp,dvir_tmp,self_ene,
     $          rene_exact,rfx_exact,rfy_exact,rfz_exact,rvir_exact,
     $          ene_exact,fx_exact,fy_exact,fz_exact,vir_exact,
     &          tolene_tmp(1),tolx_tmp(1),toly_tmp(1),tolz_tmp(1),
     &          tolvir_tmp(1),time_tmp,nrepeats)
        tolxyz(1) = max(tolx_tmp(1),toly_tmp(1),tolz_tmp(1))
C       print *,cutoff_tmp,tolxyz(1)
        do i=5,1,-1
          flag_old(i) = flag(i)
          flag(i) = tolxyz(1) - dtol_exp(i)
          if(flag(i)*flag_old(i).lt.0.0d0) then
            cutoff(i) = cutoff_old
            dir_time(i) = time_old
            dene(i) = dene_old
!$omp       parallel do
            do n=1,numatoms
              dfx(n,i) = dfx_old(n)
              dfy(n,i) = dfy_old(n)
              dfz(n,i) = dfz_old(n)
            end do
            do n=1,6
              dvir(n,i) = dvir_old(n)
            end do
            ie = max(ie,i)
C           write(*,'(i2,e12.4,e15.7,2f10.5)')
C    &                   i,cutoff(i),dir_time(i),dtol_exp(i),tolxyz(1)
          end if
        end do
      end do
c----2.6 check if 'the smaller cutoff, the smaller dir_time'
c     write(*,'(e12.4,2x,a)') ewaldcof,'2.6'
  601 do i=1,ie-1
        if(dir_time(i).gt.dir_time(i+1)) then
C         print *,'dir_time(',i,') is grater than dir_time(',i+1,')'
C         print *,'try to get 5 best cutoffs again.'
          do ii=1,ie
            dir_time_ratio(ii) = dir_time_ratio(ii) 
     &                         + dir_time(ii) / dir_time(1)
          end do
          dir_time_sum = dir_time_sum + dir_time(1)
          nerrors(1) = nerrors(1) + 1
          if(nerrors(1).eq.10 .and. nerrors(2).lt.3) then
            print *,'WARNING: The executing host seems currently ',
     &                                                     'unstable,'
            print *,'         and it''s taking longer time than is ',
     &                                                     'expected.'
          else if(nerrors(1).eq.30) then
C           write(*,*)
C           write(*,*) 'dir_time calibration forced to take place.'
C           write(*,*) 'nerrors(1) = ',nerrors(1),
C    &                 'nerrors(2) = ',nerrors(2)
            do ii=1,ie
              dir_time_ratio(ii) = dir_time_ratio(ii) * dir_time_sum
     &                           / DBLE(nerrors(1)**2)
            end do

            ical=1
            dir_time_cal(ical) = dir_time_ratio(1)
            do ii=2,ie
              if(cutoff(ii).ne.cutoff(ii-1)) then
                ical = ical + 1
                dir_time_cal(ical) = dir_time_ratio(ii)
                ical_end = ical
              end if
            end do

            call bubble_sort(ical_end,dir_time_cal,i_dir_time_cal)

            ical=1
            dir_time(1) = dir_time_cal(i_dir_time_cal(1))
            do ii=2,ie
              if(cutoff(ii).ne.cutoff(ii-1)) ical = ical + 1
              dir_time(ii) = dir_time_cal(i_dir_time_cal(ical))
            end do
            if(ical.ne.ical_end) then
              print *,'forced calibration error',ical,ical_end
              do ii=1,ie
                write(*,*) ii,dir_time_ratio(ii),dir_time(ii)
              end do
              write(*,*)
              do ical=1,5
                write(*,*) ical,dir_time_cal(ical)
              end do
              write(*,*)
              stop
            end if

C           do ii=1,ie
C             write(*,'(i3,3e12.4)') ii,cutoff(ii),
C    &                   dir_time_ratio(ii),dir_time(ii)
C           end do
C           write(*,*)

            go to 602
          end if
          if(mod(nerrors(1),2).eq.1) then
            cutoff_ic = cutoff(1)
            call next_value(cutoff_ic,-1)
          else
            cutoff_ic = cutoff(ie)
            call next_value(cutoff_ic,1)
          end if
          cutoff_ic = max(cutoff_ic,cutoff_min)
          go to 599
        end if
      end do
c----2.7 get std_dir_time once again for calibration
c     write(*,'(e12.4,2x,a)') ewaldcof,'2.7'
  602 call get_std_dir_time(numatoms,x,y,z,cg,cutoff_std,box,
     $                     flg_minimg,std_dir_time2,nrepeats_std_time)
      if( (std_dir_time1/std_dir_time2).gt.prec_std .or.
     &    (std_dir_time2/std_dir_time1).gt.prec_std ) then
C       print *,'std_dir_time1 and std_dir_time2 differ too much.'
C       print *,'try to get 5 best cutoffs again.'
C       print *,std_dir_time1,std_dir_time2
        nerrors(2) = nerrors(2) + 1
        if(nerrors(2).eq.3) then
          print *,'WARNING: The executing host seems currently ',
     &                                                     'unstable,'
          print *,'         and it''s taking longer time than is ',
     &                                                     'expected.'
        end if
        cutoff_ic = cutoff(1)
        call next_value(cutoff_ic,-1)
        cutoff_ic = max(cutoff_ic,cutoff_min)
        go to 598
      end if
c----2.8 calibration of dir_time
c     write(*,'(e12.4,2x,a)') ewaldcof,'2.8'
      do i=1,ie
        dir_time(i) = dir_time(i) * std_dir_time
     &              / ((std_dir_time1+std_dir_time2)*0.5d0)
C       print *,i,dir_time(i)
      end do
C     print *,'dir_time calibration'
C     print *,std_dir_time,(std_dir_time1+std_dir_time2)*0.5d0
      cutoff_ic = cutoff(1)

c--3. find best iorder and nfft for 5 pairs of dtol_exp & cutoff
c----assume that limited mode applies for all iorders at first.
c----in limited search mode, k1, k2, and k3 values are limited.
c     write(*,'(e12.4,2x,a)') ewaldcof,'3'
  700 do i=1,5
      do j=1,5
        rec_time(j,i) = 1.0d100
      end do
      end do
!$omp parallel do
      do k=1,iorder_nchoice
        mode_ltd(k) = 1
        ncount(k) = 0
        time_trans(k) = 0.0d0
        do i=1,5
          i_flag(i,k) = 0
        end do
        do i=0,nfft_nchoice*3
          tolmax(i,k) = 1.0d100
        end do
        if(k1_ltd(k,0).eq.0) then
          k1_ltd(k,0) = 1
          do while(nfft_choice(k1_ltd(k,0)).le.iorder_choice(k))
            k1_ltd(k,0) = k1_ltd(k,0) + 1
          end do
          k2_ltd(k,0) = k1_ltd(k,0)
          k3_ltd(k,0) = k1_ltd(k,0)
        end if
      end do

c----- get std_rec_time for calibration
      call get_std_rec_time(numatoms,x,y,z,cg,recip,volume,
     $                 nfft_std,box,std_rec_time1,Q,nrepeats_std_time)

      k0 = 1
      k1 = k1_ltd(k0,ncount(k0)/ltd_step)
      k2 = k2_ltd(k0,ncount(k0)/ltd_step)
      k3 = k3_ltd(k0,ncount(k0)/ltd_step)
      i_flag_sum = 0
      do while(i_flag_sum.eq.0)
c-----if parameters are all set for 5cutoffs for the next k0, break!
        i_flag_k0_sum = 1
        do i=1,ie
          i_flag_k0_sum = i_flag_k0_sum * i_flag(i,k0)
        end do
        if(i_flag_k0_sum.ne.0) then
C         print *,''
C         print *,'next iorder to be examined is',iorder_choice(k0)
C         print *,'but 5 parameters are already determined for'
C         print *,'this iorder. stop searching.'
C         print *,''
          go to 705
        end if
        iorder_tmp = iorder_choice(k0)
        nfft1_tmp = nfft_choice(k1)
        nfft2_tmp = nfft_choice(k2)
        nfft3_tmp = nfft_choice(k3)
        call find_rec_error(numatoms,x,y,z,cg,recip,volume,ewaldcof,
     $          k1,k2,k3,iorder_tmp,nfft1_tmp,nfft2_tmp,nfft3_tmp,
     $          self_ene,dene,dfx,dfy,dfz,dvir,
     $          dene_exact,dfx_exact,dfy_exact,dfz_exact,dvir_exact,
     $          ene_exact,fx_exact,fy_exact,fz_exact,vir_exact,
     &          tolene_tmp,tolx_tmp,toly_tmp,tolz_tmp,tolvir_tmp,
     &          time_tmp,nrepeats,tolmax(0,k0),nfft_nchoice,
     &          ncount(k0),Q)

C       write(*,*)
C       write(*,'(4i5,e15.7,i4)')iorder_tmp,nfft1_tmp,nfft2_tmp,
C    &                  nfft3_tmp,time_tmp,ncount(k0)

        do i=1,ie
          tolxyz(i) = max(tolx_tmp(i),toly_tmp(i),tolz_tmp(i))
        end do
C       write(*,'(5f10.5)') (tolxyz(i),i=1,ie)

        if(mode_ltd(k0).eq.1) then
        do n = ncount(k0)+1,ncount(k0)+(ltd_step-1)
          tolmax(n,k0) = tolmax(ncount(k0),k0)
        end do
        do i=1,ie
          if(tolxyz(i).lt.tolerance) then   ! if precision satisfies 
     &                                      ! required value
            mode_ltd(k0) = 0                ! exit from the ltd mode
            if(ncount(k0).ne.0) then        ! if not 1st cycle 
     &                                      ! for this iorder
              ncount(k0) = ncount(k0) - (ltd_step-1)
              k1 = k1_next(k0)
              k2 = k2_next(k0)
              k3 = k3_next(k0)
C             write(*,'(a)') 'overran. exit limited mode.'
              go to 704
            end if
          end if
        end do
        end if

        do i=ie,1,-1

          if(i_flag(i,k0).ne.0) go to 701  !if parameters have already
     &                   ! set for the i and iorder with smaller nffts
          if(rec_time(5,i).le.time_tmp) then
            i_flag(i,k0) = 1
            i_flag_tover(i) = 1
C           write(*,*) 'time over',iorder_tmp,i
            go to 701
          else
            i_flag_tover(i) = 0
          end if

          if(tolxyz(i).lt.tolerance) then   ! if precision satisfies 
     &                                      ! required value
            jj = 5
            do j = 4,1,-1
            if(rec_time(j,i).gt.time_tmp) then
              iorder(j+1,i) = iorder(j,i)
              nfft1(j+1,i) = nfft1(j,i)
              nfft2(j+1,i) = nfft2(j,i)
              nfft3(j+1,i) = nfft3(j,i)
              tolene(j+1,i) = tolene(j,i)
              tolx(j+1,i) = tolx(j,i)
              toly(j+1,i) = toly(j,i)
              tolz(j+1,i) = tolz(j,i)
              tolvir(j+1,i) = tolvir(j,i)
              rec_time(j+1,i) = rec_time(j,i)
              jj = j
            end if
            end do
            iorder(jj,i) = iorder_tmp
            nfft1(jj,i) = nfft1_tmp
            nfft2(jj,i) = nfft2_tmp
            nfft3(jj,i) = nfft3_tmp
            tolene(jj,i) = tolene_tmp(i)
            tolx(jj,i) = tolx_tmp(i)
            toly(jj,i) = toly_tmp(i)
            tolz(jj,i) = tolz_tmp(i)
            tolvir(jj,i) = tolvir_tmp(i)
            rec_time(jj,i) = time_tmp
C           write(*,'(a,i3,4i5,e15.7,f10.5)')
C    &         'found rec',i,iorder_tmp,nfft1_tmp,nfft2_tmp,
C    &                     nfft3_tmp,time_tmp,tolxyz(i)
          end if

  701   end do
c-----see if 'time over' for all the 5 cases for the smallest nfft 
c-----with an iorder, if yes : i_flag_tover_sum = 1
        if(ncount(k0).eq.0) then
          i_flag_tover_sum = 1
          do i=1,ie
            i_flag_tover_sum = i_flag_tover_sum * i_flag_tover(i)
          end do
          if(i_flag_tover_sum.ne.0) then
            do k=k0,iorder_nchoice
              do i=1,5
                i_flag(i,k) = 1
              end do
              time_trans(k) = 1.0d100
            end do
C           print *,'time over for all the 5 cases with the smallest'
C           print *,'nfft for this iorder. skip trying larger iorders.'
            go to 702
          end if
        end if
c-----see if next nfft is not over 1024
        if(max(k1,k2,k3).gt.nfft_nchoice) then 
C         print *,'impossible to achieve the precision required'
C         print *,'with the given ewaldcof and iorder'
C         write(*,'(a,e12.4,2x,a,i8)') 'ewaldcof = ',ewaldcof,
C    &                                'iorder = ', iorder_tmp
          do i=1,5
            i_flag(i,k0) = 1
          end do
          time_trans(k0) = 1.0d100
        else
          time_trans(k0) = time_tmp
          k1_next(k0) = k1
          k2_next(k0) = k2
          k3_next(k0) = k3
c-----renew 'ncount' and save k_ltd once in 'ltd_step' ncounts
          if(mode_ltd(k0).eq.0) then
            ncount(k0) = ncount(k0) + 1
            if(mod(ncount(k0),ltd_step).eq.0) then
              k1_ltd(k0,ncount(k0)/ltd_step) = k1
              k2_ltd(k0,ncount(k0)/ltd_step) = k2
              k3_ltd(k0,ncount(k0)/ltd_step) = k3
            end if
          else
            ncount(k0) = ncount(k0) + ltd_step
          end if
        end if
c-----determine k0 for the next step
  702   i_flag_local = 0
        time_trans_min = 1.0d100
        do k=1,iorder_nchoice
          if(time_trans(k).eq.0.0d0) then
            k0 = k
            go to 703
          else if(time_trans(k).lt.time_trans_min) then
            time_trans_min = time_trans(k)
            k0 = k
            i_flag_local = 1
          end if
        end do
c-----if time_trans(k) is all 1.0d100
        if(i_flag_local.ne.1) go to 801

  703   k1 = k1_next(k0)
        k2 = k2_next(k0)
        k3 = k3_next(k0)
        if(mode_ltd(k0).eq.1) then
          if(k1_ltd(k0,ncount(k0)/ltd_step).ne.0) then
            k1 = k1_ltd(k0,ncount(k0)/ltd_step)
            k2 = k2_ltd(k0,ncount(k0)/ltd_step)
            k3 = k3_ltd(k0,ncount(k0)/ltd_step)
          else
            mode_ltd(k0) = 0
            ncount(k0) = ncount(k0) - (ltd_step-1)
C           print *,'next limited nffts not available.'
C           print *,'exit limited mode. iorder = ',iorder_choice(k0)
          end if
        end if

        i_flag_sum = 1
        do k=1,iorder_nchoice
        do i=1,ie
          i_flag_sum = i_flag_sum * i_flag(i,k)
        end do
        end do

  704 end do

c----get std_rec_time once again for calibration
  705 call get_std_rec_time(numatoms,x,y,z,cg,recip,volume,
     $                 nfft_std,box,std_rec_time2,Q,nrepeats_std_time)
      if( (std_rec_time1/std_rec_time2).gt.prec_std .or.
     &    (std_rec_time2/std_rec_time1).gt.prec_std ) then
C       print *,'std_rec_time1 and std_rec_time2 differ too much.'
C       print *,'try to get iorders and nffts again.'
C       print *,std_rec_time1,std_rec_time2
        nerrors(3) = nerrors(3) + 1
        if(nerrors(3).eq.3) then
          print *,'WARNING: The executing host seems currently ',
     &                                                     'unstable,'
          print *,'         and it''s taking longer time than is ',
     &                                                     'expected.'
        end if
        go to 700
      end if
c---- calibration of rec_time
      do i=1,ie
      do j=1,5
        rec_time(j,i) = rec_time(j,i) * std_rec_time
     &              / ((std_rec_time1+std_rec_time2)*0.5d0)
      end do
      end do
C     print *,'rec_time calibration'
C     print *,std_rec_time,(std_rec_time1+std_rec_time2)*0.5d0

c--4. summarize
  801 continue
c     write(*,'(e12.4,2x,a)') ewaldcof,'4'
C     write(*,*)'-----------------------------------'
C     do i=1,5
C       do j=1,5
C         write(*,'(2i4,e12.4,4i5,3e13.5)')
C    &         i,j,cutoff(i),iorder(j,i),nfft1(j,i),
C    &          nfft2(j,i),nfft3(j,i),dir_time(i),rec_time(j,i),
C    &          dir_time(i)+rec_time(j,i)
C       end do
C       write(*,*)
C     end do
C     write(*,*)'-----------------------------------'

      do i=1,5
      do j=1,5
        ttl_time((i-1)*5+j) = dir_time(i) + rec_time(j,i)
      end do
      end do
      call bubble_sort(25,ttl_time,min_time_array)

      do l=1,5
        cutoff_best(l) = 1.0d100
        iorder_best(l) = 100
        nfft1_best(l) = 1000000
        nfft2_best(l) = 1000000
        nfft3_best(l) = 1000000
        dir_time_best(l) = 1.0d100
        rec_time_best(l) = 1.0d100
        ttl_time_best(l) = dir_time_best(l)+rec_time_best(l)
      end do

      l=1
      n=1
      k = min_time_array(n)
      i=(k-1)/5+1
      j=k-(i-1)*5
           cutoff_best(l) = cutoff(i)
           iorder_best(l) = iorder(j,i)
           nfft1_best(l) = nfft1(j,i)
           nfft2_best(l) = nfft2(j,i)
           nfft3_best(l) = nfft3(j,i)
           tol_ene_best(l) = tolene(j,i)
           tol_fx_best(l) = tolx(j,i)
           tol_fy_best(l) = toly(j,i)
           tol_fz_best(l) = tolz(j,i)
           tol_vir_best(l) = tolvir(j,i)
           dir_time_best(l) = dir_time(i)
           rec_time_best(l) = rec_time(j,i)
           ttl_time_best(l) = dir_time(i)+rec_time(j,i)
      l = l + 1
      n = n + 1

      do while(l.le.5 .and. n.le.25)
        k = min_time_array(n)
        i=(k-1)/5+1
        j=k-(i-1)*5
        if(cutoff_best(l-1) .ne. cutoff(i) .or.
     &     iorder_best(l-1) .ne. iorder(j,i) .or.
     &     nfft1_best(l-1) .ne. nfft1(j,i) .or.
     &     nfft2_best(l-1) .ne. nfft2(j,i) .or.
     &     nfft3_best(l-1) .ne. nfft3(j,i) ) then
             cutoff_best(l) = cutoff(i)
             iorder_best(l) = iorder(j,i)
             nfft1_best(l) = nfft1(j,i)
             nfft2_best(l) = nfft2(j,i)
             nfft3_best(l) = nfft3(j,i)
             tol_ene_best(l) = tolene(j,i)
             tol_fx_best(l) = tolx(j,i)
             tol_fy_best(l) = toly(j,i)
             tol_fz_best(l) = tolz(j,i)
             tol_vir_best(l) = tolvir(j,i)
             dir_time_best(l) = dir_time(i)
             rec_time_best(l) = rec_time(j,i)
             ttl_time_best(l) = dir_time(i)+rec_time(j,i)
             l = l + 1
        end if
        n = n + 1
      end do

C     do l=1,5
C         write(*,'(e12.4,4i5,e15.7)')
C    &         cutoff_best(l),iorder_best(l),nfft1_best(l),
C    &         nfft2_best(l),nfft3_best(l),
C    &         dir_time_best(l)+rec_time_best(l)
C     end do

C     write(*,*)

      return
      end
c---------------------------------------------------------------------
      subroutine find_dir_error(numatoms,x,y,z,cg,cutoff,
     $                    ewaldcof,box,flg_minimg,
     $          dene,dfx,dfy,dfz,dvir,self_ene,
     $          rene_exact,rfx_exact,rfy_exact,rfz_exact,rvir_exact,
     $          ene_exact,fx_exact,fy_exact,fz_exact,vir_exact,
     &               tolene,tolx,toly,tolz,tolvir,time,nrepeats)
      implicit real*8(a-h,o-z)

      dimension x(*),y(*),z(*),cg(*),flg_minimg(*),box(3)
      dimension rfx_exact(numatoms),
     &          rfy_exact(numatoms),
     &          rfz_exact(numatoms),
     &          rvir_exact(6)
      dimension dfx(numatoms),
     &          dfy(numatoms),
     &          dfz(numatoms),
     &          dvir(6)
      dimension fx_exact(numatoms),
     &          fy_exact(numatoms),
     &          fz_exact(numatoms),
     &          vir_exact(6)
      dimension fx(numatoms),
     &          fy(numatoms),
     &          fz(numatoms),
     &          vir(6)
      real*4    etime,tm(2)

      n = 0
      t1 = etime(tm)
      t2 = 0.0d0
c     do while((t2-t1).lt.DBLE(nrepeats)*0.5d0)
      do while((t2-t1).lt.0.2d0)
        do i=1,nrepeats
          call spme_direct(numatoms,x,y,z,cg,cutoff,ewaldcof,box,
     $        dene,dfx,dfy,dfz,dvir,flg_minimg)
        end do
        n = n + 1
        t2 = etime(tm)
      end do
      time = (t2 - t1) / DBLE(n) / DBLE(nrepeats)
!$omp parallel do
      do i=1,numatoms
        fx(i) = rfx_exact(i) + dfx(i)
        fy(i) = rfy_exact(i) + dfy(i)
        fz(i) = rfz_exact(i) + dfz(i)
      end do
      do i=1,6
        vir(i) = rvir_exact(i) + dvir(i)
      end do
      ene = rene_exact + self_ene + dene
      call comp_pme(numatoms,ene,fx,fy,fz,vir,
     &          ene_exact,fx_exact,fy_exact,fz_exact,vir_exact,
     &          tolene,tolx,toly,tolz,tolvir)
      return
      end
