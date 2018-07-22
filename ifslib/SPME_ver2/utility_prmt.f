c---------------------------------------------------------------------
      subroutine get_std_dir_time(numatoms,x,y,z,cg,cutoff,box,
     $                            flg_minimg,std_time,nrepeats)

      implicit real*8(a-h,o-z)
      dimension x(*),y(*),z(*),cg(*),flg_minimg(*),box(3),
     $          fx(numatoms),fy(numatoms),fz(numatoms),vir(6)
      real*4    etime,tm(2)

      ewaldcof = 3.0d0 / ( box(1) + box(2) + box(3) )

      t1 = etime(tm)
      do j=1,nrepeats
        call spme_direct(numatoms,x,y,z,cg,cutoff,ewaldcof,box,
     $              ene,fx,fy,fz,vir,flg_minimg)
      end do
      t2 = etime(tm)

      std_time = (t2 - t1) / DBLE(nrepeats)

      return
      end
c---------------------------------------------------------------------
      subroutine get_std_rec_time(numatoms,x,y,z,cg,recip,volume,
     $                            nfft,box,std_time,Q,nrepeats)

      implicit real*8(a-h,o-z)
      parameter(iorder=10)
      dimension x(*),y(*),z(*),cg(*),box(3),
     $          fx(numatoms),fy(numatoms),fz(numatoms),vir(6)
      dimension recip(3,3)
      dimension bsp_mod(max(iorder+1,nfft),3)
      dimension fftable(2*(max(iorder+1,nfft)*3)+256*3)
      real*4    etime,tm(2)

      ewaldcof = 3.0d0 / ( box(1) + box(2) + box(3) )

      nfft = max(iorder+1,nfft)

      call spme_init(numatoms,iorder,nfft,nfft,nfft,
     $               bsp_mod,fftable)

      t1 = etime(tm)
      do j=1,nrepeats
        call spme_recip(
     $       numatoms,x,y,z,cg,recip,volume,
     $       ene,fx,fy,fz,vir,
     $       ewaldcof,iorder,nfft,nfft,nfft,
     $       bsp_mod,fftable,Q)
      end do
      t2 = etime(tm)

      std_time = (t2 - t1) / DBLE(nrepeats)

      return
      end
c---------------------------------------------------------------------
      subroutine comp_pme(numatoms,ene,fx,fy,fz,vir,
     &          ene0,fx0,fy0,fz0,vir0,
     &          tolene,tolx,toly,tolz,tolvir)
      implicit real*8(a-h,o-z)

      dimension fx(numatoms),
     &          fy(numatoms),
     &          fz(numatoms),
     &          vir(6)
      dimension fx0(numatoms),
     &          fy0(numatoms),
     &          fz0(numatoms),
     &          vir0(6)

      tmp_fx = 0.0d0
      aax_fx = -1.0d100
      ain_fx = 1.0d100
      ave_fx = 0.0d0
      tmp_fy = 0.0d0
      aax_fy = -1.0d100
      ain_fy = 1.0d100
      ave_fy = 0.0d0
      tmp_fz = 0.0d0
      aax_fz = -1.0d100
      ain_fz = 1.0d100
      ave_fz = 0.0d0

!$omp parallel do
!$omp&private(tmp_fx,tmp_fy,tmp_fz),
!$omp&reduction(+:ave_fx),reduction(+:ave_fy),reduction(+:ave_fz)
      do j=1,numatoms
        tmp_fx = log10(abs((fx(j)-fx0(j))/fx0(j)))
        tmp_fx = max(tmp_fx,-15.95d0)
c       aax_fx = max(aax_fx,tmp_fx)
c       ain_fx = min(ain_fx,tmp_fx)
        ave_fx = ave_fx + tmp_fx

        tmp_fy = log10(abs((fy(j)-fy0(j))/fy0(j)))
        tmp_fy = max(tmp_fy,-15.95d0)
c       aax_fy = max(aax_fy,tmp_fy)
c       ain_fy = min(ain_fy,tmp_fy)
        ave_fy = ave_fy + tmp_fy

        tmp_fz = log10(abs((fz(j)-fz0(j))/fz0(j)))
        tmp_fz = max(tmp_fz,-15.95d0)
c       aax_fz = max(aax_fz,tmp_fz)
c       ain_fz = min(ain_fz,tmp_fz)
        ave_fz = ave_fz + tmp_fz
      end do
c
      tmp_vir = 0.0d0
c     aax_vir = -1.0d100
c     ain_vir = 1.0d100
      ave_vir = 0.0d0

      do j=1,6
        tmp_vir = log10(abs((vir(j)-vir0(j))/vir0(j)))
        tmp_vir = max(tmp_vir,-15.95d0)
c       aax_vir = max(aax_vir,tmp_vir)
c       ain_vir = min(ain_vir,tmp_vir)
        ave_vir = ave_vir + tmp_vir
      end do
c     j=1
c       tmp_vir = log10(abs((vir(j)-vir0(j))/vir0(j)))
c       tmp_vir = max(tmp_vir,-15.95d0)
c       ave_vir = ave_vir + tmp_vir
c     j=4
c       tmp_vir = log10(abs((vir(j)-vir0(j))/vir0(j)))
c       tmp_vir = max(tmp_vir,-15.95d0)
c       ave_vir = ave_vir + tmp_vir
c     j=6
c       tmp_vir = log10(abs((vir(j)-vir0(j))/vir0(j)))
c       tmp_vir = max(tmp_vir,-15.95d0)
c       ave_vir = ave_vir + tmp_vir

      tmp_ene = log10(abs((ene-ene0)/ene0))
      tmp_ene = max(tmp_ene,-15.95d0)
c     aax_ene = tmp_ene
c     ain_ene = tmp_ene
      ave_ene = tmp_ene

      tolene = ave_ene
      tolx = ave_fx/DBLE(numatoms)
      toly = ave_fy/DBLE(numatoms)
      tolz = ave_fz/DBLE(numatoms)
      tolvir = ave_vir/6.0d0
c     tolvir = ave_vir/3.0d0

      return
      end
c---------------------------------------------------------------------
      subroutine find_rec_error(numatoms,x,y,z,cg,recip,volume,
     $          ewaldcof,k1,k2,k3,iorder,nfft1,nfft2,nfft3,self_ene,
     $          dene,dfx,dfy,dfz,dvir,
     $          dene_exact,dfx_exact,dfy_exact,dfz_exact,dvir_exact,
     $          ene_exact,fx_exact,fy_exact,fz_exact,vir_exact,
     &          tolene,tolx,toly,tolz,tolvir,time,nrepeats,
     &          tolmax,nfft_nchoice,ncount,Q)
      implicit real*8(a-h,o-z)

      dimension x(*),y(*),z(*),cg(*),recip(3,3),Q(*)
      dimension dfx_exact(numatoms),
     &          dfy_exact(numatoms),
     &          dfz_exact(numatoms),
     &          dvir_exact(6)
      dimension dfx(numatoms,5),rfx(numatoms),
     &          dfy(numatoms,5),rfy(numatoms),
     &          dfz(numatoms,5),rfz(numatoms),
     &          dvir(6,5),dene(5),rvir(6)
      dimension fx_exact(numatoms),
     &          fy_exact(numatoms),
     &          fz_exact(numatoms),
     &          vir_exact(6)
      dimension fx(numatoms),
     &          fy(numatoms),
     &          fz(numatoms),
     &          vir(6)
      dimension tolene(5),tolx(5),toly(5),tolz(5),tolvir(5)
      dimension bsp_mod(max(nfft1,nfft2,nfft3),3)
      dimension fftable(2*(nfft1+nfft2+nfft3)+256*3)
      dimension tolmax(0:nfft_nchoice*3)
      real*4    etime,tm(2)

      call spme_init(numatoms,iorder,nfft1,nfft2,nfft3,
     $               bsp_mod,fftable)

      n = 0
      t1 = etime(tm)
      t2 = 0.0d0
c     do while((t2-t1).lt.DBLE(nrepeats)*0.5d0)
      do while((t2-t1).lt.0.2d0)
        do i=1,nrepeats
        call spme_recip(
     $     numatoms,x,y,z,cg,recip,volume,
     $     rene,rfx,rfy,rfz,rvir,
     $     ewaldcof,iorder,nfft1,nfft2,nfft3,
     $     bsp_mod,fftable,Q)
        end do
        n = n + 1
        t2 = etime(tm)
      end do
      time = (t2 - t1) / DBLE(n) / DBLE(nrepeats)

      call rms_correction(numatoms,rfx,rfy,rfz)

!$omp parallel do
      do i=1,numatoms
        fx(i) = rfx(i) + dfx_exact(i)
        fy(i) = rfy(i) + dfy_exact(i)
        fz(i) = rfz(i) + dfz_exact(i)
      end do
      do i=1,6
        vir(i) = rvir(i) + dvir_exact(i)
      end do
      ene = rene + self_ene + dene_exact
      call comp_pme(numatoms,ene,fx,fy,fz,vir,
     &          ene_exact,fx_exact,fy_exact,fz_exact,vir_exact,
     &          tolene(1),tolx(1),toly(1),tolz(1),tolvir(1))
      tolmax(ncount) = max(tolx(1),toly(1),tolz(1))
c     print *,tolx(1),toly(1),tolz(1)
      if(tolmax(ncount).eq.tolx(1)) k1 = k1 + 1
      if(tolmax(ncount).eq.toly(1)) k2 = k2 + 1
      if(tolmax(ncount).eq.tolz(1)) k3 = k3 + 1
      if(ncount.ge.10) then
        if(tolmax(ncount).ge.tolmax(ncount-10)) then
          k1 = 1000000
        end if
      end if

      do n=1,5
!$omp parallel do
      do i=1,numatoms
        fx(i) = rfx(i) + dfx(i,n)
        fy(i) = rfy(i) + dfy(i,n)
        fz(i) = rfz(i) + dfz(i,n)
      end do
      do i=1,6
        vir(i) = rvir(i) + dvir(i,n)
      end do
      ene = rene + self_ene + dene(n)
      call comp_pme(numatoms,ene,fx,fy,fz,vir,
     &          ene_exact,fx_exact,fy_exact,fz_exact,vir_exact,
     &          tolene(n),tolx(n),toly(n),tolz(n),tolvir(n))
      end do

      return
      end
c---------------------------------------------------------------------
      subroutine rms_correction(numatoms,fx,fy,fz)
      implicit real*8(a-h,o-z)

      dimension fx(numatoms),fy(numatoms),fz(numatoms)

      fx_cor = 0.0d0
      fy_cor = 0.0d0
      fz_cor = 0.0d0
!$omp parallel do
!$omp&reduction(+:fx_cor),reduction(+:fy_cor),reduction(+:fz_cor)
      do i=1,numatoms
        fx_cor = fx_cor + fx(i)
        fy_cor = fy_cor + fy(i)
        fz_cor = fz_cor + fz(i)
      end do
      fx_cor = fx_cor / DBLE(numatoms)
      fy_cor = fy_cor / DBLE(numatoms)
      fz_cor = fz_cor / DBLE(numatoms)
!$omp parallel do
      do i=1,numatoms
        fx(i) = fx(i) - fx_cor
        fy(i) = fy(i) - fy_cor
        fz(i) = fz(i) - fz_cor
      end do

      return
      end
c------------------------------------------------------------------
      subroutine find_ewaldcof(cutoff,dtol,ewaldcof)
      implicit none
      double precision cutoff,dtol,ewaldcof

      integer i,n
      double precision pi,term,x,xlo,xhi,y,erfc

c first get direct sum tolerance. How big must ewaldcof be to get
c terms outside the cutoff below tol
      pi = 3.14159265358979323846d0

      x = 0.5d0
      i = 0
10    x = 2.d0 * x
      i = i + 1
      y = x*cutoff
      term = erfc(y)
c     term = erfc(y)/cutoff
      if ( term .ge. dtol)goto 10
c binary search tolerance is 2 to the -60th
      n = i + 60
      xlo = 0.d0
      xhi = x
      do 20 i = 1,n
        x = (xlo+xhi)/2
        y = x*cutoff
        term = erfc(y)
c       term = erfc(y)/cutoff
        if ( term .ge. dtol )then
           xlo = x
        else 
           xhi = x
        endif
20    continue
      ewaldcof = x

      return
      end
c----------------------------------------------
      subroutine find_cutoff(cutoff,dtol,ewaldcof)
      implicit none
      double precision cutoff,dtol,ewaldcof

      integer i,n
      double precision pi,term,x,xlo,xhi,y,erfc

      pi = 3.14159265358979323846d0

      x = 0.5d0
      i = 0
10    x = 2.d0 * x
      i = i + 1
      y = x*ewaldcof
      term = erfc(y)
c     term = erfc(y)/x
      if ( term .ge. dtol)goto 10
c binary search tolerance is 2 to the -60th
      n = i + 60
      xlo = 0.d0
      xhi = x
      do 20 i = 1,n
        x = (xlo+xhi)/2
        y = x*ewaldcof
        term = erfc(y)
c       term = erfc(y)/x
        if ( term .ge. dtol )then
           xlo = x
        else 
           xhi = x
        endif
20    continue
      cutoff = x

      return
      end
c------------------------------------------------------
      subroutine find_maxexp(ewaldcof,rtol,maxexp)
      implicit none
      double precision ewaldcof,rtol,maxexp

      integer i,n
      double precision pi,term,x,xlo,xhi,y,erfc

      pi = 3.14159265358979323846d0
      x = 0.5d0
      i = 0
30    x = 2.d0 * x
      i = i + 1
      y = pi*x/ewaldcof
      term = 2.d0         *erfc(y)/sqrt(pi)
c     term = 2.d0*ewaldcof*erfc(y)/sqrt(pi)
      if ( term .ge. rtol)goto 30
c binary search tolerance is 2 to the -60th
      n = i + 60
      xlo = 0.d0
      xhi = x
      do 40 i = 1,n
        x = (xlo+xhi)/2
        y = pi*x/ewaldcof
        term = 2.d0         *erfc(y)/sqrt(pi)
c       term = 2.d0*ewaldcof*erfc(y)/sqrt(pi)
        if ( term .gt. rtol )then
           xlo = x
        else 
           xhi = x
        endif
40    continue
      maxexp = x

      return
      end
c------------------------------------------------------
      subroutine get_mlim(maxexp,mlimit,eigmin,reclng,recip)
      implicit none
      double precision maxexp
      double precision eigmin
      integer mlimit(3)
      double precision reclng(3),recip(3,3)

c get coefficients for reciprocal space ewald sum

      integer mtop1,mtop2,mtop3,mlim1,mlim2,mlim3
      integer m1,m2,m3,nrecvecs
      double precision z1,z2,z3,expo
      double precision pi

      pi = 3.14159265358979323846d0
      mtop1 = reclng(1)*maxexp/sqrt(eigmin)
      mtop2 = reclng(2)*maxexp/sqrt(eigmin)
      mtop3 = reclng(3)*maxexp/sqrt(eigmin)

      nrecvecs = 0
      mlim1 = 0
      mlim2 = 0
      mlim3 = 0
!$omp parallel do private(z1,z2,z3,expo) reduction(+:nrecvecs)
!$omp&            reduction(max:mlim1,mlim2,mlim3)
      do 100 m1 = -mtop1,mtop1
      do 100 m2 = -mtop2,mtop2
      do 100 m3 = -mtop3,mtop3
        z1 = m1*recip(1,1)+m2*recip(1,2)+m3*recip(1,3) 
        z2 = m1*recip(2,1)+m2*recip(2,2)+m3*recip(2,3) 
        z3 = m1*recip(3,1)+m2*recip(3,2)+m3*recip(3,3) 
        expo = z1**2 + z2**2 + z3**2
        if ( expo .le. maxexp**2 )then
          nrecvecs = nrecvecs + 1
          mlim1 = max(abs(m1),mlim1)
          mlim2 = max(abs(m2),mlim2)
          mlim3 = max(abs(m3),mlim3)
        endif
100   continue
c     write(6,*)'number of reciprocal vecs = ',nrecvecs
c     write(6,*)'mlim1,2,3 = ',mlim1,mlim2,mlim3
      mlimit(1) = mlim1
      mlimit(2) = mlim2
      mlimit(3) = mlim3
      return
      end
c---------------------------------------------------------------------
      subroutine next_value(x,i_sign)
      implicit real*8(a-h,o-z)

c--- the main input value is 'x', which can be any positive real number.
c--- This routine updates 'x' by the number of steps represented by 
c--- i_sign. 'x' will be overwritten by the new value at the end of this
c--- routine. The output value is aimed to be around x*(1+0.01*i_sign), 
c--- i.e., i_sign % larger than the input value of 'x'. 
c--- I wanted to limit the output value to be 10^(n/N) where n is the 
c--- integer variable and N is a fixed integer. Under the requirement 
c--- that the output values next to each other be less than 1 %, N must 
c--- be larger than 231.40..., so I set N=250(=1/dy). This means that 
c--- there are always 250 possible output values between 10^m and 
c--- 10^(m+1), and I call these values as 'candidates'. 
c--- The input value x falls upon the nearest candidate value x' at the 
c--- beginning of the routine. This routine finally returns the value 
c--- that is i_sign'th candidate from x'. 

      parameter(dy=0.004d0)

      x = max(x,1.0d-300)

      y = log10(x)

      int_y = nint(y/dy)

      int_y = int_y + i_sign

      x = DBLE(int_y)*dy

      x = 10.0d0**x

      return
      end
c---------------------------------------------------------------------
      subroutine bubble_sort(ndata,x_in,i_out)
      implicit real*8(a-h,o-z)

      dimension x_in(ndata),i_out(ndata)

      do n=1,ndata
        i_out(n) = n
      end do

      ibottom = 1
      itop = ndata

      do while(ibottom.lt.itop)
        j = ibottom
        do i=ibottom,itop-1
          if(x_in(i_out(i)).gt.x_in(i_out(i+1))) then
            itmp       = i_out(i+1)
            i_out(i+1) = i_out(i)
            i_out(i)   = itmp
            j = i
          end if
        end do
        itop = j
      end do

c     do n=1,ndata
c       print *,i_out(n)
c     end do

      return
      end
