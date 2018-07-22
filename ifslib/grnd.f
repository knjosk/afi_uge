************************************************************************
*
* Copyright (C) 1997 Makoto Matsumoto and Takuji Nishimura.
*                   (Keio University)
*
************************************************************************
*
* Fortran translation by Hiroshi Takano.  Jan. 13, 1999.
*
************************************************************************
      double precision function grnd()
*
      implicit integer(a-z)
      integer*8 yh,yl,yy,z
      integer*8 TMASKD,TMASKE,TEXP,ZERO,MINIMUM
      real*8 dz
      equivalence (z,dz)
*
* Period parameters
      parameter(N     =  624)
      parameter(N1    =  N+1)
      parameter(M     =  397)
      parameter(initseed = 4357)
      parameter(TMAX = 1280)
      parameter(MATA  = -1727483681)
*                                    constant vector a
      parameter(UMASK = x'80000000')
*                                    most significant w-r bits
      parameter(LMASK = x'7fffffff')
*                                    least significant r bits
* Tempering parameters
      parameter(TMASKB = -1658038656)
      parameter(TMASKC = -272236544)
      parameter(TMASKD = 4294967295_8)
      parameter(TMASKE = 4503599627370495_8)
      parameter(TEXP   = 4607182418800017408_8)
      parameter(ZERO   = 0_8)
      parameter(MINIMUM = 1_8)
*
      dimension mt(0:N+32-1,0:TMAX),mti(32,0:TMAX)
*                     the array for the state vector
      common /block/mti,mt
      data   mti/40992*N1/
*                     mti==N+1 means mt[N] is not initialized
      common /cpunum/me
      data me/-1/
c$omp threadprivate (/cpunum/)
*
      dimension mag01(0:1)
      data mag01/0, MATA/
*                        mag01(x) = x * MATA for x=0,1
*
      TSHFTU(y)=ishft(y,-11)
      TSHFTS(y)=ishft(y,7)
      TSHFTT(y)=ishft(y,15)
      TSHFTL(y)=ishft(y,-18)
*
      icpu = 0
c$    if (me .lt. 0) then
c$      call omp_set_dynamic(.false.)
c$      me = omp_get_thread_num()
*dbug        print *,'setup finished: ',me
c$    endif
c$    icpu = me
      if(mti(1,icpu).ge.N) then
*                       generate N words at one time
        if(mti(1,icpu).eq.N+1) then
*                            if sgrnd() has not been called,
          seed = initseed
          do i=1,icpu
            seed = 86243*seed
          enddo
          call sgrnd(seed)
        endif
*
        do 1000 kk=0,N-M-1
            y=ior(iand(mt(kk,icpu),UMASK),iand(mt(kk+1,icpu),LMASK))
            mt(kk,icpu)=ieor(ieor(mt(kk+M,icpu),ishft(y,-1)),
     &                       mag01(iand(y,1)))
 1000   continue
        do 1100 kk=N-M,N-2
            y=ior(iand(mt(kk,icpu),UMASK),iand(mt(kk+1,icpu),LMASK))
            mt(kk,icpu)=ieor(ieor(mt(kk+(M-N),icpu),ishft(y,-1)),
     &                       mag01(iand(y,1)))
 1100   continue
        y=ior(iand(mt(N-1,icpu),UMASK),iand(mt(0,icpu),LMASK))
        mt(N-1,icpu)=ieor(ieor(mt(M-1,icpu),ishft(y,-1)),
     &                    mag01(iand(y,1)))
        mti(1,icpu) = 0
      endif
*
      y=mt(mti(1,icpu),icpu)
      mti(1,icpu)=mti(1,icpu)+1
      y=ieor(y,TSHFTU(y))
      y=ieor(y,iand(TSHFTS(y),TMASKB))
      y=ieor(y,iand(TSHFTT(y),TMASKC))
      y=ieor(y,TSHFTL(y))

      y2=mt(mti(1,icpu),icpu)
      mti(1,icpu)=mti(1,icpu)+1
      y2=ieor(y2,TSHFTU(y2))
      y2=ieor(y2,iand(TSHFTS(y2),TMASKB))
      y2=ieor(y2,iand(TSHFTT(y2),TMASKC))
      y2=ieor(y2,TSHFTL(y2))
*
      yh = y
      yl = y2
      yl = iand(yl,TMASKD)
      yh = ishft(yh, 20_8)
      yl = ishft(yl,-12_8)
      yy = iand(ior(yh,yl),TMASKE)
      if (yy .eq. ZERO) then
        yy = MINIMUM
      endif
      z = ior(TEXP,yy)
      grnd = dz - 1.d0
*
      return
      end
      subroutine sgrnd(seed)
*
      implicit integer(a-z)
*
* Period parameters
      parameter(N     =  624)
      parameter(TMAX  =  1280)
*
      dimension mt(0:N+32-1,0:TMAX),mti(32,0:TMAX)
*                     the array for the state vector
      common /block/mti,mt
*
*      setting initial seeds to mt[N] using
*      the generator Line 25 of Table 1 in
*      [KNUTH 1981, The Art of Computer Programming
*         Vol. 2 (2nd Ed.), pp102]
*
      icpu = 0
c$    icpu = omp_get_thread_num()
*dbug     write(6,'( i10,z20 )') icpu,seed

      mt(0,icpu)= iand(seed,-1)
      do i=1,N-1
        mt(i,icpu) = 69069 * mt(i-1,icpu)
      enddo
*
      return
      end
      subroutine mtsave(fileno)
*
      implicit integer(a-z)
*
* Period parameters
      parameter(N     =  624)
      parameter(TMAX  =  1280)
*
      dimension mt(0:N+32-1,0:TMAX),mti(32,0:TMAX)
*                     the array for the state vector
      common /block/mti,mt

      open(unit=fileno,form='unformatted')
      write(fileno) (mti(1,i),i=0,TMAX)
      write(fileno) ((mt(i,j),i=0,N-1),j=0,TMAX)
      close(fileno)
      return
      end
      subroutine mtload(fileno)
*
      implicit integer(a-z)
*
* Period parameters
      parameter(N     =  624)
      parameter(TMAX  =  1280)
*
      dimension mt(0:N+32-1,0:TMAX),mti(32,0:TMAX)
*                     the array for the state vector
      common /block/mti,mt

      open(fileno,form='unformatted')
      read(fileno) (mti(1,i),i=0,TMAX)
      read(fileno) ((mt(i,j),i=0,N-1),j=0,TMAX)
      close(fileno)
      return
      end
*
