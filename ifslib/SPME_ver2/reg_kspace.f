      subroutine reg_kspace(numatoms,x,y,z,cg,ewaldcof,
     $       ene,force,recip,
     $       maxexp,mlimit,volume,box,virial)
      implicit none
      double precision x(*),y(*),z(*),cg(*),ewaldcof,box(3),
     $      ene,force(3,*),volume,maxexp
      double precision recip(3,3),virial(6),frac(3,numatoms)
      integer numatoms,mlimit(3),mlimax,need

      integer i
      double precision vir,tvir(3,3)

c     print *,'reg_kspace 1'
      ene = 0.d0
!$omp parallel do
      do 50 i = 1,numatoms
        force(1,i) = 0.d0
        force(2,i) = 0.d0
        force(3,i) = 0.d0
        frac(1,i) = x(i)/box(1)
        frac(2,i) = y(i)/box(2)
        frac(3,i) = z(i)/box(3)
50    continue

      mlimax = max(mlimit(1),mlimit(2),mlimit(3))+1
      need   = ( 7*mlimax+6 )*numatoms

c     print *,'reg_kspace 2'

      call recip_reg(numatoms,cg,ene,vir,tvir,need,
     $      mlimit,volume,recip,force,frac,ewaldcof,maxexp)

c     print *,'reg_kspace 8'
      virial(1) = tvir(1,1)
      virial(2) = tvir(1,2)
      virial(3) = tvir(1,3)
      virial(4) = tvir(2,2)
      virial(5) = tvir(2,3)
      virial(6) = tvir(3,3)
      return
      end
c--------------------------------------------------------
      subroutine recip_reg(numatoms,charge,eer,vir,tvir,navail,
     $      mlimit,volume,recip,force,fraction,ewaldcof,maxexp)
      
      implicit none

      integer  numatoms,mlimit(3)
      real*8   charge(*),eer,vir,tvir(3,3),X(navail),volume,recip(3,3)
      integer  mlimax,navail,need,lckc,lcks,lclm,lslm,lelc,lemc,lenc,
     x         lels,lems,lens,lfrc,i,j
      real*8   force(3,*),fraction(3,*),ewaldcof,maxexp

      vir = 0.d0
      do 10 i = 1,3
         do 20 j = 1,3
            tvir(i,j) = 0.d0
 20      continue
 10   continue

c.. check if the the array space in X is large eneough
c   to supply space for the regular ewald summations. 
      
      mlimax = max(mlimit(1),mlimit(2),mlimit(3))+1
      need   = ( 7*mlimax+6 )*numatoms
      if( navail.lt. need ) then
         write(6,*)'Memory space in the FFT-arrays not large enough '
         write(6,*)'Provided space: ',navail,' Needed space: ',need
         stop
      endif

      lckc = 1
      lcks = lckc + numatoms
      lclm = lcks + numatoms
      lslm = lclm + numatoms 
      lelc = lslm + mlimax*numatoms
      lemc = lelc + mlimax*numatoms
      lenc = lemc + mlimax*numatoms
      lels = lenc + mlimax*numatoms
      lems = lels + mlimax*numatoms
      lens = lems + mlimax*numatoms
      lfrc = lens + mlimax*numatoms

c     print *,'reg_kspace 3'
      call ew_ccp5( numatoms,mlimax,mlimit,volume,recip,ewaldcof,
     x              maxexp,charge,fraction,force,X(lckc),
     x              X(lcks),X(lclm),X(lslm),X(lelc),X(lemc),
     x              X(lenc),X(lels),X(lems),X(lens),eer,vir,tvir )

      return
      end


      SUBROUTINE EW_CCP5( maxsit,kmax,mlimit,dvol1,hi,alphad,
     x                    rksmax,schg,fraction,ffxyz,ckc,cks,clm,slm,
     x                    elc,emc,enc,els,ems,ens,pe,vir,tvr )
      implicit none
C
C-----------------------------------------------------------------------
C     Arguments:
C                 maxsit   = number of sites ( atoms )
C                 kmax     = maximum number of k-vectors
C                 dvol1    = volume of md-cell
C                 hi       = reciprocal cell-vectors
C                 alphad   = Ewald - convergence parameter
C                 schg     = fractional coordinates 
C                 fraction = dimensionless ( fractional ) coordinates
C                 ffxyz    = forces on sites
C                 ckc      = work-array to store cosines ( size : maxsit )
C                 cks      = work-array to store sines   ( size : maxsit )
C                 clm      = work-array for cosine-products
C                 slm      = work-array for sine-products
C                 elc      = work-array ( size : (kmax+1)*maxsit )
C                 emc      = work-array ( size : (kmax+1)*maxsit )
C                 enc      = work-array ( size : (kmax+1)*maxsit )
C                 els      = work-array ( size : (kmax+1)*maxsit )
C                 ems      = work-array ( size : (kmax+1)*maxsit )
C                 ens      = work-array ( size : (kmax+1)*maxsit )
C                 pe       = potential energy
C                 vir      = virial
C                 tvr      = virial-tensor
C-----------------------------------------------------------------------
C
      integer    kmax,maxsit,mlimit(3)
      real*8     alphad,ffxyz(3,maxsit),schg(maxsit),fraction(3,maxsit),
     x           ckc(maxsit),cks(maxsit),clm(maxsit),slm(maxsit),
     x           tvr(9),elc(kmax,maxsit),
     x           emc(kmax,maxsit),enc(kmax,maxsit),els(kmax,maxsit),
     x           ems(kmax,maxsit),ens(kmax,maxsit),pe,hi(9),vir,dvol1

      real*8     rkx2,rky2,rkz2,rm,rkx1,rky1,rkz1,rl,rn,
     x           rkx3,rky3,rkz3,rksq,ak,akv,qpe,ckcs,rksmax,
     x           ralph,rvol,ckss,rcl,qvf,sqpi,qforce,twopi,cl,
     x           fac,omg(9),rksmaxsq,ffxyz_tmp(3,maxsit)
      integer    m,mm,mmm,n,nn,nnn,l,ll,mmin,nmin,i,it(9),
     x           klimx,klimy,klimz,klim2y,klim2z

      DATA IT/1,4,7,2,5,8,3,6,9/
      DATA CL/1.0D0/
C
c     print *,'reg_kspace 4'
      TWOPI = 8.D0*ATAN(1.D0)
      SQPI  = SQRT(4.D0*ATAN(1.D0) )
      KLIMX = MLIMIT(1)+1
      KLIMY = MLIMIT(2)+1
      KLIMZ = MLIMIT(3)+1
      KLIM2Y = 2*MLIMIT(2)+1
      KLIM2Z = 2*MLIMIT(3)+1

      RCL=TWOPI/CL
      RVOL=TWOPI/DVOL1
      RALPH=-1.0D0/(4*CL*CL*ALPHAD**2)
      RKSMAXSQ = TWOPI*TWOPI*RKSMAX**2
      fac = 2.0d0
C
C     INITIALISE TENSORS
      DO 10 I=1,9
         TVR(I)=0.0D0
         OMG(I)=0.0D0
   10 CONTINUE
C
C     INITIALISE ACCUMULATORS
      PE=0.0D0
      QPE=0.0D0
      VIR=0.0D0
C
c     print *,'reg_kspace 5'
!$omp parallel do
      do i=1,maxsit
        ffxyz_tmp(1,i) = 0.0d0
        ffxyz_tmp(2,i) = 0.0d0
        ffxyz_tmp(3,i) = 0.0d0
      end do
!$omp parallel do
      DO L=1,KMAX
         DO I=1,MAXSIT
            ELC(L,I)=0.0d0
            EMC(L,I)=0.0d0
            ENC(L,I)=0.0d0
            ELS(L,I)=0.0d0
            EMS(L,I)=0.0d0
            ENS(L,I)=0.0d0
         end do
      end do
C     CALCULATE AND STORE EXPONENTIAL FACTORS
      DO 100 i=1,MAXSIT
         ELC(1,i)=1.0D0
         EMC(1,i)=1.0D0
         ENC(1,i)=1.0D0
         ELS(1,i)=0.0D0
         EMS(1,i)=0.0D0
         ENS(1,i)=0.0D0
         ELC(2,i)=COS(RCL*(fraction(1,i)))
         EMC(2,i)=COS(RCL*(fraction(2,i)))
         ENC(2,i)=COS(RCL*(fraction(3,i)))
         ELS(2,i)=SIN(RCL*(fraction(1,i)))
         EMS(2,i)=SIN(RCL*(fraction(2,i)))
         ENS(2,i)=SIN(RCL*(fraction(3,i)))
 100  CONTINUE
      DO 140 L=3,KMAX
         DO 130 I=1,MAXSIT
            ELC(L,I)=ELC(L-1,I)*ELC(2,I)-ELS(L-1,I)*ELS(2,I)
            EMC(L,I)=EMC(L-1,I)*EMC(2,I)-EMS(L-1,I)*EMS(2,I)
            ENC(L,I)=ENC(L-1,I)*ENC(2,I)-ENS(L-1,I)*ENS(2,I)
            ELS(L,I)=ELS(L-1,I)*ELC(2,I)+ELC(L-1,I)*ELS(2,I)
            EMS(L,I)=EMS(L-1,I)*EMC(2,I)+EMC(L-1,I)*EMS(2,I)
            ENS(L,I)=ENS(L-1,I)*ENC(2,I)+ENC(L-1,I)*ENS(2,I)
 130     CONTINUE
 140  CONTINUE
C
c     print *,'reg_kspace 6'
C     LOOP OVER ALL K VECTORS  K=2PI(LL/CL,MM/CL,NN/CL)
c     MMIN=KLIMY
c     NMIN=KLIMZ+1
!$omp parallel do
!$omp&shared(KLIMX,RCL,HI,KLIM2Y,KLIMY,MAXSIT,ELC,EMC,ELS,EMS,
!$omp&       KLIM2Z,KLIMZ,RKSMAXSQ,RALPH,SCHG,ENC,ENS),
!$omp&private(L,LL,RL,RKX1,RKY1,RKZ1,MMM,MM,M,RM,RKX2,RKY2,RKZ2,
!$omp&        I,CLM,SLM,NNN,NN,N,RN,RKX3,RKY3,RKZ3,RKSQ,AK,AKV,
!$omp&        CKC,CKS,CKCS,CKSS,QVF,QFORCE,MMIN,NMIN),
!$omp&reduction(+:QPE),reduction(+:OMG),reduction(+:FFXYZ_tmp)
      DO 230 L=1,KLIMX
         IF(L.GT.1) THEN
            MMIN=1
            NMIN=1
         ELSE
            MMIN=KLIMY
            NMIN=KLIMZ+1
         ENDIF
         LL=L-1
         RL=RCL*DBLE(LL)
         RKX1=RL*HI(1)
         RKY1=RL*HI(4)
         RKZ1=RL*HI(7)
         DO 220 MMM=MMIN,KLIM2Y
            MM=MMM-KLIMY
            M=IABS(MM)+1
            RM=RCL*DBLE(MM)
            RKX2=RKX1+RM*HI(2)
            RKY2=RKY1+RM*HI(5)
            RKZ2=RKZ1+RM*HI(8)
C     SET TEMPORARY PRODUCTS OF EXPONENTIAL TERMS
            IF(MM.GE.0)THEN
               DO 150 I=1,MAXSIT
                  CLM(I)=ELC(L,I)*EMC(M,I)-ELS(L,I)*EMS(M,I)
                  SLM(I)=ELS(L,I)*EMC(M,I)+EMS(M,I)*ELC(L,I)
 150           CONTINUE
            ELSE
               DO 160 I=1,MAXSIT
                  CLM(I)=ELC(L,I)*EMC(M,I)+ELS(L,I)*EMS(M,I)
                  SLM(I)=ELS(L,I)*EMC(M,I)-EMS(M,I)*ELC(L,I)
 160           CONTINUE
            ENDIF
            DO 210 NNN=NMIN,KLIM2Z
               NN=NNN-KLIMZ
               N=IABS(NN)+1
               RN=RCL*DBLE(NN)
               RKX3=RKX2+RN*HI(3)
               RKY3=RKY2+RN*HI(6)
               RKZ3=RKZ2+RN*HI(9)
C     CALCULATE AK COEFFICIENTS
               RKSQ=RKX3**2+RKY3**2+RKZ3**2
C     
C     BYPASS K VECTORS OUTSIDE CUTOFF
               IF(RKSQ.LE.RKSMAXSQ)THEN
                  AK=EXP(RALPH*RKSQ)/RKSQ
                  AKV=2.0D0*AK*(1.0D0/RKSQ-RALPH)
C     CALCULATE EXP(IKR) TERMS AND PRODUCT WITH SITE CHARGES
                  IF(NN.GE.0)THEN
                     DO 170 I=1,MAXSIT
                        CKC(I)=SCHG(I)*(CLM(I)*ENC(N,I)-SLM(I)*ENS(N,I))
                        CKS(I)=SCHG(I)*(SLM(I)*ENC(N,I)+CLM(I)*ENS(N,I))
 170                 CONTINUE
                  ELSE
                     DO 180 I=1,MAXSIT
                        CKC(I)=SCHG(I)*(CLM(I)*ENC(N,I)+SLM(I)*ENS(N,I))
                        CKS(I)=SCHG(I)*(SLM(I)*ENC(N,I)-CLM(I)*ENS(N,I))
 180                 CONTINUE
                  ENDIF
C     CALCULATE VECTOR SUMS
                  CKCS=0.0D0
                  CKSS=0.0D0
                  DO 190 I=1,MAXSIT
                     CKCS=CKCS+CKC(I)
                     CKSS=CKSS+CKS(I)
 190              CONTINUE
C     ACCUMULATE POTENTIAL ENERGY AND VIRIAL
                  QPE=QPE+AK*(CKCS*CKCS+CKSS*CKSS)
                  QVF=AKV*(CKCS*CKCS+CKSS*CKSS)
                  OMG(1)=OMG(1)-QVF*RKX3*RKX3
                  OMG(2)=OMG(2)-QVF*RKX3*RKY3
                  OMG(3)=OMG(3)-QVF*RKX3*RKZ3
                  OMG(4)=OMG(4)-QVF*RKY3*RKX3
                  OMG(5)=OMG(5)-QVF*RKY3*RKY3
                  OMG(6)=OMG(6)-QVF*RKY3*RKZ3
                  OMG(7)=OMG(7)-QVF*RKZ3*RKX3
                  OMG(8)=OMG(8)-QVF*RKZ3*RKY3
                  OMG(9)=OMG(9)-QVF*RKZ3*RKZ3
C     CALCULATE FORCE ON EACH SITE
                  DO 200 I=1,MAXSIT
                     QFORCE=AK*(CKS(I)*CKCS-CKC(I)*CKSS)
                     FFXYZ_tmp(1,I)=FFXYZ_tmp(1,I)+RL*QFORCE
                     FFXYZ_tmp(2,I)=FFXYZ_tmp(2,I)+RM*QFORCE
                     FFXYZ_tmp(3,I)=FFXYZ_tmp(3,I)+RN*QFORCE
 200              CONTINUE
C     
C     END VECTOR LOOP
               ENDIF
 210        CONTINUE
            NMIN=1
 220     CONTINUE
         MMIN=1
 230  CONTINUE
C     
c     print *,'reg_kspace 7'
C     CALCULATE EWALD FORCE ARRAYS
!$omp parallel do
      DO 240 I=1,MAXSIT
         FFXYZ(1,I) = 2.0D0*FAC*RVOL*HI(1)*FFXYZ_tmp(1,I)
         FFXYZ(2,I) = 2.0D0*FAC*RVOL*HI(5)*FFXYZ_tmp(2,I)
         FFXYZ(3,I) = 2.0D0*FAC*RVOL*HI(9)*FFXYZ_tmp(3,I)
 240  CONTINUE
C     
C     CALCULATE FINAL CORRECTED POTENTIAL
      PE = RVOL*QPE*FAC
C     
C     CALCULATE FINAL VIRIAL TENSOR
      TVR(1)=-RVOL*FAC*(QPE+OMG(1))
      TVR(2)=-RVOL*FAC*OMG(2)
      TVR(3)=-RVOL*FAC*OMG(3)
      TVR(4)=-RVOL*FAC*OMG(4)
      TVR(5)=-RVOL*FAC*(QPE+OMG(5))
      TVR(6)=-RVOL*FAC*OMG(6)
      TVR(7)=-RVOL*FAC*OMG(7)
      TVR(8)=-RVOL*FAC*OMG(8)
      TVR(9)=-RVOL*FAC*(QPE+OMG(9))
C
      VIR = TVR(1) + TVR(5) + TVR(9)
C
      RETURN
      END
