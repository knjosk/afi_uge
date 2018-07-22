c---------------------------------------------------------------------
      subroutine spme_direct(numatoms,x,y,z,cg,cutoff,ewaldcof,box,
     $      ene,fx,fy,fz,virial,flg_minimg)
      implicit none
      integer numatoms,nreps(3)
      integer*8 iter_half
      double precision x(*),y(*),z(*),cg(*),cutoff,ewaldcof,box(3),
     $      ene,fx(numatoms),fy(numatoms),fz(numatoms),virial(6)
      double precision flg_minimg(*)
      double precision virial_tmp(6),fac,pi,cutoff2
      double precision xmax,ymax,zmax,xmin,ymin,zmin
      integer i,j,nflg

      pi = 3.14159265358979323846d0

      ene = 0.d0
      do 10 i = 1,6
        virial(i) = 0.d0
10    continue
!$omp parallel do
      do 20 i = 1,numatoms
        fx(i) = 0.d0
        fy(i) = 0.d0
        fz(i) = 0.d0
20    continue

      xmax = -1.0d300
      ymax = -1.0d300
      zmax = -1.0d300
      xmin =  1.0d300
      ymin =  1.0d300
      zmin =  1.0d300
!$omp parallel do reduction(max:xmax,ymax,zmax),
!$omp&            reduction(min:xmin,ymin,zmin)
      do i = 1,numatoms
        xmax = max(xmax,x(i))
        ymax = max(ymax,y(i))
        zmax = max(zmax,z(i))
        xmin = min(xmin,x(i))
        ymin = min(ymin,y(i))
        zmin = min(zmin,z(i))
      end do
      nflg = max(1,nint((xmax-xmin)/box(1)))
     &     * max(1,nint((ymax-ymin)/box(2)))
     &     * max(1,nint((zmax-zmin)/box(3)))

      nreps(1) = anint(cutoff*0.999999999d0 / box(1))
      nreps(2) = anint(cutoff*0.999999999d0 / box(2))
      nreps(3) = anint(cutoff*0.999999999d0 / box(3))
      iter_half = int((nreps(1)*2+1),8)
     &          * int((nreps(2)*2+1),8)
     &          * int((nreps(3)*2+1),8) / 2
      fac = (2.d0/sqrt(pi))*ewaldcof
      cutoff2 = cutoff*cutoff

      if(min(box(1),box(2),box(3)).lt.2.0d0*cutoff) then  !!!!!!!!!!!!
        if(min(box(1),box(2),box(3)).lt.cutoff) then
          call get_dir_pair_ii(numatoms,cg,cutoff2,ewaldcof,box,
     $         ene,virial,fac,nreps,iter_half)
        end if
        if(nflg.gt.1) then
!$omp     parallel private(virial_tmp)
            virial_tmp = 0.d0
!$omp     do reduction(+:ene,fx,fy,fz) schedule(dynamic,1)
          do j = 1,numatoms-1
          do i = j+1,numatoms
            call get_dir_pair(numatoms,i,j,x,y,z,cg,
     $           cutoff2,ewaldcof,box,ene,fx,fy,fz,virial_tmp,
     $           flg_minimg,fac,nreps,iter_half)
          end do
          end do
!$omp     critical
          do i=1,6
            virial(i) = virial(i) + virial_tmp(i)
          enddo
!$omp     end critical
!$omp     end parallel
        else
!$omp     parallel private(virial_tmp)
            virial_tmp = 0.d0
!$omp     do reduction(+:ene,fx,fy,fz) schedule(dynamic,1)
          do j = 1,numatoms-1
          do i = j+1,numatoms
            call get_dir_pair_lt15(numatoms,i,j,x,y,z,cg,
     $           cutoff2,ewaldcof,box,ene,fx,fy,fz,virial_tmp,
     $           flg_minimg,fac,nreps,iter_half)
          end do
          end do
!$omp     critical
          do i=1,6
            virial(i) = virial(i) + virial_tmp(i)
          enddo
!$omp     end critical
!$omp     end parallel
        end if
      else  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        if(nflg.gt.1) then
!$omp     parallel private(virial_tmp)
            virial_tmp = 0.d0
!$omp     do reduction(+:ene,fx,fy,fz) schedule(dynamic,1)
          do j = 1,numatoms-1
          do i = j+1,numatoms
            call get_dir_pair_minimg(numatoms,i,j,x,y,z,cg,
     $           cutoff2,ewaldcof,box,ene,fx,fy,fz,virial_tmp,
     $           flg_minimg,fac)
          end do
          end do
!$omp     critical
          do i=1,6
            virial(i) = virial(i) + virial_tmp(i)
          enddo
!$omp     end critical
!$omp     end parallel
        else
!$omp     parallel private(virial_tmp)
            virial_tmp = 0.d0
!$omp     do reduction(+:ene,fx,fy,fz) schedule(dynamic,1)
          do j = 1,numatoms-1
          do i = j+1,numatoms
            call get_dir_pair_minimg_lt15(numatoms,i,j,x,y,z,cg,
     $           cutoff2,ewaldcof,box,ene,fx,fy,fz,virial_tmp,
     $           flg_minimg,fac)
          end do
          end do
!$omp     critical
          do i=1,6
            virial(i) = virial(i) + virial_tmp(i)
          enddo
!$omp     end critical
!$omp     end parallel
        end if
      end if  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      return
      end
c---------------------------------------------------------------------
      subroutine get_dir_pair(numatoms,i,j,x,y,z,cg,cutoff2,
     $      ewaldcof,box,ene,fx,fy,fz,virial,
     &      flg_array,fac,nreps,iter_half)
      implicit none
      integer numatoms,i,j,nreps(3)
      integer*8 iter_half
      double precision x(*),y(*),z(*),cg(*),cutoff2,ewaldcof,box(3),
     $      ene,fx(*),fy(*),fz(*),virial(6),fac
      double precision flg_array(numatoms,*)

      double precision xij,yij,zij,xij2,yij2,zij2,r2
      double precision r,pot,grad_term,cg_grad_term
      double precision xij0,yij0,zij0,flg,cg_ij,cg_grad(3)
      integer k,l,m
      integer*8 iter

      xij0 = x(i) - x(j)
      yij0 = y(i) - y(j)
      zij0 = z(i) - z(j)
      xij0 = xij0 - box(1)*anint(xij0/box(1))
      yij0 = yij0 - box(2)*anint(yij0/box(2))
      zij0 = zij0 - box(3)*anint(zij0/box(3))

      cg_ij = cg(i)*cg(j)
      iter = 0
      do 50 m = -nreps(3),nreps(3)
       zij = zij0 + m*box(3)
       zij2 = zij*zij
       do 45 l = -nreps(2),nreps(2)
        yij = yij0 + l*box(2)
        yij2 = yij*yij
        do 40 k = -nreps(1),nreps(1)
         xij = xij0 + k*box(1)
         xij2 = xij*xij
         r2 = xij2+yij2+zij2
         if ( r2 .lt. cutoff2 )then
          r = sqrt(r2)
          if ( iter.eq.iter_half ) then ! if i-j is minimum image
           flg = flg_array(i,j)
          else
           flg = 1.0d0
          end if
          call ew_direct(r,fac,ewaldcof,pot,grad_term,flg)
          ene = ene + cg_ij*pot
          cg_grad_term = cg_ij*grad_term
          cg_grad(1) = xij*cg_grad_term
          cg_grad(2) = yij*cg_grad_term
          cg_grad(3) = zij*cg_grad_term
          fx(i) = fx(i) - cg_grad(1)
          fx(j) = fx(j) + cg_grad(1)
          fy(i) = fy(i) - cg_grad(2)
          fy(j) = fy(j) + cg_grad(2)
          fz(i) = fz(i) - cg_grad(3)
          fz(j) = fz(j) + cg_grad(3)
          virial(1) = virial(1) + xij*cg_grad(1)
          virial(2) = virial(2) + xij*cg_grad(2)
          virial(3) = virial(3) + xij*cg_grad(3)
          virial(4) = virial(4) + yij*cg_grad(2)
          virial(5) = virial(5) + yij*cg_grad(3)
          virial(6) = virial(6) + zij*cg_grad(3)
         endif
         iter = iter + 1
40      continue
45     continue
50    continue
      return
      end
c---------------------------------------------------------------------
      subroutine get_dir_pair_lt15(numatoms,i,j,x,y,z,cg,cutoff2,
     $      ewaldcof,box,ene,fx,fy,fz,virial,
     &      flg_array,fac,nreps,iter_half)
      implicit none
      integer numatoms,i,j,nreps(3)
      integer*8 iter_half
      double precision x(*),y(*),z(*),cg(*),cutoff2,ewaldcof,box(3),
     $      ene,fx(*),fy(*),fz(*),virial(6),fac
      double precision flg_array(numatoms,*)

      double precision xij,yij,zij,xij2,yij2,zij2,r2
      double precision r,pot,grad_term,cg_grad_term
      double precision xij0,yij0,zij0,flg,cg_ij,cg_grad(3)
      double precision xijp,yijp,zijp,xijm,yijm,zijm
      integer k,l,m
      integer*8 iter

      xij0 = x(i) - x(j)
      yij0 = y(i) - y(j)
      zij0 = z(i) - z(j)
      if (abs(xij0).gt.0.5d0*box(1)) then
        xijp = xij0 + box(1)
        if (abs(xijp).lt.0.5d0*box(1)) then
          xij0=xijp
        else
          xijm = xij0 - box(1)
          xij0=xijm
        end if
      end if
      if (abs(yij0).gt.0.5d0*box(2)) then
        yijp = yij0 + box(2)
        if (abs(yijp).lt.0.5d0*box(2)) then
          yij0=yijp
        else
          yijm = yij0 - box(2)
          yij0=yijm
        end if
      end if
      if (abs(zij0).gt.0.5d0*box(3)) then
        zijp = zij0 + box(3)
        if (abs(zijp).lt.0.5d0*box(3)) then
          zij0=zijp
        else
          zijm = zij0 - box(3)
          zij0=zijm
        end if
      end if

      cg_ij = cg(i)*cg(j)
      iter = 0
      do 50 m = -nreps(3),nreps(3)
       zij = zij0 + m*box(3)
       zij2 = zij*zij
       do 45 l = -nreps(2),nreps(2)
        yij = yij0 + l*box(2)
        yij2 = yij*yij
        do 40 k = -nreps(1),nreps(1)
         xij = xij0 + k*box(1)
         xij2 = xij*xij
         r2 = xij2+yij2+zij2
         if ( r2 .lt. cutoff2 )then
          r = sqrt(r2)
          if ( iter.eq.iter_half ) then ! if i-j is minimum image
           flg = flg_array(i,j)
          else
           flg = 1.0d0
          end if
          call ew_direct(r,fac,ewaldcof,pot,grad_term,flg)
          ene = ene + cg_ij*pot
          cg_grad_term = cg_ij*grad_term
          cg_grad(1) = xij*cg_grad_term
          cg_grad(2) = yij*cg_grad_term
          cg_grad(3) = zij*cg_grad_term
          fx(i) = fx(i) - cg_grad(1)
          fx(j) = fx(j) + cg_grad(1)
          fy(i) = fy(i) - cg_grad(2)
          fy(j) = fy(j) + cg_grad(2)
          fz(i) = fz(i) - cg_grad(3)
          fz(j) = fz(j) + cg_grad(3)
          virial(1) = virial(1) + xij*cg_grad(1)
          virial(2) = virial(2) + xij*cg_grad(2)
          virial(3) = virial(3) + xij*cg_grad(3)
          virial(4) = virial(4) + yij*cg_grad(2)
          virial(5) = virial(5) + yij*cg_grad(3)
          virial(6) = virial(6) + zij*cg_grad(3)
         endif
         iter = iter + 1
40      continue
45     continue
50    continue
      return
      end
c---------------------------------------------------------------------
      subroutine get_dir_pair_minimg(numatoms,i,j,x,y,z,cg,
     $      cutoff2,ewaldcof,box,ene,fx,fy,fz,virial,flg_array,fac)
      implicit none
      integer numatoms,i,j
      double precision x(*),y(*),z(*),cg(*),cutoff2,ewaldcof,box(3),
     $      ene,fx(*),fy(*),fz(*),virial(6),fac
      double precision flg_array(numatoms,*)

      double precision xij,yij,zij,xij2,yij2,zij2,r2
      double precision r,pot,grad_term,cg_grad_term
      double precision xij0,yij0,zij0,flg,cg_ij,cg_grad(3)

      xij0 = x(i) - x(j)
      yij0 = y(i) - y(j)
      zij0 = z(i) - z(j)
      xij  = xij0 - box(1)*anint(xij0/box(1))
      yij  = yij0 - box(2)*anint(yij0/box(2))
      zij  = zij0 - box(3)*anint(zij0/box(3))

      cg_ij = cg(i)*cg(j)
       zij2 = zij*zij
        yij2 = yij*yij
         xij2 = xij*xij
         r2 = xij2+yij2+zij2
         if ( r2 .lt. cutoff2 )then
          r = sqrt(r2)
          flg = flg_array(i,j)
          call ew_direct(r,fac,ewaldcof,pot,grad_term,flg)
          ene = ene + cg_ij*pot
          cg_grad_term = cg_ij*grad_term
          cg_grad(1) = xij*cg_grad_term
          cg_grad(2) = yij*cg_grad_term
          cg_grad(3) = zij*cg_grad_term
          fx(i) = fx(i) - cg_grad(1)
          fx(j) = fx(j) + cg_grad(1)
          fy(i) = fy(i) - cg_grad(2)
          fy(j) = fy(j) + cg_grad(2)
          fz(i) = fz(i) - cg_grad(3)
          fz(j) = fz(j) + cg_grad(3)
          virial(1) = virial(1) + xij*cg_grad(1)
          virial(2) = virial(2) + xij*cg_grad(2)
          virial(3) = virial(3) + xij*cg_grad(3)
          virial(4) = virial(4) + yij*cg_grad(2)
          virial(5) = virial(5) + yij*cg_grad(3)
          virial(6) = virial(6) + zij*cg_grad(3)
         endif

      return
      end
c---------------------------------------------------------------------
      subroutine get_dir_pair_minimg_lt15(numatoms,i,j,x,y,z,cg,
     $      cutoff2,ewaldcof,box,ene,fx,fy,fz,virial,flg_array,fac)
      implicit none
      integer numatoms,i,j
      double precision x(*),y(*),z(*),cg(*),cutoff2,ewaldcof,box(3),
     $      ene,fx(*),fy(*),fz(*),virial(6),fac
      double precision flg_array(numatoms,*)

      double precision xij,yij,zij,xij2,yij2,zij2,r2
      double precision r,pot,grad_term,cg_grad_term
      double precision xij0,yij0,zij0,flg,cg_ij,cg_grad(3)
      double precision xijp,yijp,zijp,xijm,yijm,zijm

      xij0 = x(i) - x(j)
      yij0 = y(i) - y(j)
      zij0 = z(i) - z(j)
      if (abs(xij0).lt.0.5d0*box(1)) then
        xij=xij0
      else
        xijp = xij0 + box(1)
        if (abs(xijp).lt.0.5d0*box(1)) then
          xij=xijp
        else
          xijm = xij0 - box(1)
          xij=xijm
        end if
      end if
      if (abs(yij0).lt.0.5d0*box(2)) then
        yij=yij0
      else
        yijp = yij0 + box(2)
        if (abs(yijp).lt.0.5d0*box(2)) then
          yij=yijp
        else
          yijm = yij0 - box(2)
          yij=yijm
        end if
      end if
      if (abs(zij0).lt.0.5d0*box(3)) then
        zij=zij0
      else
        zijp = zij0 + box(3)
        if (abs(zijp).lt.0.5d0*box(3)) then
          zij=zijp
        else
          zijm = zij0 - box(3)
          zij=zijm
        end if
      end if

      cg_ij = cg(i)*cg(j)
       zij2 = zij*zij
        yij2 = yij*yij
         xij2 = xij*xij
         r2 = xij2+yij2+zij2
         if ( r2 .lt. cutoff2 )then
          r = sqrt(r2)
          flg = flg_array(i,j)
          call ew_direct(r,fac,ewaldcof,pot,grad_term,flg)
          ene = ene + cg_ij*pot
          cg_grad_term = cg_ij*grad_term
          cg_grad(1) = xij*cg_grad_term
          cg_grad(2) = yij*cg_grad_term
          cg_grad(3) = zij*cg_grad_term
          fx(i) = fx(i) - cg_grad(1)
          fx(j) = fx(j) + cg_grad(1)
          fy(i) = fy(i) - cg_grad(2)
          fy(j) = fy(j) + cg_grad(2)
          fz(i) = fz(i) - cg_grad(3)
          fz(j) = fz(j) + cg_grad(3)
          virial(1) = virial(1) + xij*cg_grad(1)
          virial(2) = virial(2) + xij*cg_grad(2)
          virial(3) = virial(3) + xij*cg_grad(3)
          virial(4) = virial(4) + yij*cg_grad(2)
          virial(5) = virial(5) + yij*cg_grad(3)
          virial(6) = virial(6) + zij*cg_grad(3)
         endif

      return
      end
c---------------------------------------------------------------------
      subroutine get_dir_pair_ii(numatoms,cg,cutoff2,
     $      ewaldcof,box,ene,virial,fac,nreps,iter_half)
      implicit none
      integer numatoms,nreps(3)
      integer*8 iter_half
      double precision cg(*),cutoff2,ewaldcof,box(3),
     $      ene,virial(6),fac

      double precision xij,yij,zij,xij2,yij2,zij2,r2,r,pot,grad_term
      double precision cg_sum,ene_tmp,vir_tmp(6)
      integer i,k,l,m
      integer*8 iter

      ene_tmp = 0.d0
      do 10 i = 1,6
        vir_tmp(i) = 0.d0
10    continue

      iter = 0
      do 50 m = -nreps(3),nreps(3)
       zij = m*box(3)
       zij2 = zij*zij
       do 45 l = -nreps(2),nreps(2)
        yij = l*box(2)
        yij2 = yij*yij
        do 40 k = -nreps(1),nreps(1)
         if ( iter.eq.iter_half ) go to 39 ! if i-j is minimum image
         xij = k*box(1)
         xij2 = xij*xij
         r2 = xij2+yij2+zij2
         if ( r2 .lt. cutoff2 )then
          r = sqrt(r2)
          call ew_direct(r,fac,ewaldcof,pot,grad_term,1.0d0)
          ene_tmp = ene_tmp + pot
          vir_tmp(1) = vir_tmp(1) + xij2*grad_term
c         vir_tmp(2) = vir_tmp(2) + xij*yij*grad_term
c         vir_tmp(3) = vir_tmp(3) + xij*zij*grad_term
          vir_tmp(4) = vir_tmp(4) + yij2*grad_term
c         vir_tmp(5) = vir_tmp(5) + yij*zij*grad_term
          vir_tmp(6) = vir_tmp(6) + zij2*grad_term
         endif
39       iter = iter + 1
40      continue
45     continue
50    continue

      cg_sum = 0.d0
!$omp parallel do reduction(+:cg_sum)
      do 140 i=1,numatoms
        cg_sum = cg_sum + cg(i)*cg(i)
140   continue

      ene = ene + 0.5d0*cg_sum*ene_tmp
      do 150 i = 1,6
       virial(i) = virial(i) + 0.5d0*cg_sum*vir_tmp(i)
150   continue

      return
      end
c---------------------------------------------------------------------
      subroutine ew_direct(r,fac,ewaldcof,pot,term,flg)
      implicit none
      double precision r,fac,ewaldcof,pot,term,flg

      double precision x,merfc,erf

      x = r*ewaldcof
      merfc  = flg - erf(x)
      pot = merfc/r
      term = fac*exp(-x**2)
      term = -(term + pot)/r/r

      return
      end
c------------------------------------------------------------------------
      subroutine spme_self(cg,numatoms,ene,ewaldcof)
      implicit none
      integer numatoms
      double precision cg(*),ene,ewaldcof

      integer i
      double precision ee,pi
      ee = 0.d0
      pi = 3.14159265358979323846d0
!$omp parallel do reduction(+:ee)
      do 10 i = 1,numatoms
        ee = ee + cg(i)**2
10    continue
      ene = -ee*ewaldcof/sqrt(pi)
      return
      end
c------------------------------------------------------------------
