      program test
      double precision x(100)
      double precision grnd
      double precision cpu
      isize=10000

      do i = 1,isize
         x(i) = grnd()
         c = tremain(cpu)
         write(*,100) i, cpu
      enddo
c      do i = 1,isize
c         write(*,100) i,x(i)
c      enddo
 100  format(I6,' ',F32.24)
      write(*,100) i, cpu
      end
