program main
      use cutensorex
      integer, parameter :: ni=5120, nj=5120, nk=5120, ntimes=10
      real(8), allocatable, dimension(:,:) :: a, b, d
      allocate(a(ni,nk),b(nk,nj),d(ni,nj))
      call random_number(a)
      call random_number(b)
      d = 0.0d0
 
      print *,"cutensor"
      call cpu_time(t1)
      do nt = 1, ntimes
        d = d + matmul(a,b)
      end do
      call cpu_time(t2)
 
      flops = 2.0*ni*nj*nk
      flops = flops*ntimes
      print *,"times",t2,t1,t2-t1
      print *,"GFlops",flops/(t2-t1)/1.e9
      end program
