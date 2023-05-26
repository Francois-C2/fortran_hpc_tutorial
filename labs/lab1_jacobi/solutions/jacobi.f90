! 
!     Copyright (c) 2020, NVIDIA CORPORATION.  All rights reserved.
!
! NVIDIA CORPORATION and its licensors retain all intellectual property
! and proprietary rights in and to this software, related documentation
! and any modifications thereto.  Any use, reproduction, disclosure or
! distribution of this software and related documentation without an express
! license agreement from NVIDIA CORPORATION is strictly prohibited.
! 


!
! Jacobi iteration example using Do Concurrent in Fortran
! Build with to target NVIDIA GPU
!   nvfortran -stdpar -Minfo=accel -fast f3.f90
! Build with to target host multicore
!   nvfortran -stdpar=multicore -Minfo=accel -fast f3.f90
!
module sm
contains
 subroutine smooth( a, b, w0, w1, w2, n, m, niters )
  real, dimension(:,:) :: a,b
  real :: w0, w1, w2
  integer :: n, m, niters
  integer :: i, j, iter
   do iter = 1,niters
    do concurrent(i=2 : n-1, j=2 : m-1) 
      a(i,j) = w0 * b(i,j) + &
               w1 * (b(i-1,j) + b(i,j-1) + b(i+1,j) + b(i,j+1)) + &
               w2 * (b(i-1,j-1) + b(i-1,j+1) + b(i+1,j-1) + b(i+1,j+1))
    enddo
    do concurrent(i=2 : n-1, j=2 : m-1)
      b(i,j) = w0 * a(i,j) + &
               w1 * (a(i-1,j) + a(i,j-1) + a(i+1,j) + a(i,j+1)) + &
               w2 * (a(i-1,j-1) + a(i-1,j+1) + a(i+1,j-1) + a(i+1,j+1))
    enddo
   enddo
 end subroutine
 
 subroutine smoothhost( a, b, w0, w1, w2, n, m, niters )
  real, dimension(:,:) :: a,b
  real :: w0, w1, w2
  integer :: n, m, niters
  integer :: i, j, iter
   do iter = 1,niters
    do i = 2,n-1
     do j = 2,m-1
      a(i,j) = w0 * b(i,j) + &
               w1 * (b(i-1,j) + b(i,j-1) + b(i+1,j) + b(i,j+1)) + &
               w2 * (b(i-1,j-1) + b(i-1,j+1) + b(i+1,j-1) + b(i+1,j+1))
     enddo
    enddo
    do i = 2,n-1
     do j = 2,m-1
      b(i,j) = w0 * a(i,j) + &
               w1 * (a(i-1,j) + a(i,j-1) + a(i+1,j) + a(i,j+1)) + &
               w2 * (a(i-1,j-1) + a(i-1,j+1) + a(i+1,j-1) + a(i+1,j+1))
     enddo
    enddo
   enddo
 end subroutine
end module

program main
 use sm
 implicit none
 real,dimension(:,:),allocatable :: aahost, bbhost, aapar, bbpar
 real :: w0, w1, w2
 integer :: i,j,n,m,iters
 integer :: c0, c1, c2, c3, c4, cpar, cseq
 integer :: errs, args
 character(10) :: arg
 real :: dif, tol

 n = 1000
 m = n
 iters = 100

 allocate( aapar(n,m) )
 allocate( bbpar(n,m) )
 allocate( aahost(n,m) )
 allocate( bbhost(n,m) )
 do i = 1,n
   do j = 1,m
     aapar(i,j) = 0
     bbpar(i,j) = i*1000 + j
     aahost(i,j) = 0
     bbhost(i,j) = i*1000 + j
   enddo
 enddo
 w0 = 0.5
 w1 = 0.3
 w2 = 0.2
 call system_clock( count=c1 )
 call smooth( aapar, bbpar, w0, w1, w2, n, m, iters )
 call system_clock( count=c2 )
 cpar = c2 - c1
 call smoothhost( aahost, bbhost, w0, w1, w2, n, m, iters )
 call system_clock( count=c3)
 cseq = c3 - c2
 ! check the results
 errs = 0
 tol = 0.000005
 do i = 1,n
  do j = 1,m
   dif = abs(aapar(i,j) - aahost(i,j))
   if( aahost(i,j) .ne. 0 ) dif = abs(dif/aahost(i,j))
   if( dif .gt. tol )then
    errs = errs + 1
    if( errs .le. 10 )then
     print *, i, j, aapar(i,j), aahost(i,j)
    endif
   endif
  enddo
 enddo
 print *, cpar, ' microseconds on parallel with do concurrent'
 print *, cseq, ' microseconds on sequential'
 if (errs .ne. 0) then
    print *, "Test FAILED"
    print *, errs, ' errors found'
 else
    print *, "Test PASSED"
 endif

end program
