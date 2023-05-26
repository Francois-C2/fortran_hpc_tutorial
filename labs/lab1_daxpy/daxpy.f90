! 
!     Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
!
! NVIDIA CORPORATION and its licensors retain all intellectual property
! and proprietary rights in and to this software, related documentation
! and any modifications thereto.  Any use, reproduction, disclosure or
! distribution of this software and related documentation without an express
! license agreement from NVIDIA CORPORATION is strictly prohibited.
! 


	
!
! DAXPY example using Do Concurrent construct in Fortran
! Build with
!   nvfortran -stdpar -Minfo -fast daxpy.f90
! Build with to target Multicore
!   nvfortran -stdpar=multicore -Minfo=accel -fast daxpy.f90
!

module sm
    contains
    subroutine daxpy_concurrent(x,y,n,a)
        real(kind=8),dimension(:) :: x, y
        real(kind=8) :: a
        integer :: n, i  
        do i=1,n
          y(i) = a*x(i)+y(i)
        enddo  
    end subroutine 

    subroutine daxpy_do(x,y,n,a)
        real(kind=8),dimension(:) :: x, y
        real(kind=8) :: a
        integer :: n, i  
        do i = 1, n
          y(i) = a*x(i)+y(i)
        enddo  
    end subroutine 
end module

program main
    use sm
    real(kind=8),dimension(:),allocatable :: x, x2, y, y2
    real(kind=8) :: a = 2.0
    integer :: n, i, err = 0
    integer :: c0, c1, c2, cpar, cseq

    n = 1000000

    allocate(x2(n), y2(n),x(n), y(n))

    do i = 1, n
       x2(i) = 1
       y2(i)  = i
       x(i)  = 1
       y(i)  = i
    enddo

    call system_clock( count=c0 )
    call daxpy_do(x2, y2, n, a)
    call system_clock( count=c1 )
    call daxpy_concurrent(x, y, n, a)
    call system_clock( count=c2 )
    cseq = c1 - c0
    cpar = c2 - c1

    do i = 1, n
      if(x(i) .ne. x2(i)) then
          err = err + 1
      endif
    enddo

    print *, cseq, ' microseconds sequential'
    print *, cpar, ' microseconds parallel '
    if(err .eq. 0) then
      print *, "Test PASSED"
    else
      print *, "Test FAILED"
    endif

end program
