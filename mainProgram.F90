module compute_kernel

    implicit none
    private

    real*8, dimension(:,:,:), allocatable :: A, B, C

    public allocate_memory, initialize_arrays, call_kernel, print_C_sum
!$omp declare target(A, B, C)
    contains

    subroutine allocate_memory(II, JJ, LL)
        integer, intent(in) :: II, JJ, LL

        allocate(A(II,JJ,LL))
        allocate(B(II,JJ,LL))
        allocate(C(II,JJ,LL))
!$omp target enter data map(to:A,B,C)
    end subroutine

    subroutine initialize_arrays()
        A = 1.0
        B = 2.0
!$omp target update to(A,B)
    end subroutine

    subroutine call_kernel(I, J, L)
!$omp declare target
        integer, intent(in) :: I, J, L

        C(I, J, L) = 3.0*A(I, J, L) + B(I, J, L)- sin(A(I,J,L)) + log(B(I,J,L))

    end subroutine

    subroutine print_C_sum()
!$omp target update from(C)
        print*,'SUM(C) = ', SUM(C)
    end subroutine

end module

! program mainProgram
!     use compute_kernel
!     implicit none

!     integer :: II, JJ, LL, I, J, L, num_args
!     integer(kind=8) :: t_start, t_end
!     real(kind=8) :: rate
!     character(len=12), dimension(:), allocatable :: args

!     ! Retrieve number of command line arguments
!     num_args = command_argument_count()

!     if(num_args .ne. 3) then
!         print*, 'Need exactly 3 integer arguments : II JJ LL'
!         call exit(1)
!     else
!         allocate(args(3))

!         call get_command_argument(1,args(1))
!         call get_command_argument(2,args(2))
!         call get_command_argument(3,args(3))

!         read(args(1),"(I10)") II
!         read(args(2),"(I10)") JJ
!         read(args(3),"(I10)") LL

!         print*,"II = ", II
!         print*,"JJ = ", JJ
!         print*,"LL = ", LL
!     endif

!     call allocate_memory(II, JJ, LL)
!     print*,'Memory allocate'
!     call initialize_arrays()
!     print*,'Memory initialized'

!     print*,'Starting kernel'
!     call system_clock(t_start,rate)

! !!$omp parallel do collapse(3)
! !$omp target teams distribute parallel do simd collapse(3) &
! !$omp              private(I, J, L)
! !!$omp target teams loop collapse(3)
!     do L = 1, LL
!         do J = 1, JJ
!             do I = 1, II
!                 call call_kernel(I, J, L)
!             enddo
!         enddo
!     enddo
! !$omp end target teams distribute parallel do simd
! !!$omp end target teams loop
! !!$omp end parallel do
    
!     call system_clock(t_end)
!     print*,'Kernel finished'
!     call print_C_sum()
!     print*,'Elapsed Time = ', (t_end - t_start)/rate
! end program

program mainProgram
    ! use compute_kernel
    use omp_lib
    implicit none

    integer :: II, JJ, LL, I, J, L, num_args
    integer(kind=8) :: t_start, t_end
    real(kind=8) :: rate
    character(len=12), dimension(:), allocatable :: args

    real*8, dimension(:,:,:), allocatable :: A, B, C

    ! Retrieve number of command line arguments
    num_args = command_argument_count()

    if(num_args .ne. 3) then
        print*, 'Need exactly 3 integer arguments : II JJ LL'
        call exit(1)
    else
        allocate(args(3))

        call get_command_argument(1,args(1))
        call get_command_argument(2,args(2))
        call get_command_argument(3,args(3))

        read(args(1),"(I10)") II
        read(args(2),"(I10)") JJ
        read(args(3),"(I10)") LL

        print*,"II = ", II
        print*,"JJ = ", JJ
        print*,"LL = ", LL
    endif

    allocate(A(II,JJ,LL))
    allocate(B(II,JJ,LL))
    allocate(C(II,JJ,LL))

    print*,'Memory allocated'
    A = 1.0
    B = 2.0
    C = 0.0
    print*,'Memory initialized'



    !!$omp target enter data map(to:A,B,C)

!!$omp parallel do collapse(3)
!$omp target data map(to: A,B) map(from: C)

    print*,'Number of offload devices available: ', omp_get_num_devices()
! !$omp target teams
!     print*,'Number of teams set to : ', omp_get_num_teams()
! !$omp target teams
    print*,'Starting kernel'
    call system_clock(t_start,rate)

!$omp target teams num_teams(32)
!$omp distribute parallel do simd simdlen(128) collapse(3)
!!$omp target teams distribute parallel do simd collapse(3)
!!$omp target teams loop collapse(3)
    do L = 1, LL
        do J = 1, JJ
            do I = 1, II
                C(I, J, L) = 3.0*A(I, J, L) + B(I, J, L)- sin(A(I,J,L)) + log(B(I,J,L))
                C(I, J, L) = C(I,J,L) + 3.0*A(I, J, L) + B(I, J, L)- sin(A(I,J,L)) + log(B(I,J,L))
                C(I, J, L) = 2.0*C(I,J,L) - 1.5*A(I, J, L) + B(I, J, L)- sin(A(I,J,L)) + log(B(I,J,L))
            enddo
        enddo
    enddo
!$omp end distribute parallel do simd
!$omp end target teams
!!$omp end target teams distribute parallel do simd
    call system_clock(t_end)
    print*,'Kernel finished'
!$omp end target data
!!$omp end target teams loop
!!$omp end parallel do
    

    !!$omp target update from(C)
    print*,'SUM(C) = ', SUM(C)
    print*,'Elapsed Time = ', (t_end - t_start)/rate
end program