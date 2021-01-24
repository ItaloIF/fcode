program al
    implicit none
    real(8), allocatable :: a(:,:)
    integer :: n,i

    read(*,*) n
    allocate(a(n,n))
    a = 1.01
    do i = 1,n 
        write(*,'(*(f6.2))') a(i,:)
    end do

end program al