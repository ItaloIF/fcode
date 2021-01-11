program for
    implicit none
    integer(4) :: n
    integer(4) :: i
    integer(4) :: a
    n = 10
    a = 0
    do i = 1,n
        a = a + i
    end do
    write(*,*) 'el resultado :', a
end program for