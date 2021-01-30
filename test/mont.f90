program mont
    integer(8) :: n
    integer(8) :: m
    integer(8) :: i
    real(8) :: x
    real(8) :: y
    m = 0
    n = 100000000
    do i = 1,n
        call random_number(x)
        call random_number(y)
        if ((x*x+y*y).le.1.0_8) m = m + 1
    end do

    write(*,*) 4.0_8*m/n

end program mont