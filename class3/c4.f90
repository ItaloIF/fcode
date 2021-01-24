program main
    use mod1
    implicit none
    real :: a,b,c
    a = 4.0
    b = sqr(a)
    call sqr2(a,c)
    write(*,*) b, c
    write(*,*) pi
end program main
