program main
    real :: a,b,c
    a = 4.0
    b = sqr(a)
    call sqr2(a,c)
    write(*,*) b, c
end program main

include 'funciones/fun.f90'