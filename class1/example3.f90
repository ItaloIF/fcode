program main
    implicit none
    real(8) :: a
    real(8) :: b
    real(8) :: c

    a = 2.4_8
    b = 3.5_8
    c = (a+b)**a
    write(*,*) c
end program main