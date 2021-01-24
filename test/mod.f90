module modulo
    implicit none
    integer(8) :: i
    real, parameter :: pi = 3.1415
    contains
    subroutine sqr2(x,s)
        implicit none
        real, intent(in) :: x
        real, intent(out) :: s
        s = x*x
    end subroutine sqr2
end module modulo