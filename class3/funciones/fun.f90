function sqr(x) result(s)
    implicit none
    real, intent(in) :: x
    real :: s
    s = x*x
    ! write(*,*) 'hola'
end function sqr

subroutine sqr2(x,s)
    implicit none
    real, intent(in) :: x
    real, intent(in out) :: s
    s = x*x
end subroutine sqr2