
function sqr(x) result(s)
    real, intent(in) :: x
    real :: s
    s = x*x
    ! write(*,*) 'hola'
end function sqr

subroutine sqr2(x,s)
    real, intent(in) :: x
    real, intent(in out) :: s
    s = x*x
end subroutine sqr2


program main
    real :: a,b,c
    a = 4.0
    b = sqr(a)
    call sqr2(a,c)
    write(*,*) b, c

end program main
