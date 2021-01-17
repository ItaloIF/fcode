program sub
    real :: a,b,c,e,f
    a = 5.6
    b = 7.2
    e = 6.7
    f = 5.2
    c = 1.3
    call suma2(a,b,c)
    call suma2(a,e,f)
    write(*,*) c,f
end program sub

subroutine suma(x,y,z)
    real :: x,y,z
    z = x + y
end subroutine suma

subroutine suma2(x,y,z)
    real, intent(in) :: x
    real, intent(in) :: y
    !real, intent(in out) :: z
    real, intent(out) :: z
    z = x + y
end subroutine suma2