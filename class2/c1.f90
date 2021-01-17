program c1
    implicit none
    real(8) :: a,b
    logical :: n
    a = 4.5
    b = 6.5
    n = .not.(a.lt.b)
    write(*,*) n

end program c1