program if
    implicit none
    real(8) :: a    !variable 2
    real(8) :: b    !variable 1
    a = 5.2_8
    b = 2.3_8
    if (b >= a) then   ! ==   !=   >=
        write(*,*) b
    else
        write(*,*) a
    end if

    if (b.GT.a) then   ! eq   ne  gt  lt  ge  le 
        write(*,*) b
    else if (b.ge.a) then
        write(*,*) a
    else
        write(*,*) a
    end if

end program if