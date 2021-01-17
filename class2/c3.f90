program c3
    implicit none
    integer(4) :: i

    i = 2

    select case(i)
    case(7)
        write(*,*) 'es 1'
    case(:5)
        write(*,*) 'es 2'
    case default 
        write(*,*) 'es otro'
    end select
end program c3