program c4
    implicit none
    integer :: i,n,m
    n = 10
    i = 1
    ! do i = 1,10
    !     write(*,*) i
    !     if (i.eq.5) cycle
    !     write(*,*) i
    ! end do
    do 
        write(*,*) i
        if (i.eq.5) exit
        write(*,*) i
        i = i+1
    end do

end program c4

