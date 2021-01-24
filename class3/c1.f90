program c1
    real(8) :: a,b
    integer(8) :: n

    a = 3.1873457835
    b = 345.45654645
    n = 675
    ! write(*,'(1f9.3)') a
    ! write(*,'(1i12.6)') n
    write(*,100) a


    write(*,200) a
    write(*,300) a



    100 format(1f9.3)
    200 format(1e9.3)
    300 format(1es9.3)
end program c1