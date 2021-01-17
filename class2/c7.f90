program name
    implicit none
    integer :: i
    real(8) :: a(10),b(4,4),c(3,3,3)
    open(1,file='out.txt',status='old')
    ! a = 1.8_8
    ! a(1) = 2.54_8
    ! do i = 1,10
    !     a(i) = 1.25*i
    ! end do
    ! a(1:4) = 2.54
    ! a(5:10) = 1.25
    b = 1.01
    do i = 1,4
        write(1,'(f20.5)') b(i,:)
    end do
    ! do i = 1,4
    !     write(1,200) b(i,:)
    ! end do
    ! 200 format((f20.5))
end program name