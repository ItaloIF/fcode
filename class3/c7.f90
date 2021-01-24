program str
    !use mod
    implicit none
    type per
        character(10) :: nm
        integer :: alt
        real(4) :: notas(5)
    end type per

    integer :: i,n
    type(per) :: al1(5)

    al1(1)%alt = 1.45
    al1(1)%nm = 'carlos'
    al1(1)%notas = 15.5
    al1(1)%notas(1) = 14.5

    write(*,*) al1%notas
end program