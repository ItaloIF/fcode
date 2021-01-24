program str
    implicit none
    type persona
        character(len=30) :: nm
        character(len=40) :: ap
        integer :: edad
        real :: alt
        real :: notas(5)
    end type persona
    type (persona) :: p1
    p1%nm = 'juan'
    p1%ap = 'Soto'
    p1%edad = 22
    p1%alt = 1.74
    p1%notas(1) = 15.4
    p1%notas(2) = 12.4
end program str