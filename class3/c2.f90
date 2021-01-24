program cad
    character(10) :: cd1, cd2
    character(len=25) :: cd3

    cd1 = 'cadena'
    read(*,*) cd2
    ! cd3 = cd1//cd2
    cd3 = trim(cd1)//cd2
    write(*,*) cd3
    write(*,*) cd3(5:6)


end program cad
