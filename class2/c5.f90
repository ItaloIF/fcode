program name1
    implicit none
    real(8):: a,b,c
    character(len=10) :: name
    read(*,*) name
    open(1,file = trim(name)//'.txt',status='old')
    read(1,*) a,b
    write(*,*) a,b
    close(1)
end program name1