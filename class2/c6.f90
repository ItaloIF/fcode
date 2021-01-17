program name1
    implicit none
    real(8):: a,b,c
    character(len=10) :: name
    read(*,*) name
    open(1,file = trim(name)//'.txt',status='old')
    open(2,file = 'out.txt',status='new')
    read(1,*) a,b
    write(2,*) a,b
    close(1)
    close(2)
end program name1