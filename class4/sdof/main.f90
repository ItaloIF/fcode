program main
    use sdof_im
    implicit none
    integer(4) :: i
    integer(4) :: nt
    real(4) :: dt
    real(4) :: tr
    real(4), allocatable :: ag(:)
    real(4), allocatable :: f(:)
    real(4), allocatable :: u(:,:)
    real(4) :: t
    real(4) :: am
    real(4), parameter :: pi = 4*atan(1.0_8)
    real(4) :: wn
    real(4) :: m
    real(4) :: c
    real(4) :: k
    open(1, file='EW.txt', status='old')
    nt = 0
    do 
        read(1,*,end=100)
        nt = nt + 1
    end do
100 close(1)
    allocate(ag(nt))
    allocate(u(nt,3))
    allocate(f(nt))

    open(1, file='EW.txt', status='old')
    do i = 1,nt
        read(1,*) tr, ag(i)
    end do
    close(1)

    dt = 0.02
    am = 0.05
    m = 1
    t = 0.2
    wn = 2*pi/t
    c = 2*am*wn
    k = wn**2
    f = -ag*m
    call sdof_cd(m, c, k, f, dt, nt, u)
    open(1, file='res.txt', status='unknown')
    write(1,*) '       desp         vel       accel'
    do i = 1,nt
        write(1,120) u(i,:)
    end do
    close(1)

    120 format(3f12.3)
end program main