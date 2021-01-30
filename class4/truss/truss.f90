program truss
    use tool
    use sle
    implicit none
    integer :: i
    integer :: iel
    integer :: k
    integer ::loaded_nodes
    integer :: ndim
    integer :: ndof = 2
    integer :: nels
    integer :: neq
    integer :: nod = 2
    integer :: nodof
    integer :: nn
    integer :: nprops = 1
    integer :: np_types
    integer :: nr
    real(8) :: axial
    real(8) :: zero = 0.0_8
    integer, allocatable :: etype(:)
    integer, allocatable :: g(:)
    integer, allocatable :: g_g(:,:)
    integer, allocatable :: g_num(:,:)
    integer, allocatable :: nf(:,:)
    integer, allocatable :: num(:)
    real(8), allocatable :: action(:)
    real(8), allocatable :: coord(:,:)
    real(8), allocatable :: eld(:)
    real(8), allocatable :: g_coord(:,:)
    real(8), allocatable :: km(:,:)
    real(8), allocatable :: loads(:)
    real(8), allocatable :: prop(:,:)
    real(8), allocatable :: kg(:,:)
    character(len=15) :: file
    read(*,*) file
    open(10,file = trim(file)//'.txt')
    open(11,file = trim(file)//'_res.txt')
    read(10,*) nels, nn, ndim, np_types
    nodof = ndim
    ndof = nod*nodof
    allocate(nf(nodof,nn))
    allocate(km(ndof,ndof))
    allocate(coord(nod,ndim))
    allocate(g_coord(ndim,nn))
    allocate(g_num(nod,nels))
    allocate(num(nod))
    allocate(g(ndof))
    allocate(g_g(ndof,nels))
    allocate(etype(nels))
    allocate(prop(nprops,np_types))
    read(10,*) prop
    etype = 1
    if (np_types.GT.1) read(10,*) etype
    read(10,*) g_coord
    read(10,*) g_num
    nf = 1
    read(10,*) nr, (k, nf(:,k), i = 1,nr)
    call formnf(nf)
    neq = maxval(nf)
    allocate(kg(neq,neq))
    allocate(loads(0:neq))

    do iel = 1,nels
        num = g_num(:,iel)
        call num_to_g(num,nf,g)
        g_g(:,iel) = g
    end do

    kg = zero
    do iel = 1,nels
        num = g_num(:,iel)
        coord = transpose(g_coord(:,num))
        call ke_truss(km,prop(1,etype(iel)),coord)
        g = g_g(:,iel)
        call ensamg(kg,km,g)
    end do
    loads = zero
    read(10,*) loaded_nodes, (k,loads(nf(:,k)),i = 1,loaded_nodes)

    call sle_lu(kg,loads(1:))
    loads(0) = zero
    write(11,'(a)') " Node Displacement(s)"
    do k = 1,nn
        write(11,'(i5,3e12.4)') k, loads(nf(:,k))
    end do

    write(11,'(/a)') "Element Actions"
    do iel = 1,nels
        num = g_num(:,iel)
        coord = transpose(g_coord(:,num))
        g = g_g(:,iel)
        eld = loads(g)
        call ke_truss(km,prop(1,etype(iel)),coord)
        action = matmul(km,eld)
        write(11,'(i5,6e12.4)') iel, action
        call glob_to_axial(axial,action,coord)
        write(11,'(a,e12.4)') "Axial force = ", axial
    end do
end program truss