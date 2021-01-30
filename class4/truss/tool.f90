module tool
    contains

    subroutine formnf(nf)
        implicit none
        integer(4), intent(in out) :: nf(:,:)
        integer(4) :: i
        integer(4) :: j
        integer(4) :: m
        
        m = 0
        do  j = 1,UBOUND(nf,2)
            do i = 1,UBOUND(nf,1)
                if (nf(i,j) /= 0) then
                    m = m+1
                    nf(i,j) = m
                end if
            end do
        end do
        return
    end subroutine formnf

    subroutine num_to_g(num, nf, g)
        implicit none
        integer(4), intent(in) :: num(:)
        integer(4), intent(in) :: nf(:,:)
        integer(4), intent(out) :: g(:)
        integer(4) :: i
        integer(4) :: k
        integer(4) :: nod
        integer(4) :: nodof
    
        nod = UBOUND(num,1)
        nodof = UBOUND(nf,1)
        do i = 1,nod
            k = i*nodof
            g(k-nodof+1:k) = nf(:,num(i))
        end do
        return
    end subroutine num_to_g

    subroutine ke_truss(km, ea, coord)
        ! This subroutine forms the stiffness matrix of a general rod element (1-, 2- or 3-d).
            implicit none
            real(8), intent(in) :: ea
            real(8), intent(in) :: coord(:,:)
            real(8), intent(out) :: km(:,:)
            integer(4) :: ndim
            real(8) :: ell
            real(8) :: cs
            real(8) :: sn
            real(8) :: x1
            real(8) :: x2
            real(8) :: y1
            real(8) :: y2
            real(8) :: z1
            real(8) :: z2
            real(8) :: a
            real(8) :: b
            real(8) :: c
            real(8) :: d
            real(8) :: e
            real(8) :: f
            real(8) :: xl
            real(8) :: yl
            real(8) :: zl
            real(8) :: one = 1.0_8
            ndim = UBOUND(coord,2)
            select case (ndim)
            case(1)
                ell = coord(2,1)-coord(1,1)
                km(1,1) = one
                km(1,2) = -one
                km(2,1) = -one
                km(2,2) = one
            case(2)
                x1 = coord(1,1)
                y1 = coord(1,2)
                x2 = coord(2,1)
                y2 = coord(2,2)
                ell = SQRT((y2-y1)**2+(x2-x1)**2)
                cs = (x2-x1)/ell
                sn = (y2-y1)/ell
                a = cs*cs
                b = sn*sn
                c = cs*sn
                km(1,1) = a
                km(3,3) = a
                km(1,3) = -a
                km(3,1) = -a
                km(2,2) = b
                km(4,4) = b
                km(2,4) = -b
                km(4,2) = -b
                km(1,2) = c
                km(2,1) = c
                km(3,4) = c
                km(4,3) = c
                km(1,4) = -c
                km(4,1) = -c
                km(2,3) = -c
                km(3,2) = -c
            case(3)
                x1 = coord(1,1)
                y1 = coord(1,2)
                z1 = coord(1,3)
                x2 = coord(2,1)
                y2 = coord(2,2)
                z2 = coord(2,3)
                xl = x2-x1
                yl = y2-y1
                zl = z2-z1
                ell = SQRT(xl*xl+yl*yl+zl*zl)
                xl = xl/ell
                yl = yl/ell
                zl = zl/ell
                a = xl*xl
                b = yl*yl
                c = zl*zl
                d = xl*yl
                e = yl*zl
                f = zl*xl
                km(1,1) = a
                km(4,4) = a
                km(2,2) = b
                km(5,5) = b
                km(3,3) = c
                km(6,6) = c
                km(1,2) = d
                km(2,1) = d
                km(4,5) = d
                km(5,4) = d
                km(2,3) = e
                km(3,2) = e
                km(5,6) = e
                km(6,5) = e
                km(1,3) = f
                km(3,1) = f
                km(4,6) = f
                km(6,4) = f
                km(1,4) = -a
                km(4,1) = -a
                km(2,5) = -b
                km(5,2) = -b
                km(3,6) = -c
                km(6,3) = -c
                km(1,5) = -d
                km(5,1) = -d
                km(2,4) = -d
                km(4,2) = -d
                km(2,6) = -e
                km(6,2) = -e
                km(3,5) = -e
                km(5,3) = -e
                km(1,6) = -f
                km(6,1) = -f
                km(3,4) = -f
                km(4,3) = -f
            end select
            km = km*ea/ell
            return
    end subroutine ke_truss

    subroutine ensamg(kg,km,g)
        real(8), intent(in out) :: kg(:,:)
        real(8), intent(in) :: km(:,:)
        integer, intent(in) :: g(:)

        integer :: n
        integer :: i
        integer :: j
        integer :: k
        integer :: l

        n = ubound(km,1)

        do i = 1,n
            k = g(i)
            if (k.ne.0) then
                do j = 1,n
                    l = g(j)
                    if (l.ne.0) then
                        kg(k,l) = kg(k,l) + km(i,j)
                    end if
                end do
            end if
        end do
        return
    end subroutine ensamg

    subroutine glob_to_axial(axial, global, coord)
        ! This subroutine transforms the global end reactions into an axial force for rod elements (2- or 3-d).
        implicit none
        real(8), intent(in) :: global(:)
        real(8), intent(in) :: coord(:,:)
        real(8), intent(out) :: axial
        real(8) :: add
        real(8) :: ell
        real(8) :: zero = 0.0_8
        integer(8) :: ndim
        integer(8) :: i
        ndim = ubound(coord,2)
        add = zero
        do i = 1,ndim
            add  = add+(coord(2,i)-coord(1,i))**2
        end do
        ell = sqrt(add)
        axial = zero
        do i = 1,ndim
            axial = axial+(coord(2,i)-coord(1,i))/ell*global(ndim+i)
        end do
        return
    end subroutine glob_to_axial
end module tool