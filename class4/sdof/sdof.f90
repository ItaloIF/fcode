module sdof_im
    contains

    !-------------------------------------------------------------
    !   sdof_cd: Central Diference Method
    !-------------------------------------------------------------
    subroutine sdof_cd (m, c, k, f, dt, na, u)
        implicit none
        integer(4) :: na
        real(4) :: dt
        real(4) :: m
        real(4) :: c
        real(4) :: k
        real(4), dimension(na) :: f
        real(4), dimension(na,3) :: u

        integer(4) :: i
        real(4) :: u0
        real(4) :: k1
        real(4) :: a
        real(4) :: b
        real(4) :: p1

        u(1,3) = (f(1) - c*u(1,2) - k*u(1,1))/m
        u0 = u(1,1) - dt*(u(1,2)) 

        u(1,3) = (f(1) - c*u(1,2) - k*u(1,1))/m
        u0 = u(1,1) - dt*u(1,2) + (dt**2/2)*u(1,3)
        k1 = m/(dt**2) + c/(2*dt)
        a = m/(dt**2) - c/(2*dt)
        b = k - 2*m/(dt**2)

        p1 = f(1) - a*u0 - b*u(1,1)
        u(2,1) = p1/k1

        do i = 2,na-1
            p1 = f(i) - a*u(i-1,1) - b*u(i,1)
            u(i+1,1) = p1/k1
            u(i,2) = (u(i+1,1) - u(i-1,1))/(2*dt)
            u(i,3) = (u(i+1,1) - 2*u(i,1) + u(i-1,1))/(dt**2)
        end do
        p1 = f(na) - a*u(na-1,1) - b*u(na,1)
        u0 = p1/k1
        u(na,2) = (u0- u(na-1,1))/(2*dt)
        u(na,3) = (u0 - 2*u(na,1) + u(na-1,1))/(dt**2)
    end subroutine sdof_cd

    !-------------------------------------------------------------
    !   sdof_nw: Newmark Method
    !-------------------------------------------------------------
    subroutine sdof_nw (m, c, k, f, dt, na, u, gamma, betha)
        implicit none
        integer(4) :: na
        real(4) :: dt
        real(4) :: m
        real(4) :: c
        real(4) :: k
        real(4), dimension(na) :: f
        real(4), dimension(na,3) :: u
        real(4) :: gamma
        real(4) :: betha

        integer(4) :: i
        real(4) :: a1
        real(4) :: a2
        real(4) :: a3
        real(4) :: k1
        real(4) :: p1

        u(1,3) = (f(1) - c*u(1,2) - k*u(1,1))/m
        a1 = m/(betha*dt**2) + gamma*c/(betha*dt)
        a2 = m/(betha*dt) + (gamma/betha - 1)*c
        a3 = (1/(2*betha)-1)*m + dt*(gamma/(2*betha)-1)*c
        k1 = k + a1
        
        do i = 1,na-1
            p1 = f(i+1) + a1*u(i,1) + a2*u(i,2) + a3*u(i,3)
            u(i+1,1) = p1/k1
            u(i+1,2) = gamma/(betha*dt)*(u(i+1,1)-u(i,1)) + (1-gamma/betha)*u(i,2) + dt*(1-gamma/(2*betha))*u(i,3)
            u(i+1,3) = (u(i+1,1)-u(i,1))/(betha*dt**2) - u(i,2)/(betha*dt) - (1/(2*betha)-1)*u(i,3)
        end do
    end subroutine sdof_nw


    !-------------------------------------------------------------
    !   sdof_ws: Wilson's Method
    !-------------------------------------------------------------
    !theta = 1, linear acceeration method (stable if dt<0.551Tn).
    !theta >= 1.37, this method is unconditionally stable.
    !theta = 1.42 gives optimal accuracy.
    !A. Chopra(2007) Dynamics of Structures(3rd), pp 625.
    subroutine sdof_ws(nt,dt,theta,m,c,k,F,U)
        integer(8) :: nt
        real(8) :: dt
        real(8) :: theta
        real(8) :: m
        real(8) :: c
        real(8) :: k
        real(8), dimension(nt) :: F
        real(8), dimension(nt,3) :: U

        integer(8) :: i
        real(8) :: a
        real(8) :: b
        real(8) :: k1
        real(8) :: dp
        real(8) :: du1
        real(8), dimension(3) :: Du
        
        U(1,3) = (F(1) - c*U(1,2) - k*U(1,1))/m
        a = 6*m/(theta*dt) + 3*c
        b = 3*m + theta*dt*c/2
        k1 = k + a/(theta*dt)
        do i = 1,nt-1
            dp = theta*(F(i+1)-F(i)) + a*U(i,2) + b*U(i,3)
            du1 = dp/k1
            Du(3) = (6*du1/(theta*dt)**2 - 6*U(i,2)/(theta*dt) - 3*U(i,3))/theta
            Du(2) = dt*U(i,3) + dt*Du(3)/2
            Du(1) = dt*U(i,2) + dt**2*U(i,3)/2 + dt**2*Du(3)/6
            U(i+1,:) = U(i,:) + Du
        end do
    end subroutine sdof_ws

    subroutine res_esp (dt, dr, ag, na, ts, nt, sr)
        implicit none
        integer(4) :: na
        integer(4) :: nt
        real(4) :: dt
        real(4) :: dr
        real(4), dimension(na) :: ag
        real(4), dimension(nt) :: ts
        real(4), dimension(nt,3) :: sr
        real(4), parameter :: pi = 4*atan(1.0_4)

        integer(4) :: i
        real(4) :: m
        real(4) :: k
        real(4) :: c
        real(4) :: w
        real(4), dimension(na,3) :: u
        real(4), dimension(na) :: f
        m = 1
        f = -ag*m
        do i = 1,nt
            u = 0
            w = 2*pi/ts(i)
            c = 2*dr*w*m
            k = w**2*m
            call sdof_nw (m, c, k, f, dt, na, u, 0.5,0.25)
            sr(i,1) = maxval(abs(u(:,1)))
            sr(i,2) = maxval(abs(u(:,2)))
            sr(i,3) = maxval(abs(u(:,3)+ag))
        end do
    end subroutine res_esp

end module sdof_im