integer(4), allocatable, dimension(:) :: a
integer(4), allocatable :: b(:)
real(8), allocatable, dimension(:,:) :: c
real(4), allocatable :: d(:)
character(len=:) :: e


allocate(a(10))
allocate(b(15))
n = 12
m = 10
allocate(c(n,m))
allocate(d(0:n))
allocate(character(15) :: e)