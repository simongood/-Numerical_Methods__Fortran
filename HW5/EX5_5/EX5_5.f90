program EX5_5
implicit none
integer, parameter :: n = 3, n1 = 9
real(4) :: x(n1), y(n1), x2(n1)
real(4) :: Exi, Eyi, Exi2, Exiyi, Exix2i, EX2i2, Ex2i, Ex2iyi
real(4) :: A(n,n), B(n), aa(n)
real(4) :: Sr, Syx, Eyi_avgy2, r2, r, avgy
integer :: i, j, k

x = [0.,1., 1., 2., 2., 3., 3., 4., 4.]
x2 = [0., 1., 2., 1., 2., 1. , 2., 1., 2.]
y = [15.1, 17.9, 12.7, 25.6, 20.5, 35.1, 29.7, 45.4, 40.2]

Exi = sum(x)
Ex2i = sum(x2)
Exi2 = sum(x**2)
Exix2i = sum(x*x2)
EX2i2 = sum(x2**2)
Eyi = sum(y)
Exiyi = sum(x*y)
Ex2iyi = sum(x2*y)
avgy = Eyi/n1


!! 高斯
A = reshape([float(n1), Exi, Ex2i, Exi, Exi2, Exix2i, Ex2i, Exix2i, Ex2i2], [n,n])
B = [Eyi, Exiyi, Ex2iyi]
do k = 1, n-1
	do i = k+1, n
		do j = k+1, n
			A(i,j) = A(i,j) - (A(i,k)/A(k,k))*A(k,j)			
		end do
		B(i) = B(i) - (A(i,k)/A(k,k))*B(k)
	end do
end do
aa(n) = B(n)/A(n,n)

do i = n-1, 1, -1
	do j = i+1, n
		aa(i) = aa(i) - A(i,j)*aa(j)
	end do
	aa(i) = (aa(i) + B(i))/A(i,i)
end do
print*, aa(1:n)


!! Sr, r2
Sr = sum((y-aa(1)-aa(2)*x-aa(3)*x2)**2)
Syx = SQRT(Sr / (n1-n))
Eyi_avgy2 = sum((y-avgy)**2)
r2 = (Eyi_avgy2 - Sr)/Eyi_avgy2
r = SQRT(r2)

print*,'sy/x =', Syx
print*,'r = ', r

end program EX5_5