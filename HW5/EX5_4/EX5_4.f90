program EX5_4
implicit none
integer, parameter :: n = 3, n1 = 6
real(4) :: x(n1), y(n1)
real(4) :: Exi, Eyi, Exi2, Exi3, Exi4, Exiyi, Exi2yi, avgy
real(4) :: A(3,3), B(3), aa(3)
real(4) :: Sr, Syx, Eyi_avgy2, r2
integer :: i, j, k

x = [0.,1.,2.,3.,4.,5.]
y = [2.1, 7.7, 13.6, 27.2, 40.9, 61.1]

Exi = sum(x)
Eyi = sum(y)
Exi2 = sum(x**2)
Exi3 = sum(x**3)
Exi4 = sum(x**4)
Exiyi = sum(x*y)
Exi2yi = sum((x**2)*y)
avgy = Eyi/n1

!! 高斯
A = reshape([float(n1), Exi, Exi2, Exi, Exi2, Exi3, Exi2, Exi3, Exi4], [3,3])
B = [Eyi, Exiyi, Exi2yi]
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
Sr = sum((y-aa(1)-aa(2)*x-aa(3)*x**2)**2)
Syx = SQRT(Sr / (n1-n))
Eyi_avgy2 = sum((y-avgy)**2)
r2 = (Eyi_avgy2 - Sr)/Eyi_avgy2

print*, Syx, r2


end program EX5_4