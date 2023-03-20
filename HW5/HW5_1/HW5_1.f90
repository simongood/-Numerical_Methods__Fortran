program HW5_1
implicit none
integer, parameter :: n = 4, n1 = 8
real(8) :: x(n1), y(n1)
real(8) :: Exi, Eyi, Exi2, Exi3, Exi4, Exi5, Exi6, Exiyi, Exi2yi, Exi3yi, avgy
real(8) :: A(n,n), B(n), aa(n)
real(8) :: Sr, Syx, Eyi_avgy2, r2, r
real(8) :: yr(n1)
integer :: i, j, k

x = [3., 4., 5., 7., 8., 9., 11., 12.]
y = [1.6, 3.6, 4.4, 3.4, 2.2, 2.8, 3.8, 4.6]

Exi = sum(x)
Eyi = sum(y)
Exi2 = sum(x**2)
Exi3 = sum(x**3)
Exi4 = sum(x**4)
Exi5 = sum(x**5)
Exi6 = sum(x**6)
Exiyi = sum(x*y)
Exi2yi = sum((x**2)*y)
Exi3yi = sum((x**3)*y)
avgy = Eyi/n1

!! 高斯
A = reshape([8.d0, Exi, Exi2, Exi3, Exi, Exi2, Exi3, Exi4, Exi2, Exi3, Exi4, Exi5, Exi3, Exi4, Exi5, Exi6], [n,n])
B = [Eyi, Exiyi, Exi2yi, Exi3yi]
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
Sr = sum((y-aa(1)-aa(2)*x-aa(3)*x**2-aa(4)*x**3)**2)
Syx = SQRT(Sr / (n1-n))
Eyi_avgy2 = sum((y-avgy)**2)
r2 = (Eyi_avgy2 - Sr)/Eyi_avgy2
r = SQRT(r2)

print*,'sy/x =', Syx
print*,'r = ', r

!! 寫入檔案
open (1, file='point.txt')
do i = 1, n1
	write (1,*) x(i), y(i)
end do
close(1)

do i = 1, n1
	yr(i) = aa(1)+aa(2)*x(i)+aa(3)*x(i)**2+aa(4)*x(i)**3

enddo

open (1, file='point1.txt')
do i = 1, n1
	write (1,*) x(i), yr(i)
end do
close(1)


end program HW5_1