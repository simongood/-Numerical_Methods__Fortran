program HW4_1
implicit none
integer, parameter :: n = 3
real(4) :: A(3,3), B(3), x(n), xt(3)
integer :: i, j, k

A = reshape([3., 0.1, 0.3, -0.1, 7., -0.2, -0.2, -0.3, 10.], [3,3])
B = [7.85, -19.3, 71.4]
xt = [3., -2.5, 7.]

do k = 1, n-1
	do i = k+1, n
		do j = k+1, n
			A(i,j) = A(i,j) - (A(i,k)/A(k,k))*A(k,j)			
		end do
		B(i) = B(i) - (A(i,k)/A(k,k))*B(k)
	end do
end do

x(n) = B(n)/A(n,n)

do i = n-1, 1, -1
	do j = i+1, n
		x(i) = x(i) - A(i,j)*x(j)
	end do
	x(i) = (x(i) + B(i))/A(i,i)
end do


print*, x(1:n)
print*, 'relative error:'
do i = 1, n
	print*, (ABS(xt(i)-x(i)))/xt(i)
end do

end program HW4_1
