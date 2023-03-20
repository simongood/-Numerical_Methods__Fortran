program HW4_2
implicit none
integer, parameter :: n = 5
real(4) :: A(5,5), B(5), x(n), temp(5), Btemp
integer :: i, j, k
integer :: bubble, lsup

A = reshape([2.,-1.,5.,3.,-4., -1.,3.,1.,-2.,-1., 4.,-2.,3.,-2.,-5., 1.,-1.,-4.,-2.,3., -1.,2.,1.,3.,-4.], [5,5])
B = [7., 1., 33., 24., -49.]

lsup = n
do while (lsup > 1)
    bubble = 0 !bubble in the greatest element out of order
    do j = 1, (lsup-1)
		if (ABS(A(j,1)) < ABS(A(j+1,1))) then
			temp = A(j,1:)
			Btemp = B(j)
			A(j,1:) = A(j+1,1:)
			B(j) = B(j+1)
			A(j+1,1:) = temp
			B(j+1) = Btemp
			bubble = j
		endif 
    enddo
    lsup = bubble   
enddo   
print *, A(1,1:), B(1)
print *, A(2,1:), B(2)
print *, A(3,1:), B(3)
print *, A(4,1:), B(4)
print *, A(5,1:), B(5)


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

print*, 'X:'
print*, x(1:)


end program HW4_2
