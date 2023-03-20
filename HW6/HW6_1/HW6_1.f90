program HW6_1
implicit none
integer, parameter :: n = 6
real :: x(n), f(n,n), fx(n-1)
real :: xr
integer :: i, j


x = [1.6, 2., 2.5, 3.2, 4., 4.5]
f(1,:) = [2., 8., 14., 15., 8., 2.]

do i = 2, n
	do j = 1, n-i+1
		f(i,j) = (f(i-1,j+1) - f(i-1,j))/(x(j+i-1) - x(j))
	end do
end do 

xr = 2.8
! fx(1) = f(1,1) + f(2,1)*(xr-x(1))
! fx(2) = fx(1) + f(3,1)*(xr-x(1))*(xr-x(2))
! fx(3) = fx(2) + f(4,1)*(xr-x(1))*(xr-x(2))*(xr-x(3))
! fx(4) = fx(3) + f(5,1)*(xr-x(1))*(xr-x(2))*(xr-x(3))*(xr-x(4))
! fx(5) = fx(4) + f(6,1)*(xr-x(1))*(xr-x(2))*(xr-x(3))*(xr-x(4))*(xr-x(5))

do i = 1, n-1
	fx(i) = f(i+1,1)
	do j = 1, i
		fx(i) = fx(i)*(xr-x(j))
	end do
	if (i>1) then
		fx(i) = fx(i) + fx(i-1)
	else 
		fx(i) = fx(i) + f(1,1)
	end if
end do

do i = 1, n
	print*, f(:,i)
end do

print*, '1 = ', fx(1)
print*, '2 = ', fx(2)
print*, '3 = ', fx(3)
print*, '4 = ', fx(4)
print*, '5 = ', fx(5)
end program HW6_1