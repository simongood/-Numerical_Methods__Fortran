program HW6_2
implicit none
integer, parameter :: n = 5  ! (1-order, n=2), (2-order, n=3).... 
real(16) :: x(n), f(n), xr
real(16) :: xpoint(21), ypoint(21), yLpoint(21) ! 取點
integer :: i, j, k

do i = 1, 21
	xpoint(i) = -1. + (i-1)*0.1
	ypoint(i) = 1/(1+25*xpoint(i)**2)
	print*, xpoint(i), ypoint(i)
end do


! Lagrange 方程, 輸入xr值得值

do k = 1, 21
	xr = xpoint(k)
	x = [-1., -0.5, 0., 0.5, 1.]
	do i = 1, n
		f(i) = 1/(1+25*x(i)**2)
	end do
	do j = 1, n
		do i = 1, n
			if (i /= j) then
				f(j) = f(j)*(xr-x(i))/(x(j)-x(i))
			end if
		end do
		yLpoint(k) = yLpoint(k) +f(j)
	end do
	print*, xpoint(k), yLpoint(k)
end do

!! 寫入檔案
open (1, file='point.txt')
do i = 1, 21
	write (1,*) xpoint(i), ypoint(i), yLpoint(i)
end do
close(1)

end program HW6_2