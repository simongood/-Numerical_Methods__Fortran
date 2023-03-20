program sine
implicit none
integer :: i, n, base
real :: x, pi, sinx

pi = 4*atan(1.0)
x = pi/6

do i = 1, 7, 2
	base = 1
	n = i
	do while (n /= 0)
		base = base*n
		n = n - 1
	end do
	
	if ( i == 1 .or. i == 5) then
		sinx = x**i/base + sinx
	else
		sinx = -x**i/base + sinx
	end if
	
	print *, i, sinx
end do 

end program sine


