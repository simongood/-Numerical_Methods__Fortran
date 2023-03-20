program EX7_1
implicit none
integer, parameter :: n = 6
real :: x(n+1), fx(n+1)
real :: ans, er, act
integer :: i

x(1) = 0.
x(n+1) = 0.8
do i = 2, n
	x(i) = x(1) + (i-1)*((x(n+1)-x(1))/n)
end do

do i = 1, n+1
	fx(i) = 0.2 + 25*x(i) - 200*x(i)**2 + 675*x(i)**3 - 900*x(i)**4 + 400*x(i)**5
end do

ans = ((x(2)-x(1))/2)*(sum(2*fx)-fx(1)-fx(n+1))
print*, ans

act = 1.640533
er = abs((act-ans)/act)
print*, 'er = ', er

end program EX7_1

