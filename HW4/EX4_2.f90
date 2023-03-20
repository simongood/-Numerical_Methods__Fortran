program EX4_2
implicit none
integer, parameter :: n = 4
real(4) :: f(4), e(4), g(4), x(4), r(4)
integer :: i

f = [2.04, 2.04, 2.04, 2.04]
e = [0., -1., -1., -1.]
g = [-1., -1., -1., 0.]
r = [40.8, 0.8, 0.8, 200.8]



do i = 2, n
        f(i) = f(i) - (e(i)/f(i-1))*g(i-1)
        r(i) = r(i) - (e(i)/f(i-1))*r(i-1)
        e(i) = 0
enddo

x(n) = r(n)/f(n)
do i = n-1, 1, -1
        x(i) = (r(i) - g(i)*x(i+1))/f(i)
end do

print*, x

end program EX4_2