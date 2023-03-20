program EX6_1
implicit none
integer, parameter :: n = 3
real(4) :: x(n), fx(n)
real(4) :: b0, b1, b2
real(4) :: xx, f2x

x = [1., 4., 6.]
fx = [0., 1.386294, 1.791759]

b0 = fx(1)
b1 = (fx(2)-fx(1))/(x(2)-x(1))
b2 = ((fx(3)-fx(2))/(x(3)-x(2)) - (fx(2)-fx(1))/(x(2)-x(1)))/(x(3) - x(1))

xx = 2
f2x = b0 + b1*(xx - 1) + b2*(xx - 1)*(xx - 4)

print*, b0, b1, b2
print*, xx, f2x
end program EX6_1