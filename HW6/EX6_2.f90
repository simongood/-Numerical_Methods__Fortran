program EX6_2
implicit none
real :: linear
real :: xi, yi, x1, x2, y1, y2
real :: fx1y1, fx2y1, fx1y2, fx2y2
real :: fxiy1, fxiy2, fxiyi

x1 = 2
x2 = 9
y1 = 1
y2 = 6
fx1y1 = 60
fx2y1 = 57.5
fx1y2 = 55
fx2y2 = 70

xi = 5.25
yi = 4.8
fxiy1 = linear(xi, x1, x2, fx1y1, fx2y1)
fxiy2 = linear(xi, x1, x2, fx1y2, fx2y2)
fxiyi = linear(yi, y1, y2, fxiy1, fxiy2)

print*, fxiyi

end program EX6_2

function linear(xi, x1, x2, fx1, fx2) 
real :: linear
real :: xi, x1, x2, fx1, fx2 
	linear = ((xi-x2)/(x1-x2))*fx1 + ((xi-x1)/(x2-x1))*fx2
end function linear