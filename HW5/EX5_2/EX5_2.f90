program EX5_2
implicit none
integer, parameter :: n = 11
real(4) :: x(n), y(n)
real(4) :: Exi, Cx, Eyi, Cy, Exiyi, Exi2, Eyi2
real(4) :: a1, a0
real(4) :: y1, y7 ! x代1,2後得出的y值
integer :: i
real(4) :: sr, syx, r


x = [6., 7., 11., 15., 17., 21., 23., 29., 29., 37., 39.]
y = [29., 21., 29., 14., 21., 15., 7., 7., 13., 0., 3.]


do i = 1, n
	Exiyi = Exiyi + x(i)*y(i)
	Exi2 = Exi2 + x(i)*x(i)
	Eyi2 = Eyi2 + y(i)*y(i)
	Exi = Exi + x(i)
	Eyi = Eyi + y(i)
	
end do


Cx = Exi/n
Cy = Eyi/n


a1 = (n*Exiyi-Exi*Eyi)/(n*Exi2-(Exi)**2)
a0 = Cy -a1*Cx



y1 = a0 + a1*x(1)
y7 = a0 + a1*X(n)

do i = 1, n
	! 寫入檔案
	open (1, file='point.txt')
	write (1,*) x(i), y(i)
end do
close(1)

! 寫入檔案
open (1, file='point1.txt')
write (1,*) x(1), y1
write (1,*) x(n), y7
close(1)

do  i = 1, n
	sr = sr + (y(i)-a0-a1*x(i))**2 
end do
syx = SQRT(sr/(n-2))

r = (n*Exiyi-Exi*Eyi)/(SQRT(n*Exi2-(Exi)**2)*SQRT(n*Eyi2-(Eyi)**2))


print*, a0, a1
print*, y1, y7
print*, syx, r

end program EX5_2