program EX5_1
implicit none
integer, parameter :: n = 7
real(4) :: x(7), y(7)
real(4) :: Exi, Cx, Eyi, Cy, Exiyi, Exi2
real(4) :: a1, a0
real(4) :: y1, y7 ! x代1,2後得出的y值
integer :: i


x = [1., 2., 3., 4., 5., 6., 7.]
y = [0.5, 2.5, 2.0, 4.0, 3.5, 6.0, 5.5]

do i = 1, n
	Exiyi = Exiyi + x(i)*y(i)
	Exi2 = Exi2 + x(i)*x(i)
	Exi = Exi + x(i)
	Eyi = Eyi + y(i)
	
end do
Cx = Exi/n
Cy = Eyi/n


a1 = (n*Exiyi-Exi*Eyi)/(n*Exi2-(Exi)**2)
a0 = Cy -a1*Cx

y1 = a0 + a1*1.
y7 = a0 + a1*7.

do i = 1, n
	! 寫入檔案
	open (1, file='point.txt')
	write (1,*) x(i), y(i)
end do
close(1)

! 寫入檔案
open (1, file='point1.txt')
write (1,*) 1, y1
write (1,*) 7, y7
close(1)


print*, a0, a1
print*, y1, y7

end program EX5_1