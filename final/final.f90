program final
implicit none
integer, parameter :: n = 9, ni = 100.
real(4) :: x(n), y(n)
real(4) :: lny_x(n)
real(4) :: Exi, avgx, Elnyi_xi, avglny_x, Exilnyi_xi, Exi2
real(4) :: A, B
real(4) :: xpoint(ni+1), ypoint(ni+1) ! 取點
integer :: i

x = [0.1, 0.2, 0.4, 0.6, 0.9, 1.3, 1.5, 1.7, 1.8]
y = [0.75, 1.25, 1.45, 1.25, 0.85, 0.55, 0.35, 0.28, 0.18]

lny_x = Alog(y/x)


Exi = sum(x)
avgx = Exi/n
Elnyi_xi = sum(lny_x)
avglny_x = Elnyi_xi/n
Exilnyi_xi = sum(x*lny_x)
Exi2 = sum(x**2)


B = (n*Exilnyi_xi - Exi*Elnyi_xi)/(n*Exi2 - Exi**2)
A = exp(avglny_x - B*avgx)

print*,A, B

! 取點
do i = 1, ni+1
	xpoint(i) = (i-1)*(2./ni)
	ypoint(i) = A*xpoint(i)*exp(B*xpoint(i))
	print*, xpoint(i), ypoint(i)
end do


! 寫入檔案
open (1, file='point.txt')
do i = 1, ni+1
	write (1,*) xpoint(i), ypoint(i)
end do
close(1)

!! 原
open (1, file='point1.txt')
do i = 1, n
	write (1,*) x(i), y(i)
end do
close(1)

end program final