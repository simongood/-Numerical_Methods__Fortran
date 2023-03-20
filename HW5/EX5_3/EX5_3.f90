program EX5_3
implicit none
integer, parameter :: n = 5
real(4) :: x(n), y(n)
real(4) :: avgx, avgy, Exi, Eyi, Exi2, Exiyi
real(4) :: A, B
integer :: i

x = [1.,2.,3.,4.,5.]
y = [0.5, 1.7, 3.4, 5.7, 8.4]

! do i = 1, n
	! x(i) = log(x(i))
	! y(i) = log(y(i))
! end do
x = log10(x)
y = log10(y)

! do i = 1, n
	! Exi = Exi + x(i)
	! Eyi = Eyi + y(i)
	! Exi2 = Exi2 + x(i)**2
	! Exiyi = Exiyi + x(i)*y(i)
! end do
Exi = sum(x)
Eyi = sum(y)
Exi2 = sum(x**2)
Exiyi = sum(x*y)


avgx = Exi/n
avgy = Eyi/n

B = (n*Exiyi - Exi*Eyi)/(n*Exi2 - Exi**2)
A = 10**(avgy - B*avgx)

print*,A, B

end program EX5_3