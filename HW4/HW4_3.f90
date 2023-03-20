program HW4_3
implicit none
integer, parameter :: n = 3
real(4) :: a(n,n), x(n), b(n)
integer :: i, j
real :: er(n), temp(n), temp1

a = reshape([-8., 2., -3., 1., -6., -1., -2., -1., 7.], [n,n])
b = [-20., -38., -34.]
er = [1.,1.,1.]


do while (maxval(er) > 0.0001)
	do i = 1, n
		temp(i) = x(i)
		temp1 = 0.
		do j = 1, n
			if (j/=i) then
				temp1 = temp1 - a(i,j)*x(j)
			endif
		enddo
		x(i) = (temp1+b(i))/a(i,i)
		er(i) = ABS((x(i) - temp(i))/x(i))
		print*, temp(i), x(i)
	enddo
enddo

print*,x


end program HW4_3