program HW2
implicit none
	real :: h, x, y, base
	real :: fplus, fminus, firstdir, realvalue, rounderror
	integer :: i, j
	
	! 初始數值設置
	x = 0.5
	call f(x, y, 1)
	realvalue = y
	
	
	! 計算 round off error
	do j = 0, 9
		h = 1.0/(10**j)	! 設定初始參數 
		fplus = realvalue
		fminus = realvalue
		base = 1
		
		! 計算 fplus, fminus
		do i = 1, 4	
			base = base*i			! 分母計算	
			call f(x, y, i)
			fplus = fplus + (y*h**i)/base
			fminus = fminus + ((y*h**i)/base)*(-1)**i

		end do
		
		firstdir = (fplus - fminus)/(2*h)
		rounderror = ABS(realvalue - firstdir)
		
		print*,'h', h, 'rounderror', rounderror
		
		! 寫入檔案
		open (1, file='RoundOffErro.txt')
		write (1,*) h, rounderror

	
	end do
	close(1)

end program HW2



subroutine f(x, y, i)
implicit none 
integer :: i
real :: x, y

	if ( i == 0 ) then
		y = -0.1*x**4 - 0.15*x**3 - 0.5*x**2 - 0.25*x + 1.2
	else if ( i == 1 ) then
		y = -0.4*x**3 - 0.45*x**2 - x - 0.25
	else if ( i == 2 ) then
		y = -1.2*x**2 - 0.9*x - 1
	else if ( i == 3 ) then
		y = -2.4*x - 0.9
	else
		y = -2.4
	end if

end subroutine f