program countt
implicit none
integer, parameter :: n = 50
real(4) :: y(n), b
integer :: i

	do i = 1, n
		call ycount(i, b)
		if ( b > 0 ) then
			y(i) = b
		else
			y(i) = 0
		end if
	end do
	open (1, file='countt.txt')
	do i = 1, n
		write (1,*)i, y(i)
	end do
	close(1)

end program countt



subroutine ycount(t, yc)
implicit none
integer :: t
real(4), intent(inout) :: yc

	if ( 0<=t .and. t<15) then
		yc = 38.145*t + 0.13743*t**3
	else if ( 15<=t .and. t<33) then
		yc = 1036 + 130.909*(t - 15) + 6.18425*(t - 15)**2 - 0.428*(t - 15)**3
	else
		yc = 2900 - 62.468*(t - 33) - 16.9274*(t - 33)**2 + 0.41796*(t - 33)**3
	end if

end subroutine ycount
