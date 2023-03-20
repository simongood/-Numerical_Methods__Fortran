program bisection 
implicit none 
real :: xl, xu, xr 
real :: fl, fu, fr 
real :: limit, func 
integer :: i
! print *, 'input lower boundary' 
! read (*,*) xl 
! print *, 'input upper boundary' 
! read (*,*) xu 

        xl = 0
        xu = 1.0
        
        limit = 1.e-6
        fl = func(xl) 
        fu = func(xu)
        fr = 1
        if (fl*fu > 0.) then 
            print *, 'boundary choice error' 
            print *, 'STOP program' 
            stop 
        endif

        Do while ( ABS(fr) > limit ) 
                xr = xu - (fu*(xl-xu))/(fl-fu)
                fr = func(xr)
                if (fl*fr < 0.) then 
                    xu = xr
                    fu = func(xu)
                else if (fl*fr > 0.) then 
                    xl = xr
                    fl = func(xl) 
                endif
                print *, i, 'xr', xr, 'f(xr)', fr
                i = i + 1
        enddo 
        print *, 'x =', xr, 'f(xr)', fr
end program bisection

function func(x) 
real :: x, func 
func = exp(-x)-x 
end function func
