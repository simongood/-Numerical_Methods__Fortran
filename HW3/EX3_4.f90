program bisection 
implicit none 
real :: xi , x, temp 
real :: fi ,fx
real :: limit, func 
integer :: i
! print *, 'input lower boundary' 
! read (*,*) xl 
! print *, 'input upper boundary' 
! read (*,*) xu 
        x = 0
        xi = 1
        
        limit = 1.e-6
        fx = func(x)
        fi = func(xi)

        Do while ( ABS(fi) > limit ) 
                temp = xi 
                xi = xi - ( fi*(x-xi) )/( fx-fi )
                x = temp
				fi = func(xi)
                fx = func(x)
                
                print *, i, 'xi', xi, 'f(xi)', fi
                i = i + 1
        enddo 
        print *, 'x =', xi, 'f(xi)', fi
end program bisection

function func(x) 
real :: x, func 
func = exp(-x)-x 
end function func
