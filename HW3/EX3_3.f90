program bisection 
implicit none 
real :: xi, xii 
real :: fi, fii 
real :: limit, func 
integer :: i
! print *, 'input lower boundary' 
! read (*,*) xl 
! print *, 'input upper boundary' 
! read (*,*) xu 

        xi = 0
        
        limit = 1.e-6
        fi = func(xi)
        fii = 1

        Do while ( ABS(fii) > limit ) 
                xii = xi - ( exp(-xi)-xi )/( -exp(-xi)-1 )
                fii = func(xii)
                xi = xii
                print *, i, 'xii', xii, 'f(xii)', fii
                i = i + 1
        enddo 
        print *, 'x =', xii, 'f(xii)', fii
end program bisection

function func(x) 
real :: x, func 
func = exp(-x)-x 
end function func
