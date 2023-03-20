program EX4_1
implicit none
integer, parameter :: n = 2
real(4) :: A(3,2), B(2,2), C(3,2)
integer :: i, j, k

A = reshape([1., 3., 7., 6., 10., 4.], [3,2])
B = reshape([1., 0.5, 3., 2.], [2,2])


do i = 1, 3
        do j = 1, 2
                do k = 1, n
                        C(i,j) = C(i,j) + A(i,k)*B(k,j)
                enddo
        enddo
enddo

print*, C(1,:)
print*, C(2,:)
print*, C(3,:)



end program EX4_1