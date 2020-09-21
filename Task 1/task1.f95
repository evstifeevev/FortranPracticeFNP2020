Program task1

implicit none

! constants
integer, parameter :: n = 2
real(8), parameter :: PI = 3.1415926535897932
complex, parameter :: i_c = (0.0,1.0)
! variables
real(8), dimension(n) :: arr = (/0.0,0.0 /)
complex :: c = (0.0, 0.0)
arr = (/1.0, -1.0 /)
! transform to
c = arr(1) + i_c * arr(2)

! main
print *, 'This program is a demo of fortran trigonometry and algebra functions &
providing calculations for complex and real arguments' , new_line(' '),&
'max(1,2,3) = ', max(1,2,3), ', mod(11.01,0.3) = ', mod(11.01,0.3) , new_line(' '),&
'c = ', c, ', conjg(c) = ', conjg(c) , new_line(' '),&
'log(c) = ', log(c), ', sin(c) = ', Sin(c), new_line(' '),&
'abs(c) = ', abs(c), ', exp(c) = ', exp(c), new_line(' '),&
'cosh(real(c)) = ', cosh(real(c)), ', sqrt(c) = ', sqrt(c)

pause
end program task1
