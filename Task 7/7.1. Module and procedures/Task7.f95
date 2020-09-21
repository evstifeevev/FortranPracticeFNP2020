Program main

use recursion

implicit none

! constants

! variables

real, dimension(5) :: a = (/6.0,0.0,23.0,6.0,-1.0/)
real :: mymax
integer :: n,m

print *, mymax(a,size(a))

! part 2
n = 5
call triangle(n)

call ifactIndirect(n,m)
print *, 'factorial ', n , '=', m

include 'filename.txt'

pause
call exit
end program

! function usage
real function mymax(a,n)
real, dimension(1:n), intent(in)  :: a ! input
real :: temp
integer :: i
temp = a(1)
do i=2,size(a)
if(a(i)>temp) temp=a(i)
end do
mymax = temp
return
end function
