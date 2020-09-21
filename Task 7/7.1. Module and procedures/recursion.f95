module recursion

implicit none

! variables
integer, private :: n,m

contains

recursive subroutine triangle(n) ! direct recursive  procedure

integer, intent(in) :: n
integer :: i
character (len = n) :: temp
temp = ""
do i=1,n
temp = "*"//temp
end do
print *, temp
if(n>1) call triangle(n-1)
end subroutine triangle

! p.s. There is no point to do it in this way, but i didn't come up with
! any better examples
recursive subroutine ifactIndirect(n, res) ! indirect recursive  procedure
implicit none
integer, intent(out) :: res
integer, intent(in) :: n
integer :: temp
if (n<=1) then
res = 1
else
call AuxiliarySubroutine(n-1, temp)
res = n * temp
end if
end subroutine ifactIndirect

recursive subroutine AuxiliarySubroutine(n, res) ! indirect recursive  procedure
implicit none
integer, intent(out) :: res
integer, intent(in) :: n
integer :: temp
if (n<=1) then
res = 1
else
call ifactIndirect(n-1, temp)
res = n * temp
end if
end subroutine AuxiliarySubroutine

end module recursion

