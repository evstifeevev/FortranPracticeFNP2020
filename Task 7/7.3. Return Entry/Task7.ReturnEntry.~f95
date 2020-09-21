Program task7ReturnEntry

! include 'fileName' - deprecated. Up to 10 times can be used.

implicit none

! constants
  real, parameter :: PI = 3.141592653589793
! variables
integer :: a = 1, b = 1, temp
character (1) :: c = '+'
! main


do a=-1,1

call SqrRoot(a, *10, *11)

print *, 'Sqrt of', a, 'is ', 0

go to 20

10 print *, 'Sqrt of', a, 'is ', Sqrt(real(a))

go to 20

11 print *, 'Sqrt of', a, 'is ', 'NaN'

20 continue

print *, ''

temp = b
call  Increase(b, 7)
print *, temp, '+ 7 =', b

temp = b
call  Decrease(b, 7)
print *, temp, '- 7 =', b

temp = b
call  Double(b)
print *, temp, '* 2 =', b

end do


pause
call exit
end program task7ReturnEntry

subroutine SqrRoot(a, *, *)

integer :: a

if (a == 0) return ! exit the procedure

if (a > 0) return 1 ! goto 10

return 2 ! goto 11

end subroutine

recursive subroutine Increase(a,b)
implicit none
real, intent(out) :: a
real, intent(in) :: b
a = a + b
return

entry Decrease(a, b)
call Increase(a, -b)
return

entry Double(a)
a = a * 2
return


end subroutine
