Program task3

implicit none

! constants

! variables
real :: a(5) = (/ -5.0, 5.0, -2.0, 0.0, -0.0/)
integer :: i = 0

interface

subroutine inverse(a)

real a(:)

end subroutine inverse

end interface

! main

print *, 'initial array', a

where(a>0)
a=a*2
elsewhere(a<0)
a=a+1
elsewhere
a=-999.999
end where

print *, 'modified array', a

! part 2
print *, ''
forall(i=2:5:1) a(i)=a(i-1)+a(i)**i
print *, 'array modified using forall', a

!forall(i=1:100) a(i,i)=0
!forall(i=1:n) a(i)=sum(x(1:i))
!forall(i=1:n) b(i)=product(x(1:i))

! part 3
print *, ''
print *, 'initial array:',a
call inverse(a)
print *, 'inversed array:',a

call adopt(a, size(a))
print *, 'array of i**i:',a
 
pause
end program task3

! part 3
subroutine inverse(a)

real a(:) ! adopting form array
real c(size(a))  ! auto array

forall(i=1:size(a)) c(i) = a(size(a)-i+1)
a = c

end subroutine inverse


subroutine adopt(x, n)

integer :: i = 0
integer n
real x(1,1,*)  ! adopting size array

forall(i=1:n) x(1,1,i) = i**i

end subroutine
