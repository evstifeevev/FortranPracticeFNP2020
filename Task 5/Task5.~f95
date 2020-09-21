Program task5

implicit none

! constants
integer, parameter :: n = 2, m = 5

! variables
integer :: i = 0, j = 0
integer :: arrI(4) = 0
real(kind = 8) :: r = 0.0 , r2 = 0.0
real(kind = 8), parameter :: pi=3.14159265358979
real :: arrR(n,m) = 0.0
complex :: c = (0.0, 0.0)
complex :: arrC(10) = (0.0, 0.0)

! part 2
! interfaces
interface mymin

function inmin(i1,i2)

integer(4) inmin, i1, i2

end function inmin

function remin(i1,i2)

real(4) remin, i1, i2

end function remin

function rimin(i1,i2)

real(4) rimin, i1
integer i2

end function rimin

end interface

! main
print *, 'int(2e9 + 1e-9, 4) =', int(2e9 + 1e-9, 4)
print *, 'int((12,-1000), 4) =',int((12,-1000), 4)
print *, 'int((/ 0.5, 1.5, -0.5, 0.99/), 4) =',int((/ 0.5, 1.5, -0.5, 0.99/), 4)
i =  2e9 + 1e-9
print *, '2e9 + 1e-9 =', i
i = (12,-1000)
print *, '(12,-1000) =', i
arrI = (/ 0.5, 1.5, -0.5, 0.99/)
print *, '(/ 0.5, 1.5, -0.5, 0.99/) =', arrI
! real()
! dble()  !kind===8
!cmplx(re,im,kind)
!dcmplx() !kind===8

r=-2.300000000000000
i=5
r2=i*r
print *, '5 * -2.300000000000000 =', r2

print *, 'pi =',pi
print *, 'real(pi,4) =', real (pi,4)

r2 = 0
do i=1,1000000
   r2 = r2+r
end do
r2=r2/1000000
print *, 'r =',r, 'r2 =', r2, 'r == r2 ? Answer - ', r == r2

print *, '1e-10 = ', 1e-10

do j=1,n

do i=1,m

!arrR(i,j)=i/(10*j) ! wrong result
arrR(i,j)=i/(10.0*j)

end do

end do
print *, arrR

print *, 'mymin(-10.0,12.0) = ', mymin(-10.0,12.0)
print *, 'mymin(-10,12) = ', mymin(-10,12)
print *, 'mymin(-10.0,12) = ', mymin(-10.0,12)

pause
call exit
end program task5

! part 2
function inmin(i1,i2)

integer(4) inmin, i1, i2

if (i2 >=i1) then

inmin=i1

else

inmin=i2

end if

end function inmin

function remin(a1,a2)

real(4) remin, a1, a2

if (a1 <= a2) then

remin=a1

else

remin=a2

end if

end function remin

function rimin(a1,a2)

real(4) rimin, a1
integer a2

if (a1 <= a2) then

rimin=a1

else

rimin=a2

end if

end function rimin
