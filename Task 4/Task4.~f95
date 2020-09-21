Program task4

implicit none

! constants

! variables
integer :: a(100) = 0
integer :: i = 0

!main
a=0

do i=1,100
   if ((mod(i,2)==0) .and. (i>50)) then
   a(i) = 1
   else if((mod(i,2)==1) .and. (i<50)) then
   a(i) = -1
   else
   cycle
   end if
end do
print *, a
print *,''

do i=1,1000
if(i>100) exit
   select case(a(i))
   case(1:998)
   a(i)=5
   case(:-1)
   a(i)=-5
   case default
   a(i)=0
   end select
end do
print *, a
print *,''

!part 3
i=1
10 continue
a(i)=i**2
i=i+1
if(i>100) goto 11
goto 10 ! no goto to do, if, select case, where
11 continue

pause
call exit
end program task4
