Program task6

implicit none

! constants
integer, parameter :: n = 8

! variables
integer :: k = 0, il = 0
logical :: l = .false.
character (100) :: string
! main

1001 format(6X,A30,f10.3,i5.2)

open(20, file = 'formatDemo.txt')

write(20,*) 'Original input', 0.1, 1
write(20,1001) "Formatted input", 0.1, 1

! example of variable format expression (Not supported by gfortran)
! write(20, 1002)
! 1001 format(<I>x,<I>F10.<MIN(I-1,5)>)
! do k=1,5
! write (*,1002) k
! end do

write(20,*) ('CycleInput',k=1,80)

do k=1,100
   write(20,'(2X,2(i3,e12.4e3,f8.3),5X,5i10)') k,k*1.0,k*1.0,k,k*1.0,k*1.0,k,k,k,k,k
end do

close (20, status = 'KEEP')

! (i/b/o)w[.m] - integral format descriptor
! (f/e/es)w.d - floating format descriptor
! e12.8e4 -> ...E+0000
! lw - logical format descriptor
! a[w] - character format descriptor

! part 2
open(1, file = 'demo.txt')
write (1,*) ('$',k=1,10)
write (1,*) ('1',k=1,10)
write (1,*) ('N',k=1,10)
rewind 1
read(1,*) string, string
print *, string
backspace 1;backspace 1;
read(1,*) string
print *, string
rewind 1
inquire(unit = 1, name = string)
print *, string
inquire(iolength=il) string
print *, il
endfile 1
!print *, eof(1) ! eof is unknown
!https://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vna3/index.html

close(1, status = 'delete')
pause
call exit
end program task6
