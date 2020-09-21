Program task2

implicit none

! constants

! variables
real :: a(1:10)
integer :: v(3) = (/1, 10, 3/)
logical :: mask(3) = (/ .true.,.false.,.true./)

! part 2
real :: m1(-5:-3,0:3), m2(4,2)
integer :: i,j

! part 3
real, allocatable :: arrAlloc(:,:)

! main
open(1, file='arr.txt')
a(:) = 2
print *, 'initial array:', a
print '(a, 3ES10.3)', 'formatted array:', a

a(6:10:1) = a(5:9)-a(1:10:2)+10
print *, 'modified array: ', a

a(v) = 4
print *, 'array modified by vector:', a

where(a>5)
a = 5
elsewhere(a<3)
a = -5
elsewhere
a = 0
end where
print * , 'array modified using where:', a

write (1,*), a(1:10:2), new_line(' '), a(v)
close(1,status='keep')

! part 2
print *,'' ;

print *, 'all((/.true.,.true./)):' , all((/.true.,.true./))
print *, 'any((/.true.,.false./)):' , any((/.true.,.false./))
print *, 'count((/.false.,.true./)):' , count((/.false.,.true./))
print *, 'count((/.false.,.false./)):' , count((/.false.,.false./))

m1=0;m2=0;
do i=lbound(m1,1),ubound(m1,1),1
   m1(i,1) = i
   m1(i,:) = m1(i,:) + 4
end do
print *, 'First matrix:'
write (*, '(3F4.0)' ) m1

do i=lbound(m2,1),ubound(m2,1),1
   do j=lbound(m2,2),ubound(m2,2),1
      m2(i,j) = i*j
   end do
end do
print *, 'Second matrix:'
write (*, '(4F4.0)' ) m2

print *, 'shape(m2) : ' , shape(m2)
print *, 'maxloc(m2) : ' , maxloc(m2)
print *, 'maxloc(m2,m2>=5 .and.m2<9) : ' , maxloc(m2,m2>=5 .and.m2<9)
print *, 'maxval(m2,m2>=5 .and.m2<9) : ' , maxval(m2,m2>=5 .and.m2<9)
print *, 'product(m2,1,m2>=2 .and.m2<4) : ' , product(m2,1,m2>=2 .and.m2<4)
print *, 'sum(m2,1,m2>=2 .and.m2<4) : ' , sum(m2,1,m2>=2 .and.m2<4)

print *, 'dot_product((/ 1, 2, 3/),(/ 10, 100, 1000/)) : ' ,&
dot_product((/ 1, 2, 3/),(/ 10, 100, 1000/))

print *, 'matmul(m1,m2) : '
write (*, '(3F5.0)' ) matmul(m1,m2)     !nm, mk - nk or m, mk - k or nm m - m

print *, 'cshift(m1, 1, 1) : '
write (*, '(4F4.0)' ) cshift(m2, 1, 1)
print *, 'cshift(m1, -1, 1) : '
write (*, '(4F4.0)' ) cshift(m2, -1, 1)
print *, 'cshift(m1, 1, 2) : '
write (*, '(4F4.0)' ) cshift(m2, 1, 2)
print *, 'cshift(m1, -1, 2) : '
write (*, '(4F4.0)' ) cshift(m2, -1, 2)
print *, 'cshift(m2,(/ -1, 1, 0, 1/),2) : '
write (*, '(4F4.0)' ) cshift(m2,(/ -1, 1, 0, 1/),2)
print *, 'cshift(m2,(/ 4, -4, 0, 4/),2) : '
write (*, '(4F4.0)' ) cshift(m2,(/ 4, -4, 0, 4/),2)

print *,''
print *, 'eoshift(m2, 2) : '
write (*, '(4F4.0)' ) eoshift(m2, 2)
print *, 'eoshift(m2, -2) : '
write (*, '(4F4.0)' ) eoshift(m2, -2)

print *, 'transpose(m2) : '
write (*, '(2F4.0)' ) transpose(m2)

! part 3
print *,''
allocate(arrAlloc(3,7))

do i=lbound(arrAlloc,1),ubound(arrAlloc,1),1
   do j=lbound(arrAlloc,2),ubound(arrAlloc,2),1
      arrAlloc(i,j) = j**i
   end do
end do

write (*, '(a, 21(3F6.1))') 'initial allocated array:',arrAlloc
deallocate(arrAlloc)

allocate(arrAlloc(5,1))

arrAlloc(:,1) = 12

print *, 'reallocated array:',arrAlloc

pause
end program task2
