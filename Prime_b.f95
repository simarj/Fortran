program prime_b
implicit none
integer::i,h,n,c=3
real::j=4
real,dimension(:),allocatable::L
print*,'Enter N'
read*,n
allocate(L(n))
L(1)=2
L(2)=3
do while (j<=n)
call primecheck(h,j)
if (h==1)then 
L(c)=j
c=c+1
end if 
j=j+1
end do
print*,'No of Primes less than equal to ',n,'are :'
print*,'         Sno.  ',' Prime '
do i=1,c-1
print*,i,L(i)
end do 

end program

subroutine primecheck(h,c)
    integer, intent (out)  :: h ! input
    real, intent (in) :: c ! output
    do i=2,floor(sqrt(c))
    if (mod(floor(c),i)==0) then 
    h=0
    exit
    else 
    h=1
    end if
    end do
end subroutine
