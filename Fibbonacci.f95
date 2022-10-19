program Fibbonacci
implicit none
integer::a_0,a_1,a_n,n,i
real::r
real,dimension(:),allocatable::s,rt
a_0=0
a_1=1
write(*,*)'Number of terms for fibbonacci series'
read*,n
allocate(s(n),rt(n-2))
s(1)=a_0
s(2)=a_1 
print*,'          n   ','Fibbonacci    ',' Ratio :a(n)/a(n-1)'
print*,1,0,'       -'
print*,2,1,'       inf'
do i=3,n
s(i)=a_0+a_1
rt(i-2)=s(i)/a_1
if (mod(i,2)==0) then 
print*,i,s(i),rt(i-2)
else 
print*,i,S(i),'  -'
end if
a_0=a_1
a_1=s(i)
end do 

end program