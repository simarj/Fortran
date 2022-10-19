program Max_Min_Sort 
    implicit none
    real::a,b,c,t
    real,dimension(:),allocatable::z
    integer::i,j,n
    print*,"How many numbers"
    read*,n			
    allocate(z(n))
    print*,size(Z)
    read(*,*)(Z(i),i=1,n)
    print*,'Original array:',Z
    do i=1,size(z)
    do j=i+1,size(z)  
    if (z(i)>z(j)) then
    t=z(i)
    z(i)=z(j)
    z(j)=t
    end if
    end do 
    end do
    print*,'Sorted array:',Z  
    print*,'Max:',z(1)
    print*,Z(n)
end program
