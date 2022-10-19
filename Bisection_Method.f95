program bisection
implicit none 
real::a,b,c,f,err,e
integer::i
print*,'Enter values of x1,x2 so that f(x1)*f(x2)<0'
read*,a,b
if (f(a)*f(b)>=0) then 
print*,'Cannot Apply Bisection on given starting values'
stop
end if 
print*,'Enter tolerance value'
read*,err
do while (b-a>=err)
c=(a+b)/2
if (f(c)*f(a)<0) then 
    b=c
else if (f(c)*f(b)<0) then 
    a=c
else 
    print*,'Root of f(x) is x= ',c
    stop
end if 
end do
print*,'Root of f(x) is x=',a
end program
function f(x)
real::f,x
f=sin(x)
return
end function
