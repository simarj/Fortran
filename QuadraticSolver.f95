program Quad
implicit none
real(8)::a,b,c
COMPLEX::x1,x2,z
print*,'Enter Coefficents (a,b,c)'
read*,a,b,c
call roots(a,b,c,x1,x2)
if (x1==x2) then 
print*,'Roots of the given quadratic are Real and Equal=',x1
else if  (x2==conjg(x1)) then 
print*,'Roots of the given quadratic are Complex and are :',x1,x2
else 
print*,'Roots of the given quadratic are Real and are :',x1,x2
end if
print*,b**2-(4*a*c)
end program
subroutine Roots(a,b,c,x1,x2)
real(8),intent(in)::a,b,c 
complex,intent(out)::x1,x2
if (b**2-4*a*c>=0) then 
x1=(-b+sqrt(b**2-4*a*c))/(2*a)
x2=(-b-sqrt(b**2-(4*a*c)))/(2*a)
else  
x1=cmplx(-b/(2*a),sqrt(abs(b**2-4*a*c))/(2*a))
x2=conjg(x1)
end if
end SUBROUTINE


