Program Least_sqr_fit
	IMPLICIT NONE
	double precision, allocatable :: x(:), y(:)
	double precision :: maximum, minimum, sat
	integer :: n, i, st, ed
	character(len=20) :: nam
	
	write(*,*)"Enter file name: "
	read(*,*) nam
	write(*,*)"Enter Number of Observations: "
	read(*,*) n	
		allocate(x(n)); allocate(y(n))
		open(1, file = nam)
		do i=1,n
			read(1,*)x(i),y(i)
		enddo
	x = log10(x);	y = log10(y)
	open(2, file = "Saturation.dat")
	write(*,*)"Enter Saturation starting point:"
	read(*,*)st
	write(*,*)"Enter Saturation Ending point:"
	read(*,*)ed
		if(st == 0) then
			st = 1
		elseif(ed > n) then
			ed = n	
		endif
	
	maximum = maxval(y(st:ed))
	minimum = minval(y(st:ed))
	sat = (maximum + minimum)/2
	
	write(2,*)"For File name:", nam
	write(2,*)"Between Observation numbers:", st, " to ", ed	
	write(2,*)"Saturation Point: log(y) =:", sat
	write(2,*)"or, Saturation Point: y =:", 10**sat
	
End Program Least_sqr_fit