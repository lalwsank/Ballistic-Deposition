Program Least_sqr_fit
	IMPLICIT NONE
	double precision, allocatable :: x(:), y(:)
	double precision :: sumx, sumy, sumxx, sumxy, a, b
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
	open(2, file = "LSF.dat")
	write(*,*)"Enter starting point:"
	read(*,*)st
	write(*,*)"Enter Ending point:"
	read(*,*)ed
		if(st == 0) then
			st = 1
		elseif(ed > n) then
			ed = n	
		endif
	
	sumx = 0.0 ;	sumy = 0.0 ; sumxx = 0.0 ; sumxy = 0.0	
	do i = st,ed
		sumx = sumx + x(i);			sumy = sumy + y(i)
		sumxx = sumxx + x(i)**2; 	sumxy = sumxy + x(i)*y(i)
	enddo
	n = ed - st + 1
	a = (sumy*sumxx - sumx*sumxy) / (n*sumxx - sumx**2)
	b = (sumx*sumy - n*sumxy) / (sumx**2 - n*sumxx)
	
	write(2,*)"For File name:", nam
	write(2,*)"With no. of Observations:",n
	write(2,*)"LSF between Observation numbers:", st, " to ", ed	
	write(2,*)"in log scale: log(y) =",real(b),"log(x) +",real(a)
	write(2,*)"Therefore, LSF: y = x^",real(b),"x",10**real(a)

End Program Least_sqr_fit