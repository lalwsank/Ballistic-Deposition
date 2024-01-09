program Porousity
	implicit none
	
	double precision, allocatable :: y(:)
	double precision :: maximum, area, particles, a
	integer :: n, i, Length, nmc
	character(len=20) :: nam
	
	write(*,*)"Enter the Length of the Box:"
	read(*,*)Length
	write(*,*)"Enter No. of MC Cycles: "
	read(*,*)nmc

	
	open(1,file = "final.dat", status = "old")
	open(2,file = "porosity.dat")

	allocate( y(nmc*length) ); y = 0.0; a = 0 
	
	do i = 1,nmc*length
		read(1,*) a, y(i)
	enddo
	
	do i = 1,nmc
		maximum = maxval(y(1:i*Length))
		area = maximum*Length
		particles = real(Length*i)
		write(2,*)i, 1 - particles/area		
	enddo
	
	maximum = maxval(y)
	area = maximum*Length
	particles = real(Length*nmc)
	write(2,*)""
	write(2,*)"Enter the Length of the Box: ", Length
	write(2,*)"Enter No. of MC Cycles: ", nmc
	write(2,*)"Total Area of Deposition: ", int(area)
	write(2,*)"number of Particles Deposited: ", int(particles)
	write(2,*)"Porousity: ",1 - particles/area
	
end program