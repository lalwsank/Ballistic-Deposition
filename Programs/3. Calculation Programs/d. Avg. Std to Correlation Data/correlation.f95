program correlation
	implicit none
	double precision :: a, b, length, alpha, z
	integer :: nmc, i, j
	real :: start, finish
	double precision, allocatable :: correlation(:,:)
	
	write(*,*)"Enter No. of MC Cycles:"
	read(*,*)nmc
	write(*,*)"Enter Length of Box:"
	read(*,*)length
	write(*,*)"Enter value of alpha:"
	read(*,*)alpha
	write(*,*)"Enter value of z:"
	read(*,*)z
	
	open(1,file = "avgstd.dat", status = "old")
	open(2,file = "correlation.dat")
	open(3,file = "correlation_log.dat")

	allocate(correlation(2,nmc)); correlation = 0.0
	
	call CPU_time(start)
	do i = 1,nmc
		read(1,*)a, b
		correlation(1,i) = a/(length**z)
		correlation(2,i) = b/(length**alpha)
	enddo
	
	do i = 1,nmc
		write(2,*)correlation(1,i),correlation(2,i)
	enddo
	call CPU_time(finish)

	write(3,*)"Enter No. of MC Cycles:", nmc
	write(3,*)"Enter Length of Box:", length
	write(3,*)"Enter value of alpha:", alpha
	write(3,*)"Enter value of z:", z
	write(3,*)"Total Elapsed Time: ", finish - start
	
end program