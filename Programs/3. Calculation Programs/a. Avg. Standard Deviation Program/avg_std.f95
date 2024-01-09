program avg_std
	implicit none
	double precision :: a, b
	integer :: n, nmc, i, j
	real :: start, finish
	double precision, allocatable :: std(:,:)
	
	write(*,*)"Enter No. of Repititions:"
	read(*,*)n
	write(*,*)"Enter No. of MC Cycles:"
	read(*,*)nmc
	
	!n = 100;	nmc = 10000
	
	open(1,file = "std.dat", status = "old")
	open(2,file = "avgstd.dat")
	open(3,file = "avg_log.dat")

	allocate(std(2,nmc)); std = 0.0
	
	call CPU_time(start)
	do i = 1,n
		do j = 1, nmc
			read(1,*)a, b
			std(1,j) = std(1,j) + a
			std(2,j) = std(2,j) + b
		enddo
	enddo
	std = std/n
	
	do i = 1,nmc
		write(2,*)std(1,i),std(2,i)
	enddo
	call CPU_time(finish)

	write(3,*)"Enter No. of Repititions:", n
	write(3,*)"Enter No. of MC Cycles:", nmc
	write(3,*)"Total Elapsed Time: ", finish - start
	
end program