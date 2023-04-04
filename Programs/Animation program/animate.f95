program BD_Animation
	implicit none
	real, allocatable :: A(:,:)
	integer :: nmc, length, i, j
	
	open(1,file="final.dat",status = "old")
	open(2,file="animate.dat")

	write(*,*)"Enter No. of MC Cycles: "
	read(*,*)nmc
	write(*,*)"Enter the Length of the Box:"
	read(*,*)Length
	
	allocate( A(nmc*length, 2) ); A = 0.0
	
	do i = 1,nmc*length
		read(1,*) A(i,1), A(i,2)
	enddo
	
	do i = 1,nmc
		do j = 1,length*i
			write(2,*) A(j,1), A(j,2)
		enddo	
		write(2,*)" "
		write(2,*)" "
	enddo

end program