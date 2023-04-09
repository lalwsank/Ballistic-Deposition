program BD_Animation
	implicit none
	real, allocatable :: A(:,:)
	integer :: nmc, length, i, j, layer
	
	open(1,file="final.dat",status = "old")
	open(2,file="final_layer.dat")

	write(*,*)"Enter No. of MC Cycles: "
	read(*,*)nmc
	write(*,*)"Enter the Length of the Box:"
	read(*,*)Length
	write(*,*)"Enter the numbers of cycles in 1 Layer:"
	read(*,*)layer
	
	allocate( A(nmc*length, 2) ); A = 0.0
	layer = layer*length
	
	do i = 1,nmc*length
		read(1,*) A(i,1), A(i,2)
	enddo
	
	do i = 1, (nmc*length/layer)
		do j = 1, layer
			write(2,*)A((layer*(i-1)) + j, 1), A((layer*(i-1)) + j, 2)
		enddo
		write(2,*)""
		write(2,*)""	
	enddo
		

end program