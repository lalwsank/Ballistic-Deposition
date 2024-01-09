program ballistic_deposition
	implicit none
	real, allocatable :: H(:)
	integer :: nmc, n, Length, x, y, i, j, k, l, dis
	double precision :: r, mean, sqmean, std
	real :: start, finish
	write(*,*)"Enter the Length of the Box: "
	read(*,*)Length	
	write(*,*)"Enter No. of MC Cycles: "
	read(*,*)nmc
	write(*,*)"Enter No. of Repititions: "
	read(*,*)n
	write(*,*)"Create Display file for o/p ? (0 = No / 1 = Yes):"
	read(*,*)dis
	
	
	allocate(H(Length))	!Array to store Max Height of "Length" no. of Columns
	y = 0
	if(dis == 1) then
		open(1, file = "final.dat")		!Stores BD data after n no. of repetitions
	endif
	open(2, file = "std.dat")		!Stores Data to Calculate Standard Deviation (Roughness/Width)
	open(3, file = "log.dat")		!Stores Log of Elapsed time along with input data

	call cpu_time(start)		!assighning cpu time to 'start" before commencing the loop

	do k = 1, n
        H = 0
		do i = 1,nmc
			do j = 1, Length
			
				!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
				call random_number(r)		!!
				x = int(Length*r) + 1		!!	 choosing a random integer
				if(x > Length) then			!!-- point "x" ranging (0,Length)
					x = Length				!!
				endif						!!
				!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

				!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
				if(x == 1) then						!!
					y = max(H(Length),H(x),H(x+1))	!!
				elseif(x == Length) then			!!	 Comparing max heights of (x)th, (x+1)th and (x-1)th
					y = max(H(x-1),H(x),H(1))		!!-- and applying boundary consitions at x = 1 & x = Length
				else								!!	 and later storing that max height in "y"
					y = max(H(x-1),H(x),H(x+1))		!!
				endif								!!
				!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

				!!!!!!!!!!!!!!!!!!!!!!!!!!
				if (y == H(x)) then	    !!
					H(x) = y + 1		!!	 If the value of y is the same as max height at (x)th column
				else					!!-- then the new max height of (x)th column is y + 1
					H(x) = y			!!	 else the new max height of (x)th column is y
				endif					!!
				!!!!!!!!!!!!!!!!!!!!!!!!!!

				if(dis == 1) then
					!!!!!!!!!!!!!!!!!!!!!!!!!!
					if(k == 1) then			!!
						write(1,*)x, H(x)	!!-- Only taking 1 repitition for the display o/p of BD
					endif					!!	 (Can be ommited if o/p is already registered)
					!!!!!!!!!!!!!!!!!!!!!!!!!!
				endif
			enddo
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			mean = 0.0; 	sqmean = 0.0; 	std = 0.0		!!	
			mean = sum(H(1:Length))/Length					!!-- Standard Deviation / Width Calculation
			std = sqrt(sum((H(1:Length)-mean)**2)/Length)	!!
			write(2,*)i, std								!!		
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		enddo
		write(2,*)" " !-- Creating Blocks to differentiate indices in Gnuplot 
		write(2,*)" " !   
	enddo
	
	call cpu_time(finish)
	write(3,*)"Enter the Length of the Box: ", Length
	write(3,*)"Enter No. of MC Cycles: ", nmc
	write(3,*)"Enter No. of Repititions: ",	n
	write(3,*)"Create Display file for o/p ? (0 = No / 1 = Yes):", dis
	write(3,*)"Total time Elapsed to run code:", finish - start
	
end program