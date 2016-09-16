program vetoral
	implicit none
	real,dimension(5) :: u,v
	print*, "vetores aleatorios:"
	call random_number(u)
	call random_number(v)
	call imprima_vetor(u,size(u) - 1,v,size(v) - 2)
end program vetoral

subroutine imprima_vetor(u,m,v,n)
	implicit none
	integer,intent(in) :: m,n
	real,intent(in) :: u(m),v(n)
	integer*1 :: i
	do i = 1,m
		write(*,"(e10.3$)") u(i)
	end do
	print*
	do i = 1,n
		write(*,"(e10.3$)") v(i)
	end do
	print*
end subroutine imprima_vetor
