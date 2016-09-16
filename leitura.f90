program leitura
  implicit none
  integer :: n, ast, i
  real, allocatable :: xy(:,:)
  open(file = "impute.dat", unit = 7, status = "old", action = "read")
  read(7,*) n
  allocate(xy(2,n), stat = ast)
  if(ast /= 0) stop
  read(7,*) xy
  close(unit = 7)
  print"(i4)", n
  do i = 1, n
    print"(2es16.8)", xy(1,i), xy(2,i)
  end do
end program leitura
