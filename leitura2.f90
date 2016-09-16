program leitura2
    implicit none
    integer :: n,i
    real :: ent
    character*16 :: arq
    print"(a$)"," Nome do arquivo: "
    read*,arq
    n = 0
    open(file = arq,unit = 7,status = "old",action = "read")
    do while (n < 1000)
      read(7,*,end = 100,err = 200) ent
      n = n + 1
    end do
100 rewind(7,err = 200)
    print"(a,i4)"," Numero de dados: ",n
    print"(a)"," Dados lidos:"
    do i = 1,n
      read(7,*,err = 200) ent
      print*,ent
    end do
200 close(unit = 7)
end program leitura2
