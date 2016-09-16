program eqwid

!Variaveis:
    implicit none
    real :: lF1(2), lF2(2), dl, f, Wl
    character*16 :: fname
    integer :: n
!Notacoes: lF1 = [l_i, (F_l/F_0)_i], lF2 = [l_i+1, (F_l/F_0)_i+1], dl = hi,
!          f = 1 - overline{(F_l/F_0)}_i, Wl = int f dl

    print "(/2a)", " Este programa calcula a largura equivalente de uma ", &
"linha espectral normalizada."
    print *, "O arquivo de dados do espectro deve conter, em cada linha, o ", &
"valor de"
    print *, "comprimento de onda seguido do respectivo fluxo normalizado."

!Lendo o nome do arquivo:
    print "(/a$)", " Nome do arquivo (maximo de 16 caracteres): "
    read *, fname

!Abrindo o arquivo:
    open(file = fname, unit = 7, status = "old", action = "read")

    Wl = 0
    n = 0

!Lendo os 2 primeiros pontos:
    read(7, *, end = 100, err = 100) lF1, lF2
    n = 2

!Calculando a integral Wl pelo metodo do pto medio:
    do while (n < 1000)
      dl = lF2(1) - lF1(1)
      f = 1 - (lF1(2) + lF2(2)) / 2
      Wl = Wl + f * dl
      lF1 = lF2

      read(7, *, end = 100, err = 100) lF2
    !A condicao de parada do loop eh a marca de fim de arquivo (EOF).

      n = n + 1
    end do

100 close(unit = 7)

!Imprimindo o resultado (e o no de pts lidos):
    print "(/a$)", " Largura equivalente = "
    print *, Wl
    print *, " No de pontos lidos = ", n
    print *, ""

end program eqwid
