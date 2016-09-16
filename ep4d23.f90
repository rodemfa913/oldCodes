program ep4d23

!Variaveis:
  implicit none
  real :: l04, l14, eps4, int4
  real*8 :: l08, l18, eps8, int8
  real, external :: b4
  real*8, external :: b8
  integer :: nr

!Lendo o intervalo de integracao e a precisao:
  print*, "Calculo da integral da fcao de C N para T = 2500K entre l0 e l1."
  print*, "l0 (mum), l1 (mum), eps:"
  read*, l08, l18, eps8
  l08 = l08 * 1.0e-6
  l18 = l18 * 1.0e-6
  l04 = l08
  l14 = l18
  eps4 = eps8

!Calculando a integral pelo metodo do trapezio em precisao simples e imprimindo
!o resultado:
  call trap4(b4, l04, l14, eps4, int4, nr)
  print*, "Trapezio, precisao simples:"
  print*, " Valor da integral = ", int4, " W/m^2"
  print*, " No de refinamentos: ", nr
  if (nr <= 5) print*, " (limite minimo)"
  if (nr >= 23) print*, " (limite maximo)"
  print*, " No de intervalos: ", 2 ** nr

!Idem para o metodo do trapezio em precisao dupla:
  call trap8(b8, l08, l18, eps8, int8, nr)
  print*, "Trapezio, precisao dupla:"
  print*, " Valor da integral = ", int8, " W/m^2"
  print*, " No de refinamentos: ", nr
  if (nr <= 5) print*, " (limite minimo)"
  if (nr >= 23) print*, " (limite maximo)"
  print*, " No de intervalos: ", 2 ** nr

!Idem para o metodo de Simpson em precisao dupla:
  call simp8(b8, l08, l18, eps8, int8, nr)
  print*, "Simpson, precisao dupla:"
  print*, " Valor da integral = ", int8, " W/m^2"
  print*, " No de refinamentos: ", nr
  if (nr <= 5) print*, " (limite minimo)"
  if (nr >= 23) print*, " (limite maximo)"
  print*, " No de intervalos: ", 2 ** (nr + 1)

end program ep4d23

subroutine trap4(f, x0, x1, eps, T, n)
!Calcula a integral de f entre x0 e x1 pelo metodo do trapezio em prec simples.
!Retorna o valor da integral (T) e o no de refinamentos (n).

!Variaveis:
  implicit none
  real, external :: f
  real, intent(in) :: x0, x1, eps
  real, intent(out) :: T
  integer, intent(out) :: n
  integer :: i
  real :: h, T0, x, S

!Valores iniciais:
  h = x1 - x0
  T0 = h / 2 * (f(x0) + f(x1))

!1o refinamento:
  n = 1
  x = (x0 + x1) / 2
  T = T0 / 2 + h / 2 * f(x)

!Demais refinamentos; minimo de 5, maximo de 23 (acima disso, o loop do i abaixo
!demoraria varios minutos...):
  do while (n < 23 .and. (abs((T - T0) / T) >= eps .or. n < 5))
    T0 = T
    n = n + 1
    x = x0 + h / 2 ** n
    S = f(x)
    do i = 1, 2 ** (n - 1) - 1  !2 ** 22 - 1 = 4194303 !!!!!!!!!!!
      x = x + h / 2 ** (n - 1)
      S = S + f(x)
    end do
    T = T0 / 2 + h / 2 ** n * S
  end do

end subroutine trap4

subroutine trap8(f, x0, x1, eps, T, n)

  implicit none
  real*8, external :: f
  real*8, intent(in) :: x0, x1, eps
  real*8, intent(out) :: T
  integer, intent(out) :: n
  integer*8 :: i
  real*8 :: h, T0, x, S

  h = x1 - x0
  T0 = h / 2 * (f(x0) + f(x1))

  n = 1
  x = (x0 + x1) / 2
  T = T0 / 2 + h / 2 * f(x)

  do while (n < 23 .and. (abs((T - T0) / T) >= eps .or. n < 5))
    T0 = T
    n = n + 1
    x = x0 + h / 2 ** n
    S = f(x)
    do i = 1, 2 ** (n - 1) - 1
      x = x + h / 2 ** (n - 1)
      S = S + f(x)
    end do
    T = T0 / 2 + h / 2 ** n * S
  end do

end subroutine trap8

subroutine simp8(f, x0, x1, eps, Sim, n)

  implicit none
  real*8, external :: f
  real*8, intent(in) :: x0, x1, eps
  real*8, intent(out) :: Sim
  integer, intent(out) :: n
  integer*8 :: i
  real*8 :: h, T0, T1, Sim0, x, S

  h = (x1 - x0) / 2
  T0 = h * (f(x0) + f(x1))
  x = (x0 + x1) / 2
  T1 = T0 / 2 + h * f(x)
  Sim0 = T1 + (T1 - T0) / 3

  T0 = T1
  n = 1
  x = x0 + h / 2
  T1 = T0 / 2 + h / 2 * (f(x) + f(x + h))
  Sim = T1 + (T1 - T0) / 3

  do while (n < 23 .and. (abs((Sim - Sim0) / Sim) >= eps .or. n < 5))
    Sim0 = Sim
    T0 = T1
    n = n + 1
    x = x0 + h / 2 ** n
    S = f(x)
    do i = 1, 2 ** n - 1
      x = x + h / 2 ** (n - 1)
      S = S + f(x)
    end do
    T1 = T0 / 2 + h / 2 ** n * S
    Sim = T1 + (T1 - T0) / 3
  end do

end subroutine simp8

real function b4(lambda)
!Expressao para o espectro de um corpo negro para T = 2500K (prec simples)

  implicit none
  real, intent(in) :: lambda
  real, parameter :: h = 6.626069e-34, c = 2.997925e+8, k = 1.38065e-23, &
                     T = 2500.
  real :: a1, a2

  a1 = 2. * h * c ** 2.
  a2 = h * c / k
  b4 = a1 / (lambda ** 5. * (exp(a2 / (lambda * T)) - 1.))

end function b4

real function b8(lambda)
!Expressao para o espectro de um corpo negro para T = 2500K (prec dupla)

  implicit none
  real*8, intent(in) :: lambda
  real*8, parameter :: h = 6.626069e-34, c = 2.997925e+8, k = 1.38065e-23, &
                       T = 2500.
  real*8 :: a1, a2

  a1 = 2. * h * c ** 2.
  a2 = h * c / k
  b8 = a1 / (lambda ** 5. * (exp(a2 / (lambda * T)) - 1.))

end function b8
