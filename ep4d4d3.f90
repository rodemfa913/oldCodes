program ep4d4d3
!Variaveis:
  implicit none
  real*8 :: a, b, eps, intg
  real*8, external :: efede
  integer :: nr

!Lendo o intervalo de integracao e a precisao:
  print*, "Calculo da integral da fcao sqrt(x)*exp(-x**2) entre a e b."
  print*, "a, b, eps:"
  read*, a, b, eps

!Calculando a integral pelo metodo do trapezio em precisao dupla e imprimindo
!o resultado:
  call trap8(efede, a, b, eps, intg, nr)
  print*, " Valor da integral = ", intg
  print*, " No de refinamentos: ", nr
  if (nr <= 5) print*, " (limite minimo)"
  if (nr >= 23) print*, " (limite maximo)"
  print*, " No de intervalos: ", 2 ** nr
end program ep4d4d3

subroutine trap8(f, x0, x1, eps, T, n)
!Calcula a integral de f entre x0 e x1 pelo metodo do trapezio em prec dupla.
!Retorna o valor da integral (T) e o no de refinamentos (n).

!Variaveis:
  implicit none
  real*8, external :: f
  real*8, intent(in) :: x0, x1, eps
  real*8, intent(out) :: T
  integer, intent(out) :: n
  integer*8 :: i
  real*8 :: h, T0, x, S

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
end subroutine trap8

real function efede(x)
  implicit none
  real*8, intent(in) :: x
  efede = sqrt(x) / exp(x ** 2)
end function efede
