!Esta eh uma versao caseira da rotina 'gauleg.f' do 'Numerical Recipes'
module gau_leg
  implicit none
  integer :: n
  real*8, allocatable :: c(:), x(:), w(:)
end module gau_leg

program gauss_legendre
  use gau_leg

  implicit none
  integer :: i

  read*, n !n = ordem do metodo de Gauss-Legendre
  n = n + 1 !n agora eh o grau do polinomio de Legendre
  if(n < 0) stop !n deve ser >= 0

  allocate(c(n + 1)) !n + 1 coeficientes do polinomio
  allocate(x(n)) !n raizes
  allocate(w(n)) !n pesos

  call polleg() !calcula os n + 1 coeficientes do polinomio
  do i = 1, n + 1
    print"(f16.8$)", c(i)
  end do
  print"(a)", ""

  if(n == 0) stop !polinomio de grau 0 nao tem raiz...
  call raizes_pl() !calcula as raizes do polinomio
  do i = 1, n
    print"(f16.8$)", x(i)
  end do
  print"(a)"

  call pesos_pl(n, x, w) !calcula os pesos a partir das raizes
  do i = 1, n
    print"(f16.8$)", w(i)
  end do
  print"(a)", ""
end program gauss_legendre

subroutine polleg() !calcula os coeficientes a partir dos polinomios de
                    !grau inferior a n
  use gau_leg, only: n, c

  implicit none
  real*8 :: c0(n + 1), c1(n + 1)
  integer :: i, j
                            !exemplo: n = 4
  c = (/(0, i = 1, n + 1)/) !c = (0, 0, 0, 0, 0)
  c0 = (/(0, i = 1, n + 1)/) !c0 = (0, 0, 0, 0, 0)
  c1 = (/(0, i = 1, n + 1)/) !c1 = (0, 0, 0, 0, 0)

  if(n == 0) then
    c(1) = 1 !P0 = 1
  else if(n == 1) then
    c(2) = 1 !P1 = x
  else
    c0(1) = 1 !c0 = (1, 0, 0, 0, 0)
    c1(2) = 1 !c1 = (0, 1, 0, 0, 0)
    do i = 2, n !i = 2, 3, 4
      c(1) = -1.0 * (i - 1) / i * c0(1)
      do j = 2, i + 1 !j = 2 3, 2 3 4, 2 3 4 5
        c(j) = 1.0 * (2 * i - 1) / i * c1(j - 1) - 1.0 * (i - 1) / i * c0(j)
      !c  = (-1/2, 0, 3/2, 0, 0), (0, -3/2, 0, 5/2, 0), (3/8, 0, -30/8, 0, 35/8)
      end do
      c0 = c1 !c0 = (0, 1, 0, 0, 0), (-1/2, 0, 3/2, 0, 0)
      c1 = c !c1 = (-1/2, 0, 3/2, 0, 0), (0, -3/2, 0, 5/2, 0)
    end do
  end if
end subroutine polleg

subroutine raizes_pl()
  use gau_leg, only: n, c, x !n = grau do polinomio = no de raizes

  implicit none
  real*8 :: x1, f1, x2, f2
  real*8, external :: poli !funcao que calcula Pn(x)
  real*8, parameter :: eps = 1.0 / 2.0 ** 23
  integer :: i, j

  x1 = eps
  f1 = poli(x1)
  x2 = 2 * eps
  f2 = poli(x2)

  if(mod(n, 2) /= 0) then !n impar --> uma das raizes eh 0
    x((n + 1) / 2) = 0 !n = 5 --> x(2) = 0, i = 4, j = 2
    i = (n + 3) / 2    !n = 1 --> x(1) = 0, i = 2, j = 0
    j = (n - 1) / 2
  else
    i = n / 2 + 1 !n = 6 --> i = 4, j = 3
    j = n / 2     !n = 2 --> i = 2, j = 1
  end if !raizes sao simetricas em relacao a x = 0

  do while(i <= n)
    if(f1 > 0 .eqv. f2 < 0) then
    !buscando as raizes em passos de eps = 2**-23 (demora um pouco mas
    !funciona...) e usando o metodo da falsa posicao para refinar
      x(i) = (x1 * f2 - x2 * f1) / (f2 - f1)
      x(j) = -x(i)
      i = i + 1
      j = j - 1
    end if
    x1 = x2
    f1 = f2
    x2 = x1 + eps
    f2 = poli(x2)
  end do
end subroutine raizes_pl

real*8 function poli(x)
  use gau_leg, only: n, c

  implicit none
  real*8, intent(in) :: x
  integer :: i

  poli = c(n + 1)
  do i = n, 1, -1
    poli = poli * x + c(i) !poli = c(1) + x*(c(2) + x*(c(3) + x*(... c(n+1))...)
  end do
end function poli

subroutine pesos_pl()
  use gau_leg, only: n, x, w !n = no de raizes = no de pesos

  implicit none
  real*8, external :: ps
  real*8 :: y(n - 1), D
  integer :: i, j, k, l

  if(mod(n, 2) /= 0) then
    w = (/(2.0 / n, k = 1, n)/)
    i = (n + 1) / 2 !n = 5 --> i = j = 3 
    j = (n + 1) / 2 !n = 1 --> i = j = 1
  else
    w = (/(0, k = 1, n)/)
    i = n / 2 + 1 !n = 6 --> i = 4, j = 3
    j = n / 2     !n = 2 --> i = 2, j = 1
  end if !os pesos sao simetricos da mesma forma que as raizes

!calculando w(i) = int Li(x) dx usando um metodo bem engenhoso...
  do while(i <= n)
    k = 1
    l = 1
    do k = 1, n - 1
      if(l == i) l = l + 1
      y(k) = - x(l)
      l = l + 1
    end do
    do k = 1, n - 1, 2
      w(i) = w(i) + 2 * ps(y, n - 1, n - k) / k
    end do
    D = 1
    do k = 1, n - 1
      D = D * (x(i) + y(k))
    end do
    w(i) = w(i) / D
    if(j /= i) w(j) = w(i)
    i = i + 1
    j = j - 1
  end do
end subroutine pesos_pl

real*8 function ps(y, n, m)
!Esta funcao retorna a soma de todas as combinacoes de produtos de m elementos
!de y.
!Os loops desta funcao sao tao complexos que foram deixados na forma
!'goto P --> P <comandos>'.
    implicit none
    integer, intent(in) :: n, m
    real*8, intent(in) :: y(n)
    integer*1 :: ind(m), t, i
    real*8 :: A
!ind contem os indices dos elementos de y a serem multiplicados

    ind = (/(i, i = 1, m)/) !exemplo: para n = 6 e m = 4:
                            !ind = (1, 2, 3, 4)
    ps = y(ind(1))          !      (1, 2, 3, 5)
    i = 2                   !      (1, 2, 3, 6)
10  if(i > m) goto 20       !      (1, 2, 4, 5)
    ps = ps * y(ind(i))     !      (1, 2, 4, 6)
    i = i + 1               !      (1, 2, 5, 6)
    goto 10                 !      (1, 3, 4, 5)
                            !      (1, 3, 4, 6)
20  t = m                   !      (1, 3, 5, 6)
30  if(ind(m) == n) goto 60 !      (1, 4, 5, 6)
    ind(m) = ind(m) + 1     !      (2, 3, 4, 5)
                            !      (2, 3, 4, 6)
    A = y(ind(1))           !      (2, 3, 5, 6)
    i = 2                   !      (2, 4, 5, 6)
40  if(i > m) goto 50       !      (3, 4, 5, 6)
    A = A * y(ind(i))
    i = i + 1
    goto 40
50  ps = ps + A

    goto 30
60  t = t - 1
    if(t == 0) goto 11
    if(ind(t) == t + n - m) goto 60
    ind(t) = ind(t) + 1
70  ind(t + 1) = ind(t) + 1
    t = t + 1
    if(t /= m) goto 70

    A = y(ind(1))
    i = 2
80  if(i > m) goto 90
    A = A * y(ind(i))
    i = i + 1
    goto 80
90  ps = ps + A

    goto 30
11  continue
end function ps
