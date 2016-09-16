program episilom
  implicit none
  real*8 :: x
  x = 2.0 ** (-52.0)
  print*, x
  x = 1.0
  print*, x, epsilon(x)
  x = 0.5
  print*, x, x * epsilon(x)
  x = 0.25
  print*, x, x * epsilon(x)
  x = 0.125
  print*, x, x * epsilon(x)
  x = 0.0625
  print*, x, x * epsilon(x)
end program episilom
