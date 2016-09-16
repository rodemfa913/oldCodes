program combo
    implicit none
    integer*1 :: n, m, t
    integer*1, allocatable :: c(:)
    read*, n, m
    if(m <= 0 .or. m > n) goto 40
    allocate(c(m))
    c = (/(t, t = 1, m)/)
    print*, c
    t = m
10  if(c(m) == n) goto 20
    c(m) = c(m) + 1
    print*, c
    goto 10
20  t = t - 1
    if(t == 0) goto 40
    if(c(t) == t + n - m) goto 20
    c(t) = c(t) + 1
30  c(t + 1) = c(t) + 1
    t = t + 1
    if(t /= m) goto 30
    print*, c
    goto 10
40  continue
end program combo
