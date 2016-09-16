#0   1   2   3   4   5   6   7   8   9   a   1   2   3   4   5   6   7   8   9   b   1
#|<----------------------------------------H---------------------------------------->|
#               |<--N-->|   |<--N-->|                                            |<i>|
#                               |<----Q---->|
#|<----C---->|<-------------T-------------->|
#                                        |<------------------P------------------>|
#                   |<----------B---------->|
H = input('H = ')
if H <= 0:
    print 'H must be > 0'
    quit()
if H % 1 != 0:
    print 'H must be an integer'
    quit()
C = input('C = ')
if C >= 2. * H / 3.:
    print 'C must be < 2H/3'
    quit()
# 1 2 3 4 6 7 12 14 21 28 42 84 
# 1 2 3   6 7    14 21    42
# 1 2   4   7    14    28
# 1   3     7       21
# 1 2       7    14
# 1 2 3 4 6   12
# 1         7
# 1 2 3   6
# 1 2   4
# 1   3
# 1 2
if H % 84 == 0:
    print 'i = %d' % (H / 21)
    print 'N = %d' % (2 * H / 21)
    print 'Q = %d' % (H / 7)
    print 'T = %d' % (31 * H / 84)
    print 'P = %d' % (13 * H / 21 - C)
    print 'B = %d' % (2 * H / 7)
elif H % 42 == 0:
    print 'i = %d' % (H / 21)
    print 'N = %d' % (2 * H / 21)
    print 'Q = %d' % (H / 7)
    print 'T = %d + %d/2', (31 * H / 84, (31 * H / 42) % 2) 
    print 'P = %d' % (13 * H / 21 - C)
    print 'B = %d' % (2 * H / 7)
elif H % 28 == 0: # 112
    print 'i = %d + %d/3' % (H / 21, (H / 7) % 3) # 16 / 3 = 5 + 1 / 3
    print 'N = %d + %d/3' % (2 * H / 21, (2 * H / 7) % 3)
    print 'Q = %d' % (H / 7)
    print 'T = %d + %d/3' % (31 * H / 84, (31 * H / 28) % 3)
    print 'P = %d + %d/3' % (13 * H / 21 - C, (13 * H / 7) % 3)
    print 'B = %d' % (2 * H / 7)
elif H % 21 == 0:
    print 'i = %d' % (H / 21)
    print 'N = %d' % (2 * H / 21)
    print 'Q = %d' % (H / 7)
    print 'T = %d + %d/4' % (31 * H / 84, (31 * H / 21) % 4)
    print 'P = %d' % (13 * H / 21 - C)
    print 'B = %d' % (2 * H / 7)
elif H % 14 == 0:
    print 'i = %d + %d/3' % (H / 21, (H / 7) % 3)
    print 'N = %d + %d/3' % (2 * H / 21, (2 * H / 7) % 3)
    print 'Q = %d' % (H / 7)
    print 'T = %d + %d/6' % (31 * H / 84, (31 * H / 14) % 6)
    print 'P = %d + %d/3' % (13 * H / 21 - C, (13 * H / 7) % 3)
    print 'B = %d' % (2 * H / 7)
elif H % 12 == 0: # 60
    print 'i = %d + %d/7' % (H / 21, (H / 3) % 7) # 20 / 7 = 2 + 6 / 7
    print 'N = %d + %d/7' % (2 * H / 21, (2 * H / 3) % 7)
    print 'Q = %d + %d/7' % (H / 7, H % 7) # 60 / 7 = 8 + 4 / 7
    print 'T = %d + %d/7' % (31 * H / 84, (31 * H / 12) % 7)
    print 'P = %d + %d/7' % (13 * H / 21 - C, (13 * H / 3) % 7)
    print 'B = %d + %d/7' % (2 * H / 7, (2 * H) % 7)
elif H % 7 == 0:
    print 'i = %d + %d/3' % (H / 21, (H / 7) % 3)
    print 'N = %d + %d/3' % (2 * H / 21, (2 * H / 7) % 3)
    print 'Q = %d' % (H / 7)
    print 'T = %d + %d/12' % (31 * H / 84, (31 * H / 7) % 12)
    print 'P = %d + %d/3' % (13 * H / 21 - C, (13 * H / 7) % 3)
    print 'B = %d' % (2 * H / 7)
elif H % 6 == 0:
    print 'i = %d + %d/7' % (H / 21, (H / 3) % 7)
    print 'N = %d + %d/7' % (2 * H / 21, (2 * H / 3) % 7)
    print 'Q = %d + %d/7' % (H / 7, H % 7)
    print 'T = %d + %d/14' % (31 * H / 84, (31 * H / 6) % 14)
    print 'P = %d + %d/7' % (13 * H / 21 - C, (13 * H / 3) % 7)
    print 'B = %d + %d/7' % (2 * H / 7, (2 * H) % 7)
elif H % 4 == 0:
    print 'i = %d + %d/21' % (H / 21, H % 21)
    print 'N = %d + %d/21' % (2 * H / 21, (2 * H) % 21)
    print 'Q = %d + %d/7' % (H / 7, H % 7)
    print 'T = %d + %d/21' % (31 * H / 84, (31 * H / 4) % 21)
    print 'P = %d + %d/21' % (13 * H / 21 - C, (13 * H) % 21)
    print 'B = %d + %d/7' % (2 * H / 7, (2 * H) % 7)
elif H % 3 == 0:
    print 'i = %d + %d/7' % (H / 21, (H / 3) % 7)
    print 'N = %d + %d/7' % (2 * H / 21, (2 * H / 3) % 7)
    print 'Q = %d + %d/7' % (H / 7, H % 7)
    print 'T = %d + %d/28' % (31 * H / 84, (31 * H / 3) % 28)
    print 'P = %d + %d/7' % (13 * H / 21 - C, (13 * H / 3) % 7)
    print 'B = %d + %d/7' % (2 * H / 7, (2 * H) % 7)
elif H % 2 == 0:
    print 'i = %d + %d/21' % (H / 21, H % 21)
    print 'N = %d + %d/21' % (2 * H / 21, (2 * H) % 21)
    print 'Q = %d + %d/7' % (H / 7, H % 7)
    print 'T = %d + %d/42' % (31 * H / 84, (31 * H / 2) % 42)
    print 'P = %d + %d/21' % (13 * H / 21 - C, (13 * H) % 21)
    print 'B = %d + %d/7' % (2 * H / 7, (2 * H) % 7)
else:
    print 'i = %d + %d/21' % (H / 21, H % 21)
    print 'N = %d + %d/21' % (2 * H / 21, (2 * H) % 21)
    print 'Q = %d + %d/7' % (H / 7, H % 7)
    print 'T = %d + %d/84' % (31 * H / 84, (31 * H) % 84)
    print 'P = %d + %d/21' % (13 * H / 21 - C, (13 * H) % 21)
    print 'B = %d + %d/7' % (2 * H / 7, (2 * H) % 7)
