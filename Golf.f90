Real*8 Function gfunc(vel)
  Implicit None
  Real*8 :: vel
  gfunc = 1.0_8 / Sqrt(1.0_8 - vel**2)
End Function gfunc

Real*8 Function Ux(Up, theta, v)
  Implicit None
  Real*8 :: Up, theta, v, Upx
  Upx = Up * Cos(theta)
  Ux = (Upx + v) / (1.0_8 + v * Upx)
End Function Ux

Real*8 Function Uy(Up, theta, v)
  Implicit None
  Real*8 :: Up, theta, v, Upx, Upy
  Real*8 :: gfunc
  Upx = Up * Cos(theta)
  Upy = Up * Sin(theta)
  Uy = Upy / (gfunc(v) * (1.0_8 + v * Upx))
End Function Uy

Subroutine plotGamma
  Use dislin
  Implicit None
  Integer, Parameter :: n=1000
  Integer :: i
  Real*8, Dimension(n) :: xv, yg
  Real*8 :: v, gfunc
  v = 0.0
  do i =1, n
     xv(i) = v
     yg(i) = gfunc(v)
     v = v + .001_8
  end do
  call metafl('XWIN')
  call disini
  call titlin('gamma(v) vs. v', 1)
  call name('v', 'X')
  call name('gamma(v)', 'Y')
  call graf(-0.05_8, 1.05_8, 0._8, .1_8, -1._8, 26._8, 0._8, 5._8)
  call grid(1,1)
  call title
  call color('RED')
  call curve(xv, yg, n)
  call disfin
End Subroutine plotGamma

Program golf_double
  Implicit None
  Real*8 :: Up, Tp, v, theta, T, uxval, uyval, u, phi
  Real*8, Parameter :: PI = 3.14159265358979323846_8
  Real*8 :: gfunc, Ux, Uy
  Up = 1.0_8 / Sqrt(3.0_8)
  Tp = 2.6e7_8
  v = 0.5_8
  theta = 30.0_8 * PI / 180.0_8  
  T = gfunc(Up) * Tp
  uxval = Ux(Up, theta, v)
  uyval = Uy(Up, theta, v)
  u = Sqrt(uxval ** 2 + uyval ** 2)
  phi = Atan2(uyval, uxval)
  print *, 'T =', T
  print *, 'ux =', uxval
  print *, 'uy =', uyval
  print *, 'u =', u
  print *, 'phi =', phi
  call plotGamma
End Program golf_double
