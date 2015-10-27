PROGRAM integralTest
  !
  ! PURPOSE: To test out the numerical integration routines in module integrate
  !

  USE integrate
  IMPLICIT NONE

  INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND(15, 307)
  INTEGER :: N, i
  REAL(dbl), DIMENSION(7) :: vals
  
  WRITE(*, *) 'Integral of x^2 between 0 and 1:'
  WRITE(*, 100) 'N', 'LCONST.', 'RCONST.', 'MIDP.', 'TRAP.', 'SIMP.',  'ROMB.', 'M.C.'

  100 FORMAT(1X, A5, A10, A10, A10, A10, A10, A10, A10, A10)
  200 FORMAT(1X, I5, F10.6, F10.6, F10.6, F10.6, F10.6, F10.6, F10.6)
  
  DO N = 2, 50, 2
     CALL constant(vals(1), vals(2), 0.d0, 1.d0, f1, N)
     CALL midpoint(vals(3), 0.d0, 1.d0, f1, N)
     CALL trapezium(vals(4), 0.d0, 1.d0, f1, N)
     CALL simpson(vals(5), 0.d0, 1.d0, f1, N)
     CALL romberg(vals(6), 0.d0, 1.d0, f1, N)
     CALL montecarlo(vals(7), 0.d0, 1.d0, f1, N*100)
     WRITE(*, 200) N, (vals(i), i=1,7)
  END DO

  WRITE(*, *) ''
  WRITE(*, *) 'Integral of 4.5exp(-x^2) between 1 and 5:'
  WRITE(*, 100) 'N', 'LCONST.', 'RCONST.', 'MIDP.', 'TRAP.', 'SIMP.', 'ROMB.', 'M.C.'

  DO N = 2, 50, 2
     CALL constant(vals(1), vals(2), 1.d0, 5.d0, f2, N)
     CALL midpoint(vals(3), 1.d0, 5.d0, f2, N)
     CALL trapezium(vals(4), 1.d0, 5.d0, f2, N)
     CALL simpson(vals(5), 1.d0, 5.d0, f2, N)
     CALL romberg(vals(6), 1.d0, 5.d0, f2, N)
     CALL montecarlo(vals(7), 1.d0, 5.d0, f2, N*100)
     WRITE(*, 200) N, (vals(i), i=1,7)
  END DO
  
CONTAINS

  REAL(dbl) PURE FUNCTION f1(y)
    REAL(dbl), INTENT(IN) :: y
    f1 = y**2
  END FUNCTION f1

  REAL(dbl) PURE FUNCTION f2(y)
    REAL(dbl), INTENT(IN) :: y
    f2 = 4.5*EXP(-Y**2)
  END FUNCTION f2

END PROGRAM integralTest
  
