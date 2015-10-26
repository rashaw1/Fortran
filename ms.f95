PROGRAM mspec
  !
  ! PURPOSE: To determine the sum formula for a CHNO molecule
  !          given the molecular mass.
  !
  IMPLICIT NONE

  INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND(15, 307)
  INTEGER :: a
  REAL(dbl) :: mass
  CHARACTER :: keystroke

  DO a = 1, 50 
     WRITE(*, *) 'Enter the mass of the molecule:'
     READ(*, *) mass
     CALL getABCD(mass)
     WRITE(*, *) 'Enter another? (Y/N)'
     READ(*, *) keystroke
     IF (keystroke == 'N') THEN
        EXIT
     END IF
  END DO
END PROGRAM mspec

SUBROUTINE getABCD(mass)
  IMPLICIT NONE

  INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND(15, 307)
  REAL(dbl), INTENT(IN) :: mass
  REAL(dbl) :: MC = 12.0038
  REAL(dbl) :: MN = 14.0075
  REAL(dbl) :: MO = 16.0
  REAL(dbl) :: MH = 1.0081
  REAL(dbl) :: testmass
  INTEGER :: amax, bmax, cmax, dmax, i, j, k, l

  amax = MIN(FLOOR(mass/MC), 10)
  bmax = FLOOR(mass/MN)
  cmax = FLOOR(mass/MO)
  dmax = FLOOR(mass/MH)

  outer: DO i = amax, 0, -1
     inner1: DO j = bmax, 0, -1
        inner2: DO k = cmax, 0, -1
           inner3: DO l = dmax, 0, -1
              testmass = i*MC+j*MN+k*MO+l*MH
              IF (ABS(testmass-mass) < 5E-3) THEN
                 IF ( 4*i + 3*j + 2*k > l) THEN
                    WRITE(*, *) 'Sum formula is: '
                    WRITE(*, '(1X, A2, I2, A2, I2, A2, I2, A2, I2)') 'C', i, 'N', j, 'H', l, 'O', k
                 END IF
              END IF
           END DO inner3
        END DO inner2
     END DO inner1
  END DO outer
END SUBROUTINE getABCD
                 
 
  
