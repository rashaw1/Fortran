PROGRAM rpart
  !
  ! PURPOSE: To calculate a rotational partition function at varying temperatures
  !
  
  IMPLICIT NONE

  INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND(15, 307)
  REAL(dbl) :: f, B, T, endT, est
  INTEGER :: i, interval, limit

  WRITE(*, *) 'Enter B in cm-1:'
  READ(*, *) B
  WRITE(*, *) 'Enter starting and ending T:'
  READ(*, *) T, endT
  WRITE(*, *) 'Enter interval:'
  READ(*, *) interval

  WRITE(*, 10) 'T', 'f(T)', 'T/B'
  10 FORMAT(1X, A5, A10, A10)
  limit = FLOOR((endT-T)/interval)
  DO i = 0, limit
     CALL calcF(f, B, T+i*interval)
     est = (T+i*interval)/B
     WRITE(*, 20) T+i*interval, f, est 
     20 FORMAT(1X, F5.0, F10.6, F10.6)
  END DO
END PROGRAM rpart

SUBROUTINE calcF(f, B, T)
  IMPLICIT NONE
  INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND(15, 307)
  REAL(dbl), INTENT(IN) :: B, T
  REAL(dbl), INTENT(OUT) :: f
  REAL(dbl) :: KB = 1.38066/1.986
  INTEGER :: i
  REAL(dbl) :: temp

  f = 0.
  outer: DO i = 0, 100
     temp = (2*i+1)*EXP(-B*i*(i+1)/(KB*T))
     IF (temp < 1E-7) THEN
        EXIT outer
     END IF
     f = f + temp
  END DO outer
END SUBROUTINE calcF
  
