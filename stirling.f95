PROGRAM stirling
!
! PURPOSE: To test Stirling's approximation for log(N!)
!

IMPLICIT NONE

INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND(15, 307)
REAL(dbl) :: sum = 0.0 ! Sum of log(n) for n = 2..N
REAL(dbl) :: approx ! Stirling's approximation
INTEGER :: N, i

! Get choice of N from user
WRITE(*, *) 'Choose a value of N'
READ(*, *) N

! Calculate log(N!) as a sum of logs
DO i=2, N
   sum = sum + LOG(REAL(i, kind=dbl))
END DO

! Calculate Stirling's approximation
approx = N*log(REAL(N, kind=dbl)) - N

WRITE(*, 10) 'ACTUAL:', sum ! Real sum
WRITE(*, 10) 'APPROX:', approx ! The approximation
WRITE(*, 10) 'ABSERR:', sum - approx ! The absolute error
WRITE(*, 10) 'RELERR:', 100.*(sum-approx)/sum ! The percentage error
10 FORMAT(1X, A10, F20.6)
END PROGRAM stirling
