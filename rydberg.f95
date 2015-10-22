PROGRAM rydberg
!
! PURPOSE: To determine n1, n2 in the Rydberg formula
!          given the frequency
!

IMPLICIT NONE

INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND(15, 307)
REAL(dbl), ALLOCATABLE :: freqs(:) ! Array of frequencies
REAL :: RH = 109737.32 ! Rydberg constant, in reciprocal cm
INTEGER :: n1, n2 ! The quantum numbers from Rydberg formula
INTEGER :: N ! Number of frequencies
INTEGER :: i ! counter
INTEGER :: n1temp ! To check if same series
LOGICAL :: sameseries = .TRUE.

WRITE(*, *) 'How many frequencies?'
READ(*, *) N

! Check sensible value of N given
IF (N > 0) THEN 
   ! Allocate memory to array
   ALLOCATE(freqs(N))

   ! Get the frequencies
   WRITE(*, *) 'Enter frequencies:'
   DO i = 1, N
      READ(*, *) freqs(i)
   END DO

   ! Calculate the n1, n2 pairs and write out
   WRITE(*, '(1X, A10, A5, A5)') 'Freq.', 'n1', 'n2'
   DO i = 1, N
      CALL getn(freqs(i)/RH, n1, n2) ! Get n1, n2 for this freq.
      
      ! Check if n1 has changed - if it has, these are from diff. series
      IF (i > 1) THEN
         IF (n1 .NE. n1temp) THEN
            sameseries = .FALSE.
         END IF
      ELSE 
         n1temp = n1 ! On first frequency, this is the n1 to test against
      END IF

      WRITE(*, 10) freqs(i), n1, n2 ! Write out data
    END DO
    10 FORMAT(1X, F10.2, I5, I5)
    WRITE(*, *) 'Are these part of the same series?', sameseries
END IF

END PROGRAM rydberg

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE getn(f, n1, n2)
! 
! PURPOSE: To determine the n1, n2 values for a given 'frequency'
!          where f has been normalised by dividing by RH
!          
IMPLICIT NONE

INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND(15, 307)
REAL(dbl), INTENT(IN) :: f ! The normalised frequency (unitless)
INTEGER, INTENT(OUT) :: n1, n2 ! The n1, n2 values to be found
REAL(dbl) :: testf ! To store 1/(n1^2) - 1/(n2^2)
INTEGER :: i, j ! Counters

! Initialise n1, n2 just in case
n1 = 1
n2 = 1

! Loop up to 20/21, because this really isn't meant to be exhaustive
outer: DO i = 1, 20 
   inner: DO j = i+1, 21
      testf = 1./REAL(i*i) - 1./REAL(j*j) ! Calculate test value
      IF (ABS(testf - f)/f < 1E-2) THEN ! See if it matches desired value
         n1 = i
         n2 = j
         EXIT outer ! If so, exit subroutine
      END IF
   END DO inner
END DO outer

END SUBROUTINE getn
