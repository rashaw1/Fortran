PROGRAM sieve
!
!  PURPOSE: To implement the sieve of Eratosthenes
!           , a method of finding all primes less than a given number N.
!

IMPLICIT NONE

INTEGER, ALLOCATABLE :: unsieved (:) ! Array of unsieved values
INTEGER, ALLOCATABLE :: sieved (:) ! Array of primes
INTEGER :: N, i, j, ERR, m ! i, j counters, m num. of primes, ERR for allocate
REAL :: sqn ! square root of N

! Get N from user
WRITE(*, *) 'Primes up to...?' 
READ(*, *) N

m = N-1 ! Initialise counter for primes

! Check that sensible number has been given
IF (N > 1) THEN
   ! If so, allocate necessary memory, and form 
   ! array of integers 2 to N.
   ALLOCATE(unsieved(N-1), STAT = ERR)
   DO i = 2, N 
      unsieved(i-1) = i
   END DO
ELSE 
   WRITE(*, *) 'N must be bigger than 1'
END IF

sqn = SQRT(REAL(N)) ! Get square root

! Now sieve out multiples of primes
DO i = 1, FLOOR(sqn)  
   IF (unsieved(i) > 0) THEN ! Only 0 if already sieved
      DO j=i+unsieved(i), N, unsieved(i) ! Get rid of all multiples up to N
         IF ( unsieved(j) > 0 ) THEN
            unsieved(j) = 0 
            m = m -1 ! Only decrement number of primes if not already 0
         END IF
      END DO
   END IF
END DO

ALLOCATE(sieved(m), STAT=ERR) ! Form matrix of primes

! Copy primes for unsieved into sieved
j = 1
DO i = 1, N-1
   IF (unsieved(i) > 0) THEN
      sieved(j) = unsieved(i)
      j = j + 1
   END IF
END DO

! Write primes out to screen
WRITE(*, *) 'The primes up to', N
WRITE(*, 10) (i, sieved(i), i = 1, m)
10 FORMAT(1X, I5, I10)

END PROGRAM sieve
