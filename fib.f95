PROGRAM fibonacci
!
! PURPOSE: To calculate fibonacci numbers
!

IMPLICIT NONE

INTEGER, DIMENSION(25) :: arr ! Array of 
INTEGER :: i
INTEGER :: limit

WRITE(*, *) 'How many Fibonacci numbers?'
READ(*, *) limit

limit = MAX(3, limit)

arr(1) = 1
arr(2) = 1
DO i = 3, LIMIT
   arr(i) = arr(i-1)+arr(i-2)
END DO

WRITE(*, *) 'THE FIRST ', limit, ' FIBONACCI NUMBERS:'
WRITE(*, '(1X, A5, A10)') 'n', 'F(n)' 
WRITE(*, 10) (i, arr(i), i=1, limit)
10 FORMAT(1X, I5, I10)

END PROGRAM fibonacci
