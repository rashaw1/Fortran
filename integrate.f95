MODULE integrate
  !
  ! PURPOSE: To define a library of numerical integration methods
  !

CONTAINS

  SUBROUTINE romberg(val, x0, x1, f, N)
    !
    ! PURPOSE: To use the Romberg convergence acceleration method
    !          in conjuction with trapezium rule for numerical integration
    !
    IMPLICIT NONE
    INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND(15, 307)
    REAL(dbl), INTENT(IN) :: x0, x1 ! Limits of integration
    REAL(dbl), INTENT(OUT) :: val ! Return value
    INTEGER, INTENT(IN) :: N ! Number of intervals
    INTEGER :: i, j ! Counters
    INTEGER :: MAX = 10 ! Max number of iterations
    REAL(dbl) :: h ! initial width of intervals
    REAL(dbl), DIMENSION(10, 10) :: R ! Romberg values
    REAL(dbl) :: CUTOFF = 1e-7
    
    ! Declare interface to user defined function
    INTERFACE
       REAL(dbl) PURE FUNCTION f(y)
         IMPORT dbl
         REAL(dbl), INTENT(IN) :: y
       END FUNCTION f
    END INTERFACE

    ! Get R(1, i) values
    DO i = 1, MAX
       CALL trapezium(R(1, i), x0, x1, f, N*(2**(i-1)))
    END DO

    ! Use recursion until cutoff is reached
    val = 0.d0
    outer: DO i = 2, MAX
       inner: DO j = i, MAX
          R(i, j) = R(i-1, j) + (R(i-1, j) - R(i-1, j-1))/((4.d0)**i - 1.d0)
          IF (ABS(R(i, j) - val) < CUTOFF) THEN
             val = R(i, j)
             EXIT outer
          END IF
          val = R(i, j)
       END DO inner
    END DO outer
  END SUBROUTINE romberg
    
  SUBROUTINE constant(lval, rval, x0, x1, f, N)
    !
    ! PURPOSE: The constant rule for integration - estimate by rectangles
    !          under the graph of the function.
    !
    IMPLICIT NONE
    INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND(15, 307)
    REAL(dbl), INTENT(IN) :: x0, x1 ! Limits of integration
    REAL(dbl), INTENT(OUT) :: lval, rval ! Return values
    INTEGER, INTENT(IN) :: N ! Number of intervals
    INTEGER :: i ! Counter
    REAL(dbl) :: width ! width of intervals
    
    ! Declare interface to user defined function
    INTERFACE
       REAL(dbl) PURE FUNCTION f(y)
         IMPORT dbl
         REAL(dbl), INTENT(IN) :: y
       END FUNCTION f
    END INTERFACE

    ! Intialise return values
    ! rval uses the right hand value for the function
    ! lval uses the left
    rval = 0.
    lval = 0.

    ! Calculate interval width
    width = (x1-x0)/(REAL(N))
    
    ! Main loop - I = sum_i(f(x_i)*width)
    DO i = 1, N-1
       rval = rval + f(x0 + i*width)*width
       lval = lval + f(x0 + (i-1)*width)*width
    END DO
  END SUBROUTINE constant

  SUBROUTINE midpoint(val, x0, x1, f, N) 
    !
    ! PURPOSE: The midpoint rule for integration - evaluate at midpoint
    !          of interval instead of endpoints
    !    
    IMPLICIT NONE
    INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND(15, 307)
    REAL(dbl), INTENT(IN) :: x0, x1 ! Limits of integration
    REAL(dbl), INTENT(OUT) :: val ! Return value
    INTEGER, INTENT(IN) :: N ! Number of intervals
    INTEGER :: i ! Counter
    REAL(dbl) :: width ! width of intervals

    ! Declare interface to user defined function
    INTERFACE
       REAL(dbl) PURE FUNCTION f(y)
         IMPORT dbl
         REAL(dbl), INTENT(IN) :: y
       END FUNCTION f
    END INTERFACE

    ! Initialise return value and calculate interval width
    val = 0.
    width = (x1-x0)/(REAL(N))

    ! Main loop
    ! I = sum_i(width*f(x0+(i+0.5)*width))
    DO i = 1, N-1
       val = val + width*f(x0+(i+0.5)*width)
    END DO
  END SUBROUTINE midpoint

  SUBROUTINE trapezium(val, x0, x1, f, N)
    !
    ! PURPOSE: The trapezium rule for integration - uses trapezoids instead
    !          of rectangles
    !
    IMPLICIT NONE
    INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND(15, 307)
    REAL(dbl), INTENT(IN) :: x0, x1 ! Limits of integration
    REAL(dbl), INTENT(OUT) :: val ! Return value
    INTEGER, INTENT(IN) :: N ! Number of intervals
    INTEGER :: i ! Counter
    REAL(dbl) :: width ! width of intervals

    ! Declare interface to user defined function
    INTERFACE
       REAL(dbl) PURE FUNCTION f(y)
         IMPORT dbl
         REAL(dbl), INTENT(IN) :: y
       END FUNCTION f
    END INTERFACE

    ! Initialise return value and calculate interval width
    width = (x1-x0)/(REAL(N))
    val = 0.5*width*(f(x0) + f(x1))
    
    ! Main loop
    ! I = sum_i(width*f(x0+(i+0.5)*width))
    DO i = 1, N-1
       val = val + width*f(x0+i*width)
    END DO
  END SUBROUTINE trapezium

  SUBROUTINE simpson(val, x0, x1, f, N)
    !
    ! PURPOSE: Simpson's rule for integration - using weighted abscissae instead of fixed shapes
    !
    IMPLICIT NONE
    INTEGER, PARAMETER :: dbl = SELECTED_REAL_KIND(15, 307)
    REAL(dbl), INTENT(IN) :: x0, x1 ! Limits of integration
    REAL(dbl), INTENT(OUT) :: val ! Return value
    INTEGER, INTENT(IN) :: N ! Number of intervals
    INTEGER :: i ! Counter
    REAL(dbl) :: width ! width of intervals

    ! Declare interface to user defined function
    INTERFACE
       REAL(dbl) PURE FUNCTION f(y)
         IMPORT dbl
         REAL(dbl), INTENT(IN) :: y
       END FUNCTION f
    END INTERFACE

    ! Initialise return value and calculate interval width
    width = (x1-x0)/(REAL(N))
    val = f(x0) + f(x1)

    ! Main loop
    DO i = 1, N, 2
       val = val + 4.*f(x0+i*width)
    END DO

    DO i = 2, N-1, 2
       val = val + 2.*f(x0+i*width)
    END DO

    val = val*width/3.d0

  END SUBROUTINE simpson
  
END MODULE integrate
