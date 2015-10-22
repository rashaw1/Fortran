! Calculates the roots of a quadratic equation

PROGRAM Quadratic
  IMPLICIT NONE
  
  !Define variables
  REAL :: a, b, c !Coefficients of ax^2 + bx + c
  REAL :: discriminant !Discriminant of equation
  REAL :: x1, x2 !Real solutions
  REAL :: imag_part, real_part !Imaginary solutions
  
  !Get coefficients from user
  WRITE (*,*) 'Enter a, b, and c: '
  READ (*,*) a, b, c
  WRITE (*,*) 'The solutions of ', a, 'x^2 + ', b, 'x + ', c, 'are :'
  
  !Calculate discriminant
  discriminant = b**2 - 4. * a * c
  
  !Determine roots
  !Two real solutions
  IF (discriminant > 1.E-8) THEN
     x1 = (-b)/(2. * a) + (SQRT(discriminant)/(2. * a))
     x2 = (-b)/(2. * a) - (SQRT(discriminant)/(2. * a))
     WRITE (*,*) 'x1 =   ', x1
     WRITE (*,*) 'x2 =   ', x2
  !Two complex roots
  ELSE IF (discriminant < -1.E-8) THEN
     imag_part = SQRT(ABS(discriminant))/(2. * a)
     real_part = (-b)/(2. * a)
     WRITE (*,*) 'x1 =   ', real_part, ' +i ', imag_part
     WRITE (*,*) 'x2 =   ', real_part, ' -i ', imag_part
  !Repeated root
  ELSE
     x1 = (-b)/(2. * a)
     WRITE (*,*) 'This has a repeated root of ', x1
  END IF
END PROGRAM Quadratic
