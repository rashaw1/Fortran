!g2scc problems 1.2-1.6 in Fortran
! 1.2: Product of 2 integers
! 1.3: Vectors
! 1.4: Matrices
! 1.5: Names
! 1.5: Averages

Program test
  Implicit None
  Integer :: prog, a, b, ab
  Integer :: counter = 1
  Integer :: i = 1
  Integer :: j = 1
  Integer :: k = 1
  Integer, Dimension(5) :: numbers
  Real*8 :: average = 0.0
  Real*8 :: dotp = 0.0
  Real*8 :: norm1 = 0.0
  Real*8 :: norm2 = 0.0
  Real*8, Dimension(3) :: vec1, vec2
  Logical :: flag = .TRUE.
  Character(15) :: first, last
  
  do while (flag)
     print *, 'Choose a program (2-6):'
     read *, prog
     select case (prog)
        case default
           print *, 'End of Program'
           flag = .FALSE.
        case (2)
           print *, 'Enter first integer:'
           read *, a
           print *, 'Enter second integer:'
           read *, b
           ab = a * b
           print *, a, ' x ', b, ' = ', ab
        case (3)
           do while (j <= 2)
              do while (i <= 3)
                 print *, 'Enter component ', i, ' of vector ', j
                 if (j == 1) then
                    read *, vec1(i)
                 else
                    read *, vec2(i)
                 end if
                 i = i+1
              end do
              j = j+1
              i = 1
           end do
           do while (k <=3)
              dotp = dotp + vec1(k)*vec2(k)
              norm1 = norm1 + vec1(k)**2
              norm2 = norm2 + vec2(k)**2
              k = k + 1
           end do
           norm1 = sqrt(norm1)
           norm2 = sqrt(norm2)
           print *, 'Dot product ', dotp
           print *, 'Norm of vector 1 ', norm1
           print *, 'Norm of vector 2 ', norm2
        case (5)
           print *, 'Enter first name:'
           read *, first
           print *, 'Enter last name:'
           read *, last
           print *, 'Your full name is: ', first, ' ', last
        case (6)
           print *, 'Enter 5 integers:'
           do
              read *, numbers(counter)
              average = average + numbers(counter)
              counter = counter + 1
              if (counter > 5) exit
           end do
           average = average / 5.0
           print *, 'Average = ', average
     end select
  end do
end program test
           
