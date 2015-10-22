!Area.f: Calculate area of a circle of specified radius, yo
!----------------------------------------------------------
Program Circlearea
      Implicit None
      Real*8 :: radius, circum, area
      Real*8 :: PI = 3.14159265358979323846
      Integer :: model_n = 1
      print *, 'Enter a radius dawg:'
      read *, radius
      circum = 2.0 * PI * radius
      area = radius * radius * PI
      print *, 'Program number =', model_n
      print *, 'Radius =', radius
      print *, 'Circumference =', circum
      print *, 'Area =', area
End Program Circlearea
