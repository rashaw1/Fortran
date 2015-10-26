F95 = gfortran
FFLAGS = -O -C
GNUPLOT = gnuplot

%.o: %.f95
	$(F95) $(FFLAGS) -c $*.f95

objfiles = integrate.o main.o

all: out.txt

out.txt: main makefile
	./main.x > out.txt
	cat out.txt

main: $(objfiles)
	$(F95) $(FFLAGS) $(objfiles) -o main.x

clean:
	rm -f $(objfiles) core* main.x *.mod *.dat *.png *~
