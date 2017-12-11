all: preprocessed.o

modules: 

%.o: ../src/%.f90
	gfortran -c -cpp -save-temps -g -O0 -o $@ $<

clean:
	rm -f *.o *.mod *.s *.f90
 