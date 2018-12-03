FF = gfortran
FFLAGS = 

all: modules assembler

modules: openmp.o

assembler: openmp.s

%: %.o
	$(FF) -fopenmp -o $@ $<

%.o: %.f90
	$(FF) -c -cpp -g -O0 -fopenmp $(FFLAGS) $<

%.s: %.f90 %.o
	$(FF) -S -cpp -g -O0 -fopenmp $(FFLAGS) $<

clean:
	rm -f *.o *.mod *.s *.x
 