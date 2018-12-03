FF = gfortran
FFLAGS = 

all: modules assembler run

modules: openmp.o

assembler: openmp.s

run: openmp
	export OMP_NUM_THREADS=4
	./openmp

%: %.o
	$(FF) -fopenmp -o $@ $<

%.o: %.f90
	$(FF) -c -cpp -g -O0 -fopenmp $(FFLAGS) $<

%.s: %.f90 %.o
	$(FF) -S -cpp -g -O0 -fopenmp $(FFLAGS) $<

clean:
	rm -f *.o *.mod *.s *.x
 