FF = gfortran
FFLAGS = 

all: modules assembler

modules: assignment.o

assembler: assignment.s

%: %.o
	$(FF) -o $@ $<

%.o: %.f90
	$(FF) -c -cpp -g -O0 $(FFLAGS) $<

%.s: %.f90 %.o
	$(FF) -S -cpp -g -O0 $(FFLAGS) $<

clean:
	rm -f *.o *.mod *.s *.x
 