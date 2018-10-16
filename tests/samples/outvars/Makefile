FF = gfortran
FFLAGS = 

all: modules assembler

modules: outvars.o

assembler: outvars.s

%: %.o
	$(FF) -o $@ $<

%.o: %.f90
	$(FF) -c -cpp -g -O0 $(FFLAGS) $<

%.s: %.f90 %.o
	$(FF) -S -cpp -g -O0 $(FFLAGS) $<

clean:
	rm -f *.o *.mod *.s *.x
 