FF = gfortran
FFLAGS = 

all: modules assembler

modules: RHS_mod.o

assembler: RHS_mod.s

%: %.o
	$(FF) -o $@ $<

%.o: %.f90
	$(FF) -c -g $(FFLAGS) $<

%.s: %.f90 %.o
	$(FF) -S -g $(FFLAGS) $<

clean:
	rm -f *.o *.mod *.s
 