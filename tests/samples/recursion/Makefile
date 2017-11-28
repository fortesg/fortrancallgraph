FF = gfortran
FFLAGS = 

all: assembler run

run: recursion.x
	./recursion.x

assembler: recursion.s

%.x: %.f90
	$(FF) -g -O0 $(FFLAGS) -o $@ $<

%.s: %.f90
	$(FF) -S -g -O0 $(FFLAGS) $<

clean:
	rm -f *.x *.s
 