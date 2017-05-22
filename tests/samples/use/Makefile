FF = gfortran
FFLAGS = 

all: assembler

assembler: bottom.s middle.s top.s 

%.o: %.f90
	$(FF) -c -g $(FFLAGS) $<

%.s: %.f90 %.o
	$(FF) -S -g $(FFLAGS) $<

clean:
	rm -f *.o *.mod *.s
 