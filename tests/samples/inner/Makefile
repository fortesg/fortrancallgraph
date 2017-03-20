FF = gfortran
FFLAGS = 

all: modules assembler

modules: innerprog

assembler: inner.s

innerprog: innerprog.o inner.o
	$(FF) -o $@ $^

innerprog.o: inner.o

%: %.o
	$(FF) -o $@ $<

%.o: %.f90
	$(FF) -c -g $(FFLAGS) $<

%.s: %.f90 %.o
	$(FF) -S -g $(FFLAGS) $<

clean:
	rm -f *.o *.mod *.s	innerprog
 