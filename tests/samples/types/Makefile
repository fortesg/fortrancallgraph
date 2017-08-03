FF = gfortran
FFLAGS = 

all: modules

modules: types.o

types.o: modA.o modB.o modC.o

%: %.o
	$(FF) -o $@ $<

%.o: %.f90
	$(FF) -c -cpp -g -O0 $(FFLAGS) $<

clean:
	rm -f *.o *.mod *.s
 