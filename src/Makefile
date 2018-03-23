#include conf/configure


LIB	= $(LIBDIR)/libnemsio_$(VER).a
INCMOD	= $(LIBDIR)/$(INC)
FC      = $(FCOMP)
FFLAGS      = -O -g
AR          = ar
ARFLAGS     = -rvu
RM          = rm


OBJS    = nemsio_openclose.o nemsio_read.o nemsio_write.o nemsio_module.o nemsio_module_mpi.o

$(LIB):	$(OBJS)
	$(AR) $(ARFLAGS) $@ $(OBJS)
	mkdir -p $(INCMOD)
	mkdir -p $(INCMOD)/nemsio_$(VER)
	mv *.mod $(INCMOD)/nemsio_$(VER)
	rm -f *.o

clean:
	mkdir -p $(INCMOD)/nemsio_$(VER)
	$(RM) -f *.o *.mod *.a $(LIB) -r $(INCMOD)

.SUFFIXES: .c .f90 .f .o

.f90.o:
	$(FC) $(FFLAGS) -c $*.f90

.f.o:
	$(FC) $(FFLAGS) $(FIXED) -c $*.f

%.o: %.mod

nemsio_read.o: nemsio_openclose.o
nemsio_write.o: nemsio_openclose.o
nemio_module.o: nemsio_openclose.o nemsio_read.o nemsio_write.o
