SHELL   =/bin/sh
EXEC    =nemsio_read
LIBDIR=/gpfs/hps/nco/ops/nwprod/lib
TEST=../intel
INCMOD=../intel/include/nemsio_v2.2.5
FC      =  ftn
FOPTS  = -O -FR -I$(INCMOD)
LIBS   =-L$(TEST) -lnemsio_v2.2.5 -L$(LIBDIR)/bacio/v2.0.1/intel -lbacio_v2.0.1_4 -L$(LIBDIR)/w3emc/v2.2.0/intel -lw3emc_v2.2.0_d -L$(LIBDIR)/w3nco/v2.0.6/intel -lw3nco_v2.0.6_d
#LIBS   =-L$(TEST) -lnemsio_v2.2.5
OBJS = read_nemsio.o
SRCS = read_nemsio.f
# *************************************************************************
all: $(OBJS)

$(OBJS): $(SRCS)
	$(FC)  $(FOPTS) $(SRCS) -o $(EXEC) $(LIBS)

