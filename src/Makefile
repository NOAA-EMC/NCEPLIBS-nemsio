SHELL=/bin/sh

# sources, headers, objects, library and etc.
SRCS    = nemsio_openclose.f90 nemsio_read.f90 nemsio_write.f90 \
          nemsio_module.f90 nemsio_module_mpi.f90
OBJS    = $(patsubst %.f90, %.o, $(SRCS))

all: build

build: $(LIB)

$(LIB): $(OBJS)
	$(AR) $(ARFLAGS) $@ $^

debug: dbg_build

dbg_build: override MPIFFLAGS += $(DEBUG)
dbg_build: $(LIB)

%.o: %.f90
	$(MPIFC) -c $(FREEFORM) $(MPIFFLAGS) $(FFLAGSDEFS) $<

message: $(MSGSRC)
	$(CC) -c $(firstword $(MSGSRC))
	$(FC) -c $(lastword $(MSGSRC))
	$(AR) $(ARFLAGS) $(LIB) $(patsubst %.c, %.o, $(firstword $(MSGSRC)))
	$(AR) $(ARFLAGS) $(LIB) $(patsubst %.f, %.o, $(lastword $(MSGSRC)))
	$(RM) $(patsubst %.c, %.o, $(firstword $(MSGSRC)))
	$(RM) $(patsubst %.f, %.o, $(lastword $(MSGSRC)))
	$(RM) $(MSGSRC)

install:
	[[ -z "$(LIB)" || -z "$(LIB_DIR)" ]] || mv $(LIB) $(LIB_DIR)
	[[ -z "$(MOD)" || -z "$(INC_DIR)" ]] || mv $(MOD) $(INC_DIR)
	[[ -z "$(SRC_DIR)" ]] || cp ./* $(SRC_DIR)

test:
	@echo "*** unit test unavailable currently ***"

clean:
	$(RM) -r $(OBJS) $(MOD) $(LIB)

.SUFFIXES: .f90 .mod .o .a

# Specify targets that do not generate filesystem objects
.PHONY: all build install debug dbg_build message test clean

