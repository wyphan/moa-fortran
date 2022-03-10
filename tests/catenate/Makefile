###############################################################################
# Compiler options
###############################################################################

COMPILER = gcc
FC = gfortran
FCFLAGS = -fopt-info
OPTFLAGS = -march=native -mtune=native
LDFLAGS =

# Set this to either 0 or 1 to toggle optimizations
opt ?= 1

###############################################################################
# Executable name
###############################################################################

ifeq ($(opt), 1)
EXE = test_moa_measure.$(COMPILER).exe
else
EXE = test_moa_measure_noopt.$(COMPILER).exe
endif

###############################################################################
# Input filenames
###############################################################################

ifpath = input

nums = $(shell seq 1 5)
arrays = $(foreach n,$(nums),array$(n).inp)
view   = $(foreach n,$(nums),view$(n).inp)
viewb  = $(foreach n,$(nums),view$(n)b.inp)

###############################################################################
# Output filenames
###############################################################################

ofpath = ../../experiment

tarball = "results_`whoami`_`date -I`_catenate.tar.xz"

###############################################################################
# Prefix rules
###############################################################################

.PREFIXES: .f90

# Prevent invocation of m2c (Modula-2 compiler)
%.o: %.mod

%.f90.o: %.f90
ifeq ($(opt), 1)
	$(FC) $(FCFLAGS) $(OPTFLAGS) -c $< -o $*.o
else
	$(FC) $(FCFLAGS) -c $< -o $*.o
endif

###############################################################################

# Phony targets
.PHONY: all

all: main

OBJ = cmdparse.o \
      view_general.o \
      moa_view_ndim_flat.o \
      moa_measure.o
F90MOD = moa_basic_view_types.mod \
         moa_view_types.mod \
         cmdparse.mod

prereq: cmdparse.f90.o

view_general: prereq view_general.f90.o

moa_view_ndim_flat: view_general moa_view_ndim_flat.f90.o

# Build main executable
main: moa_view_ndim_flat moa_measure.f90.o
	$(FC) $(OBJ) -o $(EXE) $(LDFLAGS)

# Run benchmarks
run:
	cp $(EXE) $(ifpath)/; cd $(ifpath); touch run.log; \
	for f in $(arrays); do ./$(EXE) $$f >> run.log; done; \
	for f in $(view); do ./$(EXE) $$f >> run.log; done; \
	for f in $(viewb); do ./$(EXE) $$f >> run.log; done; \
	tar cJf $(tarball) run.log *.inp *.out
	mv $(ifpath)/$(tarball) $(ofpath)/

# Clean up
clean:
	-rm -f $(OBJ)
	-rm -f $(F90MOD)
	-rm -f $(EXE)
