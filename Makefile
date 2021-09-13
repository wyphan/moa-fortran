###############################################################################
# Main Makefile for moa-fortran
# Last edited: Sep 12, 2021 (WYP)
###############################################################################

# Please provide a make.inc file.
# You can start from one of the files in the makeinc-db subdirectory
include make.inc

###############################################################################
# Prefix rules
###############################################################################

.SUFFIXES: .fypp .f90

# Prevent invocation of m2c (Modula-2 compiler)
%.o: %.mod

%.pp.f90: %.fypp
	$(PY) deps/fypp/bin/fypp $< $@

%.f90.o: %.pp.f90
	$(FC) $(FCOPTS) -c $< -o $*.o

%.f90.o: %.f90
	$(FC) $(FCOPTS) -c $< -o $*.o

###############################################################################
# Phony targets
###############################################################################

.PHONY: all

all: static shared

###############################################################################
# List sources and targets
###############################################################################

MODFYPPSRC = $(shell find src/common -name \*.fypp)
MODF90SRC = $(shell find src/common -name \*.[fF]90)

FYPPSRC = $(shell find src -name \*.fypp | grep -v common)
F90SRC = $(shell find src -name \*.[fF]90 | grep -v common)

DEPTGT = $(MODF90SRC:.f90=.f90.o) $(MODFYPPSRC:.fypp=.f90.o)

TGT = $(F90SRC:.f90=.f90.o) $(FYPPSRC:.fypp=.f90.o)

OBJ = mod_moa.o \
      $(MODF90SRC:.f90=.o) $(MODFYPPSRC:.fypp=.o) \
      $(F90SRC:.f90=.o) $(FYPPSRC:.fypp=.o)
MOD = mod_moa.mod \
      $(MODF90SRC:.F90=.mod) $(MODFYPPSRC:.fypp=.mod) \
      $(F90SRC:.f90=.mod) $(FYPPSRC:.fypp=.mod)

###############################################################################
# Preprocess with fypp
###############################################################################

$(foreach f90file,$(MODFYPPSRC:.fypp=.pp.f90),$(f90file)):

$(foreach f90file,$(FYPPSRC:.fypp=.pp.f90),$(f90file)):

###############################################################################
# Build static library
###############################################################################

$(foreach objfile,$(DEPTGT),$(objfile)):

$(foreach objfile,$(TGT),$(objfile)): $(DEPTGT)

STATICLIB = libmoa.$(SYS)-$(ARCH)-$(COMPILER).a

mod_moa.f90.o: $(TGT)

mod_moa: mod_moa.f90.o

static: mod_moa
	$(AR) r $(STATICLIB) $(OBJ)
	$(AR) s $(STATICLIB)

###############################################################################
# Build shared library
###############################################################################

# TODO
shared: $(TGT)

###############################################################################
# Test
###############################################################################

test: mod_moa
	make -C tests

###############################################################################
# Install
###############################################################################

# TODO
install:

###############################################################################
# Clean up
###############################################################################

clean:
	-rm -f $(shell find . -name \*.o)
	-rm -f $(shell find . -name \*.mod)
