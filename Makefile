###############################################################################
# Main Makefile for moa-fortran
# Last edited: Sep 14, 2021 (WYP)
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

%.pp.f90: %.F90
	$(FC) $(FCOPTS) $(CPPOPTS) $< -o $@

%.f90.o: %.pp.f90
	$(FC) $(FCOPTS) $(MODFLAG)$(*D) -Isrc/common -c $< -o $(@:.f90.o=.o)

%.f90.o: %.f90
	$(FC) $(FCOPTS) $(MODFLAG)$(*D) -Isrc/common -c $< -o $(@:.f90.o=.o)

###############################################################################
# Phony targets
###############################################################################

.PHONY: all

all: static shared

###############################################################################
# List sources and targets
###############################################################################

MODF90SRC  := $(shell find src/common -name \*.f90)
MODFPPSRC  := $(shell find src/common -name \*.F90)
MODFYPPSRC := $(shell find src/common -name \*.fypp)

F90SRC  := $(shell find src -name \*.f90 | grep -v common)
FPPSRC  := $(shell find src -name \*.F90 | grep -v common)
FYPPSRC := $(shell find src -name \*.fypp | grep -v common)

PPSRC = $(MODFPPSRC:.F90=.pp.f90) $(MODFYPPSRC:.fypp=.pp.f90) \
          $(FPPSRC:.F90=.pp.f90) $(FYPPSRC:.fypp=.pp.f90)

DEPTGT = $(MODF90SRC:.f90=.f90.o) \
           $(MODFPPSRC:.F90=.f90.o) $(MODFYPPSRC:.fypp=.f90.o)

TGT = $(F90SRC:.f90=.f90.o) $(FPPSRC:.F90=.f90.o) $(FYPPSRC:.fypp=.f90.o)

OBJ = mod_moa.o \
        $(MODF90SRC:.f90=.o) $(MODFPPSRC:.F90=.o) $(MODFYPPSRC:.fypp=.o) \
        $(F90SRC:.f90=.o) $(FPPSRC:.F90=.o) $(FYPPSRC:.fypp=.o)
F90MOD = moa.mod \
           $(MODF90SRC:%.f90=mod_%.mod) \
           $(MODFPPSRC:%.F90=mod_%.mod) $(MODFYPPSRC:%.fypp=mod_%.mod) \
           $(F90SRC:%.f90=mod_%.mod) \
           $(FPPSRC:%.F90=mod_%.mod) $(FYPPSRC:%.fypp=mod_%.mod)

###############################################################################
# Preprocess with fypp
###############################################################################

$(foreach f90file,$(MODFYPPSRC:.fypp=.pp.f90),$(f90file)):

$(foreach f90file,$(FYPPSRC:.fypp=.pp.f90),$(f90file)):

###############################################################################
# Preprocess with cpp
###############################################################################

$(foreach f90file,$(MODFPPSRC:.F90=.pp.f90),$(f90file)):

$(foreach f90file,$(FPPSRC:.F90=.pp.f90),$(f90file)):

###############################################################################
# Build static library
###############################################################################

$(foreach objfile,$(DEPTGT),$(objfile)):

$(foreach objfile,$(TGT),$(objfile)): $(DEPTGT)

STATICLIB = libmoa.$(SYS)-$(ARCH)-$(COMPILER).a

mod_moa: $(TGT)
	$(FC) $(FCOPTS) -Isrc/common -Isrc -c mod_moa.f90 -o mod_moa.o

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
	-rm -f $(STATICLIB)
	-rm -f $(OBJ)
	-rm -f `find . -name \*.mod`
ifneq ($(KEEPF90PP), 1)
	-rm -f $(PPSRC)
endif
