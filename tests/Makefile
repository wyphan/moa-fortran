###############################################################################
# Main Makefile for moa-fortran tests
# Last edited: Oct 25, 2021 (WYP)
###############################################################################

# Please provide a make.inc file.
# You can start from one of the files in the makeinc-db subdirectory
include ../make.inc

###############################################################################
# Prefix rules
###############################################################################

.SUFFIXES: .fypp .f90

# Prevent invocation of m2c (Modula-2 compiler)
%.o: %.mod

%.pp.f90: %.fypp
	$(PY) ../deps/fypp/bin/fypp $< $@

%.pp.f90: %.F90
	$(FC) $(FCOPTS) $(CPPOPTS) $< -o $@

%.f90.o: %.pp.f90
	$(FC) $(FCOPTS) -J$(*D) -I../build/include -c $< -o $(@:.f90.o=.o)

%.f90.o: %.f90
	$(FC) $(FCOPTS) -J$(*D) -I../build/include -c $< -o $(@:.f90.o=.o)

###############################################################################
# Phony targets
###############################################################################

.PHONY: all

all: test_gamma test_drop test_take

###############################################################################
# List sources and targets
###############################################################################

MODF90SRC  := $(shell find . -name mod\*.f90)
MODFPPSRC  := $(shell find . -name mod\*.F90)
MODFYPPSRC := $(shell find . -name mod\*.fypp)

F90SRC  := $(shell find . -name test\*.f90 | grep -v common)
FPPSRC  := $(shell find . -name test\*.F90 | grep -v common)
FYPPSRC := $(shell find . -name test\*.fypp | grep -v common)

PPSRC = $(MODFPPSRC:.F90=.pp.f90) $(MODFYPPSRC:.fypp=.pp.f90) \
          $(FPPSRC:.F90=.pp.f90) $(FYPPSRC:.fypp=.pp.f90)

DEPTGT = $(MODF90SRC:.f90=.f90.o) \
           $(MODFPPSRC:.F90=.f90.o) $(MODFYPPSRC:.fypp=.f90.o)

TGT = $(F90SRC:.f90=.f90.o) $(FPPSRC:.F90=.f90.o) $(FYPPSRC:.fypp=.f90.o)

OBJ = $(MODF90SRC:.f90=.o) $(MODFPPSRC:.F90=.o) $(MODFYPPSRC:.fypp=.o) \
        $(F90SRC:.f90=.o) $(FPPSRC:.F90=.o) $(FYPPSRC:.fypp=.o)

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
# Generic targets
###############################################################################

$(foreach objfile,$(DEPTGT),$(objfile)): prebuild

$(foreach objfile,$(TGT),$(objfile)): $(DEPTGT)

STATICLIB = ../libmoa.$(SYS)-$(ARCH)-$(COMPILER).a

prebuild:
	-[ -e $(STATICLIB) ] || make -C .. static

###############################################################################
# Specific targets
###############################################################################

test_gamma: test_gamma.f90.o
	$(FC) $(FCOPTS) -I$(PREFIX)/include $^ $(STATICLIB) -o test_gamma.exe

test_drop: test_drop.f90.o
	$(FC) $(FCOPTS) -I$(PREFIX)/include $^ $(STATICLIB) -o test_drop.exe

test_take: test_take.f90.o
	$(FC) $(FCOPTS) -I$(PREFIX)/include $^ $(STATICLIB) -o test_take.exe

###############################################################################
# Clean up
###############################################################################

clean:
	-rm -f $(OBJ)
	-rm -f `find . -name \*.mod`
	-rm -f `find . -name \*.exe`
ifneq ($(KEEPF90PP), 1)
	-rm -f $(PPSRC)
endif
