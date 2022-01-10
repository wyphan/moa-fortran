gfortran -c view_general.f90 -fbacktrace
gfortran -c moa_view_ndim_flat.f90 -fbacktrace
gfortran ncatenate_alt.f90 moa_view_ndim_flat.o view_general.o -fbacktrace -o ncatenate_flat
