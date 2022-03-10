gfortran -c cmdparse.f90
gfortran -c view_general.f90
gfortran -c moa_view_ndim_flat.f90
gfortran -o test_moa_measure_noopt moa_measure.f90 cmdparse.o view_general.o moa_view_ndim_flat.o
