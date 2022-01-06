ifort -c view_general.f90 -traceback
ifort -c moa_view_ndim_alt.f90 -traceback
ifort ncatenate_alt.f90 moa_view_ndim_alt.obj view_general.obj -traceback
