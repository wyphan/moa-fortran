ifort -c view_general.f90 -traceback
ifort -c moa_view_ndim_flat.f90 -traceback
ifort ncatenate_alt.f90 moa_view_ndim_flat.obj view_general.obj -traceback -exe:ncatenate_flat
