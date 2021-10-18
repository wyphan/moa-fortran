module modheader
 use, intrinsic:: iso_fortran_env, only: output_unit, compiler_version, compiler_options
 implicit none
 private
 public:: header

contains

subroutine header()
 write(output_unit, '(2a)')'Compiler        : ',compiler_version()
 write(output_unit, '(2a)')'Compiler options: ',compiler_options()
end subroutine

end module
