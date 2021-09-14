MODULE mod_prec

  ! Note: This intrinsic module is provided starting in Fortran 2003
  USE ISO_FORTRAN_ENV, ONLY: CHARACTER_STORAGE_SIZE, int32

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: dl, dd, dc, dz, sz_l, sz_d, sz_c, sz_z
  public :: int32

  INTRINSIC :: STORAGE_SIZE

  ! Precision constants
  INTEGER, PARAMETER :: dl = SELECTED_INT_KIND(18)      ! 64-bit (long)    !this ensures by int64 provided by iso_fortran_env
  INTEGER, PARAMETER :: dd = SELECTED_REAL_KIND(14,100) ! 64-bit (double)
  INTEGER, PARAMETER :: dc = KIND( (0.0, 1.0) )
  INTEGER, PARAMETER :: dz = KIND( (0.0_dd, 1.0_dd) )

  ! Dummy variables for STORAGE_SIZE()
  INTEGER(KIND=dl) :: l
  REAL(KIND=dd)    :: d
  COMPLEX(KIND=dc) :: c
  COMPLEX(KIND=dz) :: z

  ! Sizes in bytes
  INTEGER(KIND=dl), PARAMETER :: sz_l = STORAGE_SIZE(l) / CHARACTER_STORAGE_SIZE
  INTEGER(KIND=dl), PARAMETER :: sz_d = STORAGE_SIZE(d) / CHARACTER_STORAGE_SIZE
  INTEGER(KIND=dl), PARAMETER :: sz_c = STORAGE_SIZE(c) / CHARACTER_STORAGE_SIZE
  INTEGER(KIND=dl), PARAMETER :: sz_z = STORAGE_SIZE(z) / CHARACTER_STORAGE_SIZE

END MODULE mod_prec
