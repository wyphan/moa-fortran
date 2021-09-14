!===============================================================================
! Module for portable precision constants
! Last edited: Sep 14, 2021 (JVDP)
!===============================================================================
MODULE mod_prec

  ! Note: This intrinsic module is provided starting in Fortran 2003
  USE ISO_FORTRAN_ENV, ONLY: CHARACTER_STORAGE_SIZE, &
                             di => int32, dl => int64, &
                             ds => real32, dd => real64, dq => real128

  IMPLICIT NONE

  PRIVATE

  ! Precision constants
  PUBLIC :: di, dl           ! INTEGER types
  PUBLIC :: ds, dd, dq       ! REAL    types
  PUBLIC :: dc, dz           ! COMPLEX types

  ! Sizes in bytes
  PUBLIC :: sz_i, sz_l
  PUBLIC :: sz_s, sz_d, sz_q
  PUBLIC :: sz_c, sz_z

  INTRINSIC :: STORAGE_SIZE

  ! Precision constants for COMPLEX types
  INTEGER, PARAMETER :: dc = KIND( (0.0_ds, 1.0_ds) )
  INTEGER, PARAMETER :: dz = KIND( (0.0_dd, 1.0_dd) )

  ! Dummy variables for STORAGE_SIZE()
  INTEGER(KIND=di) :: i
  INTEGER(KIND=dl) :: l
  REAL(KIND=ds)    :: s
  REAL(KIND=dd)    :: d
  REAL(KIND=dq)    :: q
  COMPLEX(KIND=dc) :: c
  COMPLEX(KIND=dz) :: z

  ! Sizes in bytes
  INTEGER(KIND=dl), PARAMETER :: sz_i = STORAGE_SIZE(i) / CHARACTER_STORAGE_SIZE
  INTEGER(KIND=dl), PARAMETER :: sz_l = STORAGE_SIZE(l) / CHARACTER_STORAGE_SIZE
  INTEGER(KIND=dl), PARAMETER :: sz_s = STORAGE_SIZE(s) / CHARACTER_STORAGE_SIZE
  INTEGER(KIND=dl), PARAMETER :: sz_d = STORAGE_SIZE(d) / CHARACTER_STORAGE_SIZE
  INTEGER(KIND=dl), PARAMETER :: sz_q = STORAGE_SIZE(q) / CHARACTER_STORAGE_SIZE
  INTEGER(KIND=dl), PARAMETER :: sz_c = STORAGE_SIZE(c) / CHARACTER_STORAGE_SIZE
  INTEGER(KIND=dl), PARAMETER :: sz_z = STORAGE_SIZE(z) / CHARACTER_STORAGE_SIZE

END MODULE mod_prec
