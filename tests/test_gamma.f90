PROGRAM testgamma

  USE mod_prec
  USE moa, ONLY: moa_gamma
  use modheader, only: header
  IMPLICIT NONE

  INTEGER, PARAMETER :: maxdim = 18

  INTEGER(KIND=dl) :: r
  INTEGER :: i
  INTEGER :: a(0:maxdim-1), b(0:maxdim-1)

  call header()

  ! Test error messages
  PRINT *, 'Test error message for non-conformables'
  r = moa_gamma( [0,0], [2] )
  PRINT *, r
  PRINT *
  PRINT *, 'Test error message for out-of-bounds access'
  r = moa_gamma( [69], [42] )
  PRINT *, r
  PRINT *

  ! Test for scalar
!  PRINT *, 'Scalar 42'
!  r = moa_gamma( 41, 42 )
!  PRINT *, r
!  PRINT *

  ! Test for empty vector
!  PRINT *, 'Empty vector'
!  r = moa_gamma( [], [] )
!  PRINT *, r
!  PRINT *

  ! Test for 1-element vector
  PRINT *, '1-element vector (/ 41 /), (/ 42 /)'
  r = moa_gamma( [41], [42] )
  PRINT *, r
  PRINT *

  ! Test for 3-element vector
  PRINT *, '3-element vector (/ 0, 1, 2 /), (/ 2, 3, 4 /)'
  a(0:2) = [ 0, 1, 2 ]
  b(0:2) = [ 2, 3, 4 ]
  moa_reset = .TRUE.
  r = moa_gamma( a(0:2), b(0:2) )
  PRINT *, r
  PRINT *

  ! Test for arbitrary vector
  PRINT '(3(I0,A))', maxdim, '-element vector (/ 0 ... ', maxdim-1, ' /), (/ 2 ... ', maxdim+1, ' /)'
  DO i = 0, maxdim-1
     a(i) = i
     b(i) = i + 2
  END DO
  moa_reset = .TRUE.
  r = moa_gamma( a, b )
  PRINT *, r
  PRINT *

  STOP
END PROGRAM testgamma
