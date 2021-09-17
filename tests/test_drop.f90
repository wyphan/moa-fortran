PROGRAM testdrop

  USE mod_prec
  USE moa, ONLY: moa_drop
  IMPLICIT NONE

  INTEGER, PARAMETER :: n = 10
  INTEGER :: i, sigma
  REAL, TARGET :: v(0:n-1)
  REAL, POINTER :: ptr_r(:)

  ! Initialize vector
  v = [( 0.1 * REAL(i), i = 0, n-1 )]
  PRINT *, 'vvec = '
  PRINT *, ( v(i), ' ' , i = 0, n-1 )

  ! Test error messages
  PRINT *, 'Test error message for positive overdrop'
  ptr_r => moa_drop( 12, v )
  PRINT *

  PRINT *, 'Test error message for negative overtake'
  ptr_r => moa_drop( -12, v )
  PRINT *

  ! Test positive drop
  sigma = 2
  ptr_r => moa_drop( sigma, v )
  PRINT *, sigma, ' drop vvec = '
  PRINT *, ( ptr_r(i), ' ' , i = 1, n-sigma )
  PRINT *
  NULLIFY(ptr_r)

  ! Test negative take
  sigma = -2
  ptr_r => moa_drop( sigma, v )
  PRINT *, sigma, ' drop vvec = '
  PRINT *, ( ptr_r(i), ' ' , i = 1, n-ABS(sigma) )
  NULLIFY(ptr_r)

  STOP
END PROGRAM testdrop
