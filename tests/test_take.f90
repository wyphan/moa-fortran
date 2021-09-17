PROGRAM testtake

  USE mod_prec
  USE moa, ONLY: moa_take
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
  PRINT *, 'Test error message for positive overtake'
  ptr_r => moa_take( 12, v )
  PRINT *

  PRINT *, 'Test error message for negative overtake'
  ptr_r => moa_take( -12, v )
  PRINT *

  ! Test positive take
  sigma = 2
  ptr_r => moa_take( sigma, v )
  PRINT *, sigma, ' take vvec = '
  PRINT *, ( ptr_r(i), ' ' , i = 1, sigma )
  PRINT *
  NULLIFY(ptr_r)

  ! Test negative take
  sigma = -2
  ptr_r => moa_take( sigma, v )
  PRINT *, sigma, ' take vvec = '
  PRINT *, ( ptr_r(i), ' ' , i = 1, ABS(sigma) )
  NULLIFY(ptr_r)

  STOP
END PROGRAM testtake
