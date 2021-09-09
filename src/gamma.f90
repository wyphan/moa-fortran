!===============================================================================
! The family of "Gamma" offset functions
! Last edited: Sep 9, 2021 (WYP)
!===============================================================================
MODULE mod_moa_gamma

  USE mod_prec
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: moa_gamma

  INTERFACE moa_gamma

    ! gamma ( vector, vector )
    MODULE PROCEDURE gamma_vv_i, gamma_vv_dl

  END INTERFACE moa_gamma

CONTAINS

!===============================================================================
! Helper function to check out of bounds access
!===============================================================================
  LOGICAL FUNCTION check_bounds( avec, bvec, tau )

    USE ISO_FORTRAN_ENV, ONLY: u => error_unit
    IMPLICIT NONE

    ! Input arguments
    INTEGER, INTENT(IN) :: avec(:), bvec(:), tau

    ! Internal variables
    INTEGER :: ia, ib
    LOGICAL :: lbound

    ! Initialize
    lbound = .FALSE.

    ! Move index to correct dimension
    ia = LBOUND(avec,1) + tau
    ib = LBOUND(bvec,1) + tau

    IF( avec(ia) < bvec(ib) ) THEN
      lbound = .TRUE.
    ELSE
      lbound = .FALSE.
      WRITE(u,'(A,I0,A,I0)') "Error[moa_gamma]: Out-of-bounds access ", &
                              avec(ia), " out of ", bvec(ib)
    END IF

    check_bounds = lbound
    RETURN
  END FUNCTION check_bounds

!===============================================================================
! Implementation of gamma ( vector, vector ) that returns the default integer.
! On out-of-bounds error, returns the negative of the failing 1-based dimension.
!===============================================================================
  INTEGER FUNCTION gamma_vv_i( avec, bvec ) RESULT( val )
    IMPLICIT NONE

    ! Input arguments
    INTEGER, INTENT(IN) :: avec(:), bvec(:)

    ! Internal variables
    INTEGER :: na, nb, ia, ib, tau
    LOGICAL :: lbound

    ! Check shapes
    na = SIZE(avec,1)
    nb = SIZE(bvec,1)

    IF( na == 0 .AND. nb == 0 ) THEN

      ! Quick exit for empty vectors
      val = 0

    ELSE IF( na /= nb ) THEN

      ! Non conformable shapes
      val = -1
      WRITE(u,'(A)') "Error[moa_gamma]: Non-conformable shapes"

    ELSE IF( na == 1 ) THEN

      ! One-element vector

      ! Check bounds
      lbound = check_bounds( avec, bvec, 0 )
      IF( lbound ) THEN

        ! Use identity (3.30) in MoA dissertation
        val = avec(LBOUND(avec,1))

      ELSE

        ! Out of bounds
        val = -1

      END IF ! lbound

    ELSE

      ! Initialize result
      val = avec(ia)

      ! Main logic construct
      DO tau = 1, na-1

        ! Check bounds
        lbound = check_bounds( avec, bvec, tau )
        IF( lbound ) THEN

          ! Move index to correct dimension
          ia = LBOUND(avec,1) + tau
          ib = LBOUND(bvec,1) + tau

          ! Multiply and accumulate
          val = val*bvec(ib) + avec(ia)

        ELSE

          ! Out of bounds
          val = -(tau+1)
          EXIT

        END IF ! lbound

      END DO ! tau

    END IF ! na >= 1

    RETURN
  END FUNCTION gamma_vv_i

!===============================================================================
! Implementation of gamma ( vector, vector ) that returns a 64-bit integer.
! On out-of-bounds error, returns the negative of the failing 1-based dimension.
!===============================================================================
  INTEGER(KIND=dl) FUNCTION gamma_vv_dl( avec, bvec ) RESULT( val )
    IMPLICIT NONE

    ! Input arguments
    INTEGER, INTENT(IN) :: avec(:), bvec(:)

    ! Internal variables
    INTEGER :: na, nb, ia, ib, tau
    LOGICAL :: lbound

    ! Check shapes
    na = SIZE(avec,1)
    nb = SIZE(bvec,1)

    IF( na == 0 .AND. nb == 0 ) THEN

      ! Quick exit for empty vectors
      val = 0

    ELSE IF( na /= nb ) THEN

      ! Non conformable shapes
      val = -1
      WRITE(u,'(A)') "Error[moa_gamma]: Non-conformable shapes"

    ELSE IF( na == 1 ) THEN

      ! One-element vector

      ! Check bounds
      lbound = check_bounds( avec, bvec, 0 )
      IF( lbound ) THEN

        ! Use identity (3.30) in MoA dissertation
        val = avec(LBOUND(avec,1))

      ELSE

        ! Out of bounds
        val = -1

      END IF ! lbound

    ELSE

      ! Initialize result
      val = avec(ia)

      ! Main logic construct
      DO tau = 1, na-1

        ! Check bounds
        lbound = check_bounds( avec, bvec, tau )
        IF( lbound ) THEN

          ! Move index to correct dimension
          ia = LBOUND(avec,1) + tau
          ib = LBOUND(bvec,1) + tau

          ! Multiply and accumulate
          val = val*INT(bvec(ib),KIND=dl) + INT(avec(ia),KIND=dl)

        ELSE

          ! Out of bounds
          val = -(tau+1)
          EXIT

        END IF ! lbound

      END DO ! tau

    END IF ! na >= 1

    RETURN
  END FUNCTION gamma_vv_dl

END MODULE
