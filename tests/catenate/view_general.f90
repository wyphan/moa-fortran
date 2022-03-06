!===============================================================================
! Module for generic views on arrays and scalars
! Last edited: Oct 31, 2021 (AM)
!===============================================================================
MODULE moa_basic_view_types
    USE ISO_C_BINDING, ONLY: C_LOC, C_F_POINTER

    IMPLICIT NONE

    TYPE moa_basic_view
        integer, allocatable :: shp(:)
        integer, pointer     :: array(:) => null()
    CONTAINS
        PROCEDURE :: point_to => point_to_any_rank
        PROCEDURE :: elem     => elem_of
    END TYPE moa_basic_view

CONTAINS

! point_to_any_RANK --
!     Let the view type point at an array of any RANK.
!     Note that we cannot do that in a completely generic way, though.
!
SUBROUTINE point_to_any_rank( this, array )
    CLASS(moa_basic_view), intent(inout) :: this
    INTEGER, DIMENSION(..), TARGET       :: array

    SELECT RANK( array )
        RANK (0)
            ! This requires a special treatment
            ALLOCATE( this%shp(0) )
            CALL C_F_POINTER( c_loc(array), this%array,  [1] )
        RANK (1)
            this%shp   =  SHAPE(array)
            this%array => array
        RANK (2)
            this%shp   =  SHAPE(array)
            this%array(1:size(array)) => array
        RANK (3)
            this%shp   =  SHAPE(array)
            this%array(1:size(array)) => array
        RANK (4)
            this%shp   =  SHAPE(array)
            this%array(1:size(array)) => array
        RANK (5)
            this%shp   =  SHAPE(array)
            this%array(1:size(array)) => array
        RANK (6)
            this%shp   =  SHAPE(array)
            this%array(1:size(array)) => array
        RANK (7)
            this%shp   =  SHAPE(array)
            this%array(1:size(array)) => array
        RANK (8)
            this%shp   =  SHAPE(array)
            this%array(1:size(array)) => array
        RANK (9)
            this%shp   =  SHAPE(array)
            this%array(1:size(array)) => array
        RANK (10)
            this%shp   =  SHAPE(array)
            this%array(1:size(array)) => array
        RANK (11)
            this%shp   =  SHAPE(array)
            this%array(1:size(array)) => array
        RANK (12)
            this%shp   =  SHAPE(array)
            this%array(1:size(array)) => array
        RANK (13)
            this%shp   =  SHAPE(array)
            this%array(1:size(array)) => array
        RANK (15)
            this%shp   =  SHAPE(array)
            this%array(1:size(array)) => array
        RANK DEFAULT
            WRITE(*,*) 'Rank not supported - ', RANK(array)
            ERROR STOP
    END SELECT
END SUBROUTINE point_to_any_RANK

FUNCTION elem_of( this, index ) RESULT(value)
    CLASS(moa_basic_view), INTENT(inout) :: this
    INTEGER, DIMENSION(:)                :: index
    INTEGER, POINTER                     :: value

    INTEGER                              :: idx

    IF ( SIZE(index) /= SIZE(this%shp) ) THEN
        WRITE(*,*) 'Index of wrong size'
        ERROR STOP
    ENDIF
    IF ( ANY( index < 1 ) .OR. ANY( index > this%shp ) ) THEN
        WRITE(*,*) 'Index out of bounds'
        ERROR STOP
    ENDIF

    idx   =  linear_index( index, this%shp )
    value => this%array(idx)

END FUNCTION elem_of

INTEGER FUNCTION linear_index( index, shp )
    INTEGER, DIMENSION(:), INTENT(in) :: index
    INTEGER, DIMENSION(:), INTENT(in) :: shp

    INTEGER                           :: i, idx, factor

    idx    = 1
    factor = 1
    do i = 1,size(index)
        idx    = idx + (index(i)-1) * factor
        factor = factor * shp(i)
    enddo
    linear_index = idx
END FUNCTION linear_index

END MODULE moa_basic_view_types
