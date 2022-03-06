!===============================================================================
! Module for generic views on arrays and scalars
! Last edited: Oct 31, 2021 (AM)
!
! TODO:
! - Use PCT
!===============================================================================
MODULE moa_view_types
    USE iso_c_binding
    USE moa_basic_view_types

    IMPLICIT NONE

    PRIVATE
    PUBLIC :: moa_view_type, operator(//), size, shape, rank

    !
    ! NOTE: Intel Fortran 2021.04 does not support overloading //
    ! Then a user-defined operator can be used instead:
    !
    ! INTERFACE operator(.cat.)
    !     MODULE PROCEDURE catenate_array_array, catenate_array_view, catenate_view_array, catenate_view_view
    ! END INTERFACE



    !
    ! Type for defining "views" on arrays - to allow for a#b#c: POINTERs to other views
    !
    TYPE :: moa_view_type
        TYPE(moa_basic_view)         :: left_array
        TYPE(moa_basic_view)         :: right_array
        TYPE(moa_view_type), POINTER :: left_view   => null()
        TYPE(moa_view_type), POINTER :: right_view  => null()
    CONTAINS
         generic           :: elem        => elem_single, elem_ndim
         PROCEDURE         :: elem_single => get_elem_single
         PROCEDURE         :: elem_ndim   => get_elem_ndim
    END TYPE moa_view_type

    INTERFACE operator(//)
        MODULE PROCEDURE catenate_array_array, catenate_array_view, catenate_view_array, catenate_view_view
    END INTERFACE

    INTERFACE size
        MODULE PROCEDURE size_view
    END INTERFACE

    INTERFACE shape
        MODULE PROCEDURE shape_view
    END INTERFACE

    INTERFACE rank
        MODULE PROCEDURE rank_view
    END INTERFACE
CONTAINS
FUNCTION catenate_array_array( array1, array2 ) result(new_view)
    INTEGER, DIMENSION(..), TARGET, INTENT(IN) :: array1
    INTEGER, DIMENSION(..), TARGET, INTENT(IN) :: array2
    CLASS(moa_view_type),  ALLOCATABLE          :: new_view

    CALL check_shapes( SHAPE(array1), SHAPE(array2) )

    ALLOCATE( new_view )

    CALL new_view%left_array%point_to(  array1 )
    CALL new_view%right_array%point_to( array2 )
END FUNCTION catenate_array_array

FUNCTION catenate_view_array( view1, array2 ) result(new_view)
    CLASS(moa_view_type), TARGET, INTENT(IN)   :: view1
    INTEGER, DIMENSION(..), TARGET, INTENT(IN) :: array2
    CLASS(moa_view_type),  ALLOCATABLE          :: new_view

    CALL check_shapes( SHAPE(view1), SHAPE(array2) )

    ALLOCATE( new_view )

    new_view%left_view   => view1
    CALL new_view%right_array%point_to( array2 )
END FUNCTION catenate_view_array

FUNCTION catenate_array_view( array1, view2 ) result(new_view)
    INTEGER, DIMENSION(..), TARGET, INTENT(IN) :: array1
    CLASS(moa_view_type), TARGET, INTENT(IN)   :: view2
    CLASS(moa_view_type),  ALLOCATABLE          :: new_view

    CALL check_shapes( SHAPE(array1), SHAPE(view2) )

    ALLOCATE( new_view )

    CALL new_view%left_array%point_to( array1 )
    new_view%right_view => view2
END FUNCTION catenate_array_view

FUNCTION catenate_view_view( view1, view2 ) result(new_view)
    CLASS(moa_view_type), TARGET, INTENT(IN) :: view1
    CLASS(moa_view_type), TARGET, INTENT(IN) :: view2
    CLASS(moa_view_type),  ALLOCATABLE        :: new_view

    CALL check_shapes( SHAPE(view1), SHAPE(view2) )

    ALLOCATE( new_view )

    new_view%left_view  => view1
    new_view%right_view => view2
END FUNCTION catenate_view_view

recursive SUBROUTINE get_pointer(view, rnk, idx, elem, found)
    CLASS(moa_view_type), INTENT(INOUT)          :: view
    INTEGER, INTENT(IN)                          :: rnk
    INTEGER, DIMENSION(:), INTENT(INOUT), TARGET :: idx
    INTEGER, POINTER                             :: elem
    logical, INTENT(OUT)                         :: found

    INTEGER                                      :: sz
    INTEGER, DIMENSION(rnk)                      :: shp
    INTEGER, DIMENSION(:), POINTER               :: pidx

    found = .false.

!!    WRITE(*,*) 'Index, rank: ',idx, ' - ', rnk

    IF ( ASSOCIATED(view%left_array%array) ) then
        IF ( SIZE(view%left_array%shp) > 0 ) then
            shp = view%left_array%shp
            sz  = shp(1)
        ELSE
            sz  = 1
        ENDIF
!!        WRITE(*,*) 'left array', sz, '-- ', idx
!!        WRITE(*,*) '     array', view%left_array%array
        IF ( idx(1) <= sz ) then
            found =  .true.
            pidx  => get_corrected_index(idx, view%left_array)
            elem  => view%left_array%elem(pidx)
        ELSE
!!            WRITE(*,*) 'right array?', ASSOCIATED(view%right_array%array)
            idx(1) = idx(1) - sz
            IF ( ASSOCIATED(view%right_array%array) ) then
!!                WRITE(*,*) 'right array', sz, '-- ', idx
!!                WRITE(*,*) '      array', view%right_array%array
                IF ( SIZE(view%right_array%shp) > 0 ) then
                    shp = view%right_array%shp
                    sz  = shp(1)
                ELSE
                    sz  = 1
                ENDIF
                IF ( idx(1) <= sz ) then
                    found =  .true.
                    pidx  => get_corrected_index(idx, view%right_array)
                    elem  => view%right_array%elem(pidx)
                ELSE
                    idx(1) = idx(1) - sz
                    return
                ENDIF
            ELSE
                CALL get_pointer( view%right_view, rnk, idx, elem, found )
            ENDIF
        ENDIF
    ELSE
!!        WRITE(*,*) 'left view'
        CALL get_pointer(view%left_view, rnk, idx, elem, found )

        IF ( .not. found ) then
            IF ( ASSOCIATED(view%right_array%array) ) then
                IF ( SIZE(view%right_array%shp) > 0 ) then
                    shp = view%right_array%shp
                    sz  = shp(1)
                ELSE
                    sz  = 1
                ENDIF
!!                WRITE(*,*) 'right array', shp, ' - ', idx
                IF ( idx(1) <= sz ) then
                    found =  .true.
                    pidx  => get_corrected_index(idx, view%right_array)
                    elem  => view%right_array%elem(pidx)
                ELSE
                    !WRITE(*,*) 'Element not found - index: ', idx
                    error stop
                ENDIF
            ELSE
!!                WRITE(*,*) 'right view'
                CALL get_pointer( view%right_view, rnk, idx, elem, found )
            ENDIF
        ENDIF
    ENDIF
END SUBROUTINE get_pointer

FUNCTION get_corrected_index( idx, array ) result(pidx)
    INTEGER, DIMENSION(:), INTENT(IN), TARGET :: idx
    CLASS(moa_basic_view), INTENT(IN)         :: array
    INTEGER, DIMENSION(:), POINTER            :: pidx

!!    WRITE(*,*) 'Size/rank: ', SIZE(idx), rank(array)
!!    WRITE(*,*) 'Array: ', array%array
    IF ( SIZE(idx) /= SIZE(array%shp) ) then
        IF ( SIZE(idx) == rank(array) + 1 .and. idx(1) == 1 ) then
            pidx => idx(2:)
        ELSE
            WRITE(*,*) 'Invalid index'
            error stop
        ENDIF
    ELSE
        pidx => idx
    ENDIF

!!    WRITE(*,*) '>> ', idx, ' - ', pidx, ' - ', rank(array)
END FUNCTION get_corrected_index

FUNCTION get_elem_single(view, i) result(elem)
    CLASS(moa_view_type), INTENT(INOUT) :: view
    INTEGER, INTENT(IN)                 :: i
    INTEGER, POINTER                    :: elem

    INTEGER, DIMENSION(1)               :: inew

    inew =  i
    elem => get_elem_ndim( view, inew )
END FUNCTION

FUNCTION get_elem_ndim(view, i) result(elem)
    CLASS(moa_view_type), INTENT(INOUT) :: view
    INTEGER, DIMENSION(:), INTENT(IN)   :: i
    INTEGER, POINTER                    :: elem

    INTEGER                             :: rnk
    INTEGER, DIMENSION(SIZE(i))         :: inew
    logical                             :: found

    rnk  = rank(view)
    inew = i

    CALL get_pointer( view, rnk, inew, elem, found )
END FUNCTION

INTEGER FUNCTION size_view( view )
    CLASS(moa_view_type), INTENT(IN) :: view

    size_view = product(SHAPE(view))
END FUNCTION size_view

recursive FUNCTION shape_view( view )
    CLASS(moa_view_type), INTENT(IN)   :: view

    INTEGER, DIMENSION(:),  ALLOCATABLE :: shape_view
    INTEGER, DIMENSION(:),  ALLOCATABLE :: shp

    ALLOCATE( shape_view(0) )

    IF ( ASSOCIATED(view%left_array%array) ) then
        IF ( SIZE(view%right_array%shp) > 0 ) then
            shape_view = view%left_array%shp
        ELSE
            shape_view = [1]
        ENDIF
    ELSEIF ( ASSOCIATED(view%left_view) ) then
        shape_view = SHAPE(view%left_view)
    ENDIF

    IF ( ASSOCIATED(view%right_array%array) ) then
        IF ( SIZE(view%right_array%shp) > 0 ) then
            shp            = SHAPE(view%right_array)
            shape_view(1)  = shape_view(1) + view%right_array%shp(1)
        ELSE
            shape_view = [shape_view(1) + 1]
        ENDIF
    ELSEIF ( ASSOCIATED(view%right_view) ) then
        shp           = SHAPE(view%right_view)
        shape_view(1) = shape_view(1) + shp(1)
    ENDIF
END FUNCTION shape_view

recursive INTEGER FUNCTION rank_view( view )
    TYPE(moa_view_type), INTENT(IN) :: view

    INTEGER                         :: rank_left, rank_right

    IF ( ASSOCIATED(view%left_array%array) ) then
        rank_left = SIZE(view%left_array%shp)
    ELSE
        rank_left = rank(view%left_view)
    ENDIF

    IF ( ASSOCIATED(view%right_array%array) ) then
        rank_right = SIZE(view%right_array%shp)
    ELSE
        rank_right = rank(view%right_view)
    ENDIF

    rank_view = max( 1, rank_left, rank_right )  ! Hm, not actually enough

END FUNCTION rank_view

SUBROUTINE check_shapes( shp1, shp2 )
    INTEGER, DIMENSION(:), INTENT(IN) :: shp1
    INTEGER, DIMENSION(:), INTENT(IN) :: shp2

    INTEGER                           :: i, offset1, offset2, first, last

    IF ( abs( SIZE(shp1) - SIZE(shp2) ) > 1 ) then
        WRITE(*,*) 'Incorrect combination of arrays - ranks differ more than 1'
        error stop
    ENDIF

    offset1 = 0
    offset2 = 0
    first   = 2
    last    = max( SIZE(shp1), SIZE(shp2) )

    IF ( SIZE(shp1) > SIZE(shp2) ) then
        offset2 = -1
    ENDIF

    IF ( SIZE(shp1) < SIZE(shp2) ) then
        offset1 = -1
    ENDIF

    do i = first,last
        IF ( shp1(i+offset1) /= shp2(i+offset2) ) then
            WRITE(*,*) 'Incorrect combination of arrays - shapes differ after first DIMENSION'
            error stop
        ENDIF
    enddo
END SUBROUTINE check_shapes

END MODULE moa_view_types
