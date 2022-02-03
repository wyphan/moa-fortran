!===============================================================================
! Module for generic views on arrays and scalars
! Last edited: Jan 5, 2022 (AM)
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
        TYPE(moa_basic_view)             :: left_array
        TYPE(moa_basic_view)             :: right_array
        TYPE(moa_view_type), ALLOCATABLE :: left_view
        TYPE(moa_view_type), ALLOCATABLE :: right_view
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

    new_view%left_view = view1
    CALL new_view%right_array%point_to( array2 )
END FUNCTION catenate_view_array

FUNCTION catenate_array_view( array1, view2 ) result(new_view)
    INTEGER, DIMENSION(..), TARGET, INTENT(IN) :: array1
    CLASS(moa_view_type), TARGET, INTENT(IN)   :: view2
    CLASS(moa_view_type),  ALLOCATABLE          :: new_view

    CALL check_shapes( SHAPE(array1), SHAPE(view2) )

    ALLOCATE( new_view )

    CALL new_view%left_array%point_to( array1 )
    new_view%right_view = view2

    !write(*,*) 'Left:  ', associated(new_view%left_array%array),  ' -  ', allocated(new_view%left_view)
    !write(*,*) 'Right: ', associated(new_view%right_array%array), ' -  ', allocated(new_view%right_view)
END FUNCTION catenate_array_view

FUNCTION catenate_view_view( view1, view2 ) result(new_view)
    CLASS(moa_view_type), TARGET, INTENT(IN) :: view1
    CLASS(moa_view_type), TARGET, INTENT(IN) :: view2
    CLASS(moa_view_type),  ALLOCATABLE        :: new_view

    CALL check_shapes( SHAPE(view1), SHAPE(view2) )

    ALLOCATE( new_view )

    new_view%left_view  = view1
    new_view%right_view = view2
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

    !!WRITE(*,*) 'Index, rank: ',idx, ' - ', rnk

    IF ( ASSOCIATED(view%left_array%array) ) THEN
        IF ( SIZE(view%left_array%shp) > 0 ) THEN
            shp = view%left_array%shp
            sz  = shp(1)
        ELSE
            sz  = 1
        ENDIF
        !!WRITE(*,*) 'left array', sz, '-- ', idx
        !!WRITE(*,*) '     array', view%left_array%array
        IF ( idx(1) <= sz ) THEN
            found =  .true.
            pidx  => get_corrected_index(idx, view%left_array)
            elem  => view%left_array%elem(pidx)
        ELSE
            !!WRITE(*,*) 'right array?', ASSOCIATED(view%right_array%array)
            idx(1) = idx(1) - sz
            IF ( ASSOCIATED(view%right_array%array) ) THEN
                !!WRITE(*,*) 'right array', sz, '-- ', idx
                !!WRITE(*,*) '      array', view%right_array%array
                IF ( SIZE(view%right_array%shp) > 0 ) THEN
                    shp = view%right_array%shp
                    sz  = shp(1)
                ELSE
                    sz  = 1
                ENDIF
                IF ( idx(1) <= sz ) THEN
                    found =  .true.
                    pidx  => get_corrected_index(idx, view%right_array)
                    elem  => view%right_array%elem(pidx)
                ELSE
                    found  = .false.
                    idx(1) = idx(1) - sz
                    return
                ENDIF
            ELSE
                CALL get_pointer( view%right_view, rnk, idx, elem, found )
            ENDIF
        ENDIF
    ELSE
        !!WRITE(*,*) 'left view'
        CALL get_pointer(view%left_view, rnk, idx, elem, found )

        IF ( .not. found ) THEN
            IF ( ASSOCIATED(view%right_array%array) ) THEN
                IF ( SIZE(view%right_array%shp) > 0 ) THEN
                    shp = view%right_array%shp
                    sz  = shp(1)
                ELSE
                    sz  = 1
                ENDIF
                !!WRITE(*,*) 'right array', shp, ' - ', idx
                IF ( idx(1) <= sz ) THEN
                    found =  .true.
                    pidx  => get_corrected_index(idx, view%right_array)
                    elem  => view%right_array%elem(pidx)
                ELSE
                    found  = .false.
                    idx(1) = idx(1) - sz
                    return
                    !!WRITE(*,*) 'Element not found - index: ', idx
                    !!error stop
                ENDIF
            ELSE
                !!WRITE(*,*) 'right view'
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
    IF ( SIZE(idx) /= SIZE(array%shp) ) THEN
        IF ( SIZE(idx) == rank(array) + 1 .and. idx(1) == 1 ) THEN
            pidx => idx(2:)
        ELSE
            !!WRITE(*,*) 'Invalid index'
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

    IF ( .NOT. found ) THEN
        WRITE(*,*) 'Element not found - index: ', i
        ERROR STOP
    ENDIF
END FUNCTION

INTEGER FUNCTION size_view( view )
    CLASS(moa_view_type), INTENT(IN) :: view

    size_view = product(SHAPE(view))
END FUNCTION size_view

recursive FUNCTION shape_view( view )
    CLASS(moa_view_type), INTENT(IN)   :: view

    INTEGER, DIMENSION(:),  ALLOCATABLE :: shape_view
    INTEGER, DIMENSION(:),  ALLOCATABLE :: shp
    INTEGER                              :: rnk

    !write(*,*) 'View Left:  ', associated(view%left_array%array),  ' -  ', allocated(view%left_view)
    !write(*,*) 'View Right: ', associated(view%right_array%array), ' -  ', allocated(view%right_view)

    rnk = RANK(view)
    ALLOCATE( shape_view(rnk) )
    shape_view = 0

    !write(*,*) '>> Shape'
    IF ( ASSOCIATED(view%left_array%array) ) THEN
        !write(*,*) '>> Shape - left associated'
        IF ( SIZE(view%left_array%shp) > 0 ) THEN
            shape_view = view%left_array%shp
        ELSE
            shape_view = [1]
        ENDIF
    ELSEIF ( ALLOCATED(view%left_view) ) THEN
        !write(*,*) '>> Shape - left allocated'
        shape_view = SHAPE(view%left_view)
    ENDIF

    IF ( ASSOCIATED(view%right_array%array) ) THEN
        !write(*,*) '>> Shape - right associated'
        IF ( SIZE(view%right_array%shp) > 0 ) THEN
            shp            = SHAPE(view%right_array)
            shape_view(1)  = shape_view(1) + view%right_array%shp(1)
        ELSE
            shape_view = [shape_view(1) + 1]
        ENDIF
    ELSEIF ( ALLOCATED(view%right_view) ) THEN
        shp           = SHAPE(view%right_view)
        shape_view(1) = shape_view(1) + shp(1)
        !write(*,*) '>> Shape - right allocated - ', shp
    ENDIF

    !write(*,*) '>> Shape -', shape_view
END FUNCTION shape_view

recursive INTEGER FUNCTION rank_view( view )
    TYPE(moa_view_type), INTENT(IN) :: view

    INTEGER                         :: rank_left, rank_right

    rank_left  = 0
    rank_right = 0
    !write(*,*) '>> Rank'
    IF ( ASSOCIATED(view%left_array%array) ) THEN
        !write(*,*) '>> Rank - left associated'
        rank_left = SIZE(view%left_array%shp)
    ELSE
        IF ( ALLOCATED(view%left_view) ) THEN
            !write(*,*) '>> Rank - left allocated'
            rank_left = RANK(view%left_view)
        ENDIF
    ENDIF

    IF ( ASSOCIATED(view%right_array%array) ) THEN
        !write(*,*) '>> Rank - right associated'
        rank_right = SIZE(view%right_array%shp)
    ELSE
        IF ( ALLOCATED(view%right_view) ) THEN
            !write(*,*) '>> Rank - right allocated'
            rank_right = RANK(view%right_view)
        ENDIF
    ENDIF

    rank_view = max( 1, rank_left, rank_right )  ! Hm, not actually enough
    !write(*,*) '>> Rank - ', rank_view

END FUNCTION rank_view

SUBROUTINE check_shapes( shp1, shp2 )
    INTEGER, DIMENSION(:), INTENT(IN) :: shp1
    INTEGER, DIMENSION(:), INTENT(IN) :: shp2

    INTEGER                           :: i, offset1, offset2, first, last

    IF ( abs( SIZE(shp1) - SIZE(shp2) ) > 1 ) THEN
        !!WRITE(*,*) 'Incorrect combination of arrays - ranks differ more than 1'
        error stop
    ENDIF

    offset1 = 0
    offset2 = 0
    first   = 2
    last    = max( SIZE(shp1), SIZE(shp2) )

    IF ( SIZE(shp1) > SIZE(shp2) ) THEN
        offset2 = -1
    ENDIF

    IF ( SIZE(shp1) < SIZE(shp2) ) THEN
        offset1 = -1
    ENDIF

    do i = first,last
        IF ( shp1(i+offset1) /= shp2(i+offset2) ) THEN
            !!WRITE(*,*) 'Incorrect combination of arrays - shapes differ after first DIMENSION'
            error stop
        ENDIF
    enddo
END SUBROUTINE check_shapes

END MODULE moa_view_types
