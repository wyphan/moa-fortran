!===============================================================================
! Module for generic views on arrays and scalars
! Last edited: Jan 10, 2022 (AM)
!
! TODO:
! - Use PCT
!
! Note:
! This is a reimplementaion, relying on a flat data structure instead of
! a recursive structure
!===============================================================================
MODULE moa_view_types
    USE iso_c_binding
    USE moa_basic_view_types

    IMPLICIT NONE

    PRIVATE
    PUBLIC :: moa_view_type, operator(//), size, shape, rank

    !
    ! Type for defining "views" on arrays - to allow for a#b#c: POINTERs to other views
    !
    TYPE :: moa_view_type
        TYPE(moa_basic_view), ALLOCATABLE :: array(:)
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
    CLASS(moa_view_type),  ALLOCATABLE         :: new_view

    CALL check_shapes( SHAPE(array1), SHAPE(array2) )

    ALLOCATE( new_view )
    ALLOCATE( new_view%array(2) )

    CALL point_to_any_rank( new_view%array(1), array1 )
    CALL point_to_any_rank( new_view%array(2), array2 )
END FUNCTION catenate_array_array

FUNCTION catenate_view_array( view1, array2 ) result(new_view)
    CLASS(moa_view_type), TARGET, INTENT(IN)   :: view1
    INTEGER, DIMENSION(..), TARGET, INTENT(IN) :: array2
    CLASS(moa_view_type),  ALLOCATABLE         :: new_view

    INTEGER                                    :: new_size

    CALL check_shapes( SHAPE(view1), SHAPE(array2) )

    new_size = SIZE(view1%array) + 1
    ALLOCATE( new_view )
    ALLOCATE( new_view%array(new_size) )

    new_view%array(1:new_size-1) = view1%array
    CALL point_to_any_rank( new_view%array(new_size), array2 )

END FUNCTION catenate_view_array

FUNCTION catenate_array_view( array1, view2 ) result(new_view)
    INTEGER, DIMENSION(..), TARGET, INTENT(IN) :: array1
    CLASS(moa_view_type), TARGET, INTENT(IN)   :: view2
    CLASS(moa_view_type),  ALLOCATABLE         :: new_view

    INTEGER                                    :: new_size

    CALL check_shapes( SHAPE(array1), SHAPE(view2) )

    new_size = SIZE(view2%array) + 1
    ALLOCATE( new_view )
    ALLOCATE( new_view%array(new_size) )

    CALL point_to_any_rank( new_view%array(1), array1 )
    new_view%array(2:new_size) = view2%array

END FUNCTION catenate_array_view

FUNCTION catenate_view_view( view1, view2 ) result(new_view)
    CLASS(moa_view_type), TARGET, INTENT(IN) :: view1
    CLASS(moa_view_type), TARGET, INTENT(IN) :: view2
    CLASS(moa_view_type),  ALLOCATABLE       :: new_view

    INTEGER                                  :: new_size

    CALL check_shapes( SHAPE(view1), SHAPE(view2) )

    new_size = SIZE(view1%array) + SIZE(view2%array)
    ALLOCATE( new_view )
    ALLOCATE( new_view%array(new_size) )

    new_view%array(1:SIZE(view1%array))  = view1%array
    new_view%array(SIZE(view1%array)+1:) = view2%array

END FUNCTION catenate_view_view

recursive SUBROUTINE get_pointer(view, rnk, idx, elem, found)
    CLASS(moa_view_type), INTENT(INOUT)          :: view
    INTEGER, INTENT(IN)                          :: rnk
    INTEGER, DIMENSION(:), INTENT(INOUT), TARGET :: idx
    INTEGER, POINTER                             :: elem
    logical, INTENT(OUT)                         :: found

    INTEGER                                      :: i
    INTEGER                                      :: sz
    INTEGER, DIMENSION(rnk)                      :: shp
    INTEGER, DIMENSION(:), POINTER               :: pidx

    found = .FALSE.

    IF ( ALLOCATED(view%array) ) THEN
        IF ( SIZE(view%array) == 0 ) THEN
            RETURN
        ENDIF
    ELSE
        RETURN
    ENDIF

    DO i = 1,SIZE(view%array)
        IF ( SIZE(view%array(i)%shp) > 0 ) THEN
            sz = view%array(i)%shp(1)
        ELSE
            sz  = 1
        ENDIF

        !write(*,*) '>> Element: ', i, idx(1), '|', view%array(i)%shp, '|', view%array(i)%array, '|', sz, rnk

        IF ( idx(1) <= sz ) THEN
            found =  .TRUE.
            pidx  => get_corrected_index(idx, view%array(i))
            elem  => elem_of( view%array(i), pidx)
            EXIT
        ELSE
            idx(1) = idx(1) - sz
        ENDIF
    ENDDO

END SUBROUTINE get_pointer

FUNCTION get_corrected_index( idx, array ) result(pidx)
    INTEGER, DIMENSION(:), INTENT(IN), TARGET :: idx
    CLASS(moa_basic_view), INTENT(IN)         :: array

    INTEGER, DIMENSION(:), POINTER            :: pidx

    IF ( SIZE(idx) /= SIZE(array%shp) ) THEN
        IF ( SIZE(idx) == rank(array) + 1 .and. idx(1) == 1 ) THEN
            pidx => idx(2:)
        ELSE
            ERROR STOP
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

    rnk  = RANK(view)
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

FUNCTION shape_view( view )
    CLASS(moa_view_type), INTENT(IN)    :: view

    INTEGER, DIMENSION(:),  ALLOCATABLE :: shape_view
    INTEGER, DIMENSION(:),  ALLOCATABLE :: shp
    INTEGER                             :: i
    INTEGER                             :: rnk

    rnk = RANK(view)
    ALLOCATE( shape_view(rnk) )
    shape_view = 0

    IF ( .NOT. ALLOCATED(view%array) ) THEN
        RETURN
    ENDIF

    shape_view = view%array(1)%shp
    !write(*,*) '>> shape:', shape_view, '|', view%array(1)%shp, '|', view%array(1)%array
    DO i = 2,SIZE(view%array)
        shp           = view%array(i)%shp
        shape_view(1) = shape_view(1) + shp(1)
        !write(*,*) '>> shape:', i, shape_view
    ENDDO

END FUNCTION shape_view

recursive INTEGER FUNCTION rank_view( view )
    TYPE(moa_view_type), INTENT(IN) :: view

    IF ( ALLOCATED(view%array) ) THEN
        IF ( SIZE(view%array) > 0 ) THEN
            rank_view = SIZE(view%array(1)%shp)
        ELSE
            rank_view = 0
        ENDIF
    ELSE
        rank_view = 0
    ENDIF

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
