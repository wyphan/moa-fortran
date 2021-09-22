! moa_cat_module.f90 --
!     First version of a cat operation
!
module moa_operations
    implicit none

    !
    ! Type for defining "views" on arrays - to allow for a#b#c: pointers to other views
    !
    type view_type
        integer, dimension(:), pointer :: left_array  => null()
        integer, dimension(:), pointer :: right_array => null()
        type(view_type), pointer       :: left_view   => null()
        type(view_type), pointer       :: right_view  => null()
    contains
    !    procedure, nopass :: cataa => catenate_array_array
    !    procedure, nopass :: catav => catenate_array_view
    !    procedure, nopass :: catva => catenate_view_array
    !    procedure, nopass :: catvv => catenate_view_view
         procedure         :: elem  => get_elem
    !    generic   :: cat => cataa, catav, catva, catvv
    end type view_type

    interface operator(//)
        module procedure catenate_array_array, catenate_array_view, catenate_view_array, catenate_view_view
    end interface
contains
function catenate_array_array( array1, array2 ) result(new_view)
    integer, dimension(:), target, intent(in) :: array1
    integer, dimension(:), target, intent(in) :: array2
    class(view_type), allocatable             :: new_view

    allocate( new_view )

    new_view%left_array  => array1
    new_view%right_array => array2
end function catenate_array_array

function catenate_view_array( view1, array2 ) result(new_view)
    class(view_type), target, intent(in)      :: view1
    integer, dimension(:), target, intent(in) :: array2
    class(view_type), allocatable             :: new_view

    allocate( new_view )

    new_view%left_view   => view1
    new_view%right_array => array2
end function catenate_view_array

function catenate_array_view( array1, view2 ) result(new_view)
    integer, dimension(:), target, intent(in) :: array1
    class(view_type), target, intent(in)      :: view2
    class(view_type), allocatable             :: new_view

    allocate( new_view )

    new_view%left_array => array1
    new_view%right_view => view2
end function catenate_array_view

function catenate_view_view( view1, view2 ) result(new_view)
    class(view_type), target, intent(in) :: view1
    class(view_type), target, intent(in) :: view2
    class(view_type), allocatable        :: new_view

    allocate( new_view )

    new_view%left_view  => view1
    new_view%right_view => view2
end function catenate_view_view

recursive subroutine get_pointer(view, i, elem, found)
    class(view_type), intent(inout) :: view
    integer, intent(inout)          :: i
    integer, pointer                :: elem
    logical, intent(out)            :: found

    integer                         :: sz


    found = .false.

    if ( associated(view%left_array) ) then
        sz = size(view%left_array)
        if ( i <= sz ) then
            found =  .true.
            elem  => view%left_array(i)
        else
            i = i - sz
            if ( associated(view%right_array) ) then
                sz = size(view%right_array)
                if ( i <= sz ) then
                    found =  .true.
                    elem  => view%right_array(i)
                else
                    i = i - sz
                    return
                endif
            else
                call get_pointer( view%right_view, i, elem, found )
            endif
        endif
    else
        call get_pointer(view%left_view, i, elem, found )

        if ( .not. found ) then
            if ( associated(view%right_array) ) then
                sz = size(view%right_array)
                if ( i <= sz ) then
                    found =  .true.
                    elem  => view%right_array(i)
                else
                    error stop
                endif
            else
                call get_pointer( view%right_view, i, elem, found )
            endif
        endif
    endif
end subroutine get_pointer

function get_elem(view, i) result(elem)
    class(view_type), intent(inout) :: view
    integer, intent(in)             :: i
    integer, pointer                :: elem

    integer                         :: inew
    logical                         :: found


    inew = i
    call get_pointer( view, inew, elem, found )
end function
end module moa_operations
