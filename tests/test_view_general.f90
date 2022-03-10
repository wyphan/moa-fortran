!===============================================================================
! Program to test generic views on arrays and scalars
! Last edited: Nov 2, 2021 (AM)
!===============================================================================

PROGRAM test_basic_moa_view
    USE stdlib_error, only: check
    USE moa_basic_view_types

    TYPE(moa_basic_view) :: myview

    INTEGER :: array1(10), array2(10,5), array3(3,3,3), array4(4,4,4,4)

    CALL myview%point_to( array1 )

    WRITE(*,*) 'One-dimensional: ', myview%shp
    CALL check( size(myview%shp) == 1, "Size incorrect - array1" )
    CALL check( all( myview%shp == shape(array1) ), "Shape incorrect - array1" )
    CALL check( all( myview%array == reshape(array1,[size(array1)]) ), "Content incorrect - array1" )

    CALL myview%point_to( array2 )

    WRITE(*,*) 'Two-dimensional: ', myview%shp
    CALL check( size(myview%shp) == 2, "Size incorrect - array2" )
    CALL check( all( myview%shp == shape(array2) ), "Shape incorrect - array2" )
    CALL check( all( myview%array == reshape(array2,[size(array2)]) ), "Content incorrect - array2" )

    CALL myview%point_to( array3 )

    WRITE(*,*) 'Three-dimensional: ', myview%shp
    CALL check( size(myview%shp) == 3, "Size incorrect - array3" )
    CALL check( all( myview%shp == shape(array3) ), "Shape incorrect - array3" )
    CALL check( all( myview%array == reshape(array3,[size(array3)]) ), "Content incorrect - array3" )

    CALL myview%point_to( array4 )

    WRITE(*,*) 'Four-dimensional: ', myview%shp
    CALL check( size(myview%shp) == 4, "Size incorrect - array4" )
    CALL check( all( myview%shp == shape(array4) ), "Shape incorrect - array4" )
    CALL check( all( myview%array == reshape(array4,[size(array4)]) ), "Content incorrect - array4" )

    m = 0
    do k = 1,3
        do j = 1,3
            do i = 1,3
                m = m + 1
                array3(i,j,k) = m
            enddo
        enddo
    enddo

    CALL myview%point_to( array3 )
    write(*,*) myview%elem([1,1,1])
    write(*,*) myview%elem([1,1,3])
    write(*,*) myview%elem([3,3,3])

END PROGRAM test_basic_moa_view
