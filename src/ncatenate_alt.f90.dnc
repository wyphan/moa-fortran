!===============================================================================
! Program to do repeated catenation
! Last edited: Jan 5, 2022 (AM)
!===============================================================================
PROGRAM ncatenate
    USE moa_view_types

    IMPLICIT NONE

    TYPE array_of_arrays
        INTEGER, DIMENSION(:), ALLOCATABLE :: data
    END TYPE array_of_arrays

    INTEGER, DIMENSION(:), POINTER :: pdata

    TYPE(moa_view_type)            :: w, x, y, z
    INTEGER                        :: i, j, value
    INTEGER, DIMENSION(1)          :: shp

    !
    ! Test 1: catenate two arrays
    !
    WRITE( *, '(a)') 'Test 1'
    y = [1,2] // [3,4]

    shp = SHAPE(y);  WRITE(*, '(a,i0)') 'Shape of y = [1,2]//[3,4]: ', shp
    WRITE( *, '(a)') 'Elements:'
    DO i = 1,shp(1)
        WRITE(*,*) i, y%elem(i)
    ENDDO

    !
    ! Test 2: catenate a view with an array
    !
    WRITE( *, '(/,a)') 'Test 2'
    z = y // [5,6]

    shp = SHAPE(z);  WRITE(*, '(a,i0)') 'Shape of y(view)//[5,6]: ', shp
    WRITE(*, '(a)') 'Elements:'
    DO i = 1,shp(1)
        WRITE(*, '(2i10)') i, z%elem(i)
    ENDDO

    !
    ! Test 3: catenate an array with a view
    !
    WRITE( *, '(/,a)') 'Test 3'
    z = [5,6] // y

    shp = SHAPE(z);  WRITE(*,*) 'Shape of [5.6]//y(view): ', shp
    WRITE(*, '(a)') 'Elements:'
    DO i = 1,shp(1)
        WRITE(*, '(2i10)') i, z%elem(i)
    ENDDO

    !
    ! Test 4: catenate two views
    !
    ! Note: catenating [5,6] and 7 does not work - missing specific routines!
    !
    WRITE( *, '(/,a)') 'Test 4'
    y = [101,102,103] // [201,202]
    z = [5,6] // [7]
    w = y // z

    shp = SHAPE(w);  WRITE( *,'(a,i0)') 'Shape of y(view)//z(view): ', shp
    WRITE( *, '(a)') 'Elements:'
    DO i = 1,shp(1)
        WRITE(*, '(2i10)') i, w%elem(i)
    ENDDO

    !
    ! Test 5: longer chains of catenation - prepend the array
    !
    WRITE( *, '(/,a)') 'Test 5'
    x = [1,2] // [3,4] ! Do we have a proper way to initialise x otherwise?

    shp = shape(x)
    WRITE(*, '(a,i10)') 'Start shape of x:', shp
    WRITE(*, '(a)')     'Catenating several arrays'

    DO i = 1,10
        ALLOCATE( pdata(100) )
        pdata = [(i*200 + j, j = 1,size(pdata))]

        x = pdata // x
        !x = x // pdata

        NULLIFY( pdata )

        shp = shape(x)
        WRITE(*, '(a,i5,i10)') '   Shape:', i, shp
    ENDDO

    WRITE(*, '(a)') '   First few elements:'

    DO i = 1,10
        value = x%elem(i); WRITE(*, '(a,i10,i10)') '   ', i, value
    ENDDO

    shp   = shape(x)
    write(*, '(a,i0)') 'Final shape of x: ', shp

    WRITE(*, '(a)') '   Elements 100*i+1:'
    DO i = 1,10
        value = x%elem(1+i*100)
        WRITE( *, '(a,i10,i10)' ) '   ', i, value
    ENDDO

    !
    ! Test 6: longer chains of catenation - append the array
    !
    WRITE( *, '(/,a)') 'Test 6'
    x = [1,2] // [3,4] ! Do we have a proper way to initialise x otherwise?

    shp = shape(x)
    WRITE(*, '(a,i10)') 'Start shape of x:', shp
    WRITE(*, '(a)')     'Catenating several arrays'

    DO i = 1,10
        ALLOCATE( pdata(100) )
        pdata = [(i*200 + j, j = 1,size(pdata))]

        !x = pdata // x
        x = x // pdata

        NULLIFY( pdata )

        shp = shape(x)
        WRITE(*, '(a,i5,i10)') '   Shape:', i, shp
    ENDDO

    WRITE(*, '(a)') '   First few elements:'

    DO i = 1,10
        value = x%elem(i); WRITE(*, '(a,i10,i10)') '   ', i, value
    ENDDO

    shp   = shape(x)
    write(*, '(a,i0)') 'Final shape of x: ', shp

    WRITE(*, '(a)') '   Elements 100*i+1:'
    DO i = 1,10
        value = x%elem(1+i*100)
        WRITE( *, '(a,i10,i10)' ) '   ', i, value
    ENDDO

    WRITE(*,'(/,a)') 'All done'

END PROGRAM ncatenate
