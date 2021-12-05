!===============================================================================
! Program to test catenation on arrays of arbitrary rank
! Last edited: Dec 5, 2021 (AM)
!
! TODO:
!     - Check error handling - errors will interrupt the PROGRAM
!     - Incorporate the stdlib_error module
!===============================================================================
PROGRAM test_moa_cat
    USE moa_view_types

    IMPLICIT NONE

    TYPE(moa_view_type)     :: x, y, z
    INTEGER, DIMENSION(10)  :: a, b, c
    INTEGER, DIMENSION(2,5) :: aa, bb
    INTEGER                 :: i, j

    !     1   2   3   4    5    6    7    8    9   10
    a = [ 2,  3,  5,  7,  11,  13,  17,  19,  23,  29]
    b = [-2, -3, -5, -7, -11, -13, -17, -19, -23, -29]
    c = [31, 37, 41, 43,  47,  53,  59,  61,  67,  71]

    x = a // b
    y = x // c

    WRITE(*,*) 'Catenation with one-dimensional arrays'

    WRITE(*,*) '    Shape x:', shape(x)
    WRITE(*,*) '    Rank x: ', rank(x)
    WRITE(*,*) '    Shape x%left_array: ', x%left_array%shp
    WRITE(*,*) '    Shape x%right_array:', x%right_array%shp
    WRITE(*,*) '    Elements x:'
    WRITE(*,*) '        1 --> ', x%elem(1)
    WRITE(*,*) '       11 --> ', x%elem(11)

    WRITE(*,*) '    Shape y (= a//b//c): ', shape(y)
    WRITE(*,*) '    Rank y:              ', rank(y)
    WRITE(*,*) 'Values of y(i), i = 1, ..., 30'
    do i = 1,30
        WRITE(*,*) i, y%elem(i)
    enddo

    WRITE(*,*) 'Catenation with scalars:'
    i = 12
    j = 34
    z = i // j
    WRITE(*,*) '    Shape: ', shape(z)
    WRITE(*,*) '    Rank:  ', rank(z)
    WRITE(*,*) '    First scalar:  ', z%elem(1)
    WRITE(*,*) '    Second scalar: ', z%elem(2)

    z%elem(1) = 3
    WRITE(*,*) '    New value of z(1): ', z%elem(1)

    WRITE(*,*) 'Catenation with two-dimensional arrays:'

    ! This is not a good idea: it stores temporary arrays!
    ! z = reshape(a,[2,5]) // reshape(a,[2,5])

    aa = reshape(a,[2,5])
    bb = reshape(b,[2,5])

    z = aa // bb

    WRITE(*,*) '    Rank z:  ', rank(z)
    WRITE(*,*) '    Shape z: ', shape(z)

    WRITE(*,*) '    z(2,2):  ', z%elem([2,2])
END PROGRAM test_moa_cat
