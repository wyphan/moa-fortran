! moa_cat_test.f90 --
!     Straightforward test program for the catenation operation
!
program test_cat
    use moa_operations
    implicit none

    type(view_type)        :: x, y
    integer, dimension(10) :: a, b, c
    integer                :: i, j
    !     1   2   3   4    5    6    7    8    9   10
    a = [ 2,  3,  5,  7,  11,  13,  17,  19,  23,  29]
    b = [-2, -3, -5, -7, -11, -13, -17, -19, -23, -29]
    c = [31, 37, 41, 43,  47,  53,  59,  61,  67,  71]

    x = a // b
    y = x // c

    !j = y%elem(21)

    do i = 1,30
        write(*,*) i, y%elem(i)
    enddo
end program test_cat
