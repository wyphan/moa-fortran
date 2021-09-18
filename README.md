# Mathematics of Arrays library for modern Fortran

The following operations have been implemented:
- [x] scalar take vector
- [x] scalar drop vector

The following functions have been implemented:
- [x] gamma ( vector, vector )

## Contributing

- Please submit your contributions using a [pull request](https://github.com/wyphan/moa-fortran/pulls), instead of directly pushing commits to the main repo.
- Please write *one* *module* *file* per function/operator, with a module name prefixed with `mod_moa_`.
- Begin each file with a short description and a "last edited" comment, e.g.
```f90
!===============================================================================
! The family of "Gamma" offset functions
! Last edited: Sep 9, 2021 (WYP)
!===============================================================================
MODULE mod_moa_gamma
```
- Declare everything as `PRIVATE` and export `PUBLIC` generic interfaces. The exported interface name should begin with the `moa_` prefix:
```f90
  PRIVATE
  PUBLIC :: moa_gamma

  INTERFACE moa_gamma
    MODULE PROCEDURE gamma_vv_i, gamma_vv_dl ! gamma ( vector, vector )
  END INTERFACE moa_gamma
```
- *Always* use `IMPLICIT NONE` and declare *all* arguments with `INTENT(IN`/`OUT`/`INOUT)`.
- Please sprinkle helpful comments in the code.
- For error messages, please output them to standard error instead of standard output. Put the following near the beginning of the function/subroutine:
```f90
  USE ISO_FORTRAN_ENV, ONLY: u => error_unit
```
  and output the error message as follows:
```f90
  WRITE(u,'(A,I0,A,I0)') "Error[moa_gamma]: Out-of-bounds access ", &
                         avec(ia), " out of ", bvec(ib)
```
- Provide a standalone unit test under `tests` subdirectory.
- If you haven't already, please install [EditorConfig](https://editorconfig.org/https://editorconfig.org/) for your favorite text editor to standardize the style (esp. tabs vs spaces).

## License

This work is licensed under the 3-clause BSD license.
