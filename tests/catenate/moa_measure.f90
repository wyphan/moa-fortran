!===============================================================================
! Program to measure various aspects of the catenation operation
! Last edited: Jan 17, 2022 (AM)
!===============================================================================
PROGRAM moa_measure
    USE iso_fortran_env
    USE moa_view_types
    USE cmdparse

    IMPLICIT NONE

    !
    ! Define the commands
    !
    TYPE(cmdparser) :: parser
    CHARACTER(LEN=60) :: cmds(8) = &
        [ 'report-file (a)filename         ', &
          'use-plain-array                 ', &
          'use-view                        ', &
          'allocate-array (i)size          ', &
          'allocate-view (i)chunks (i)size ', &
          'sequential-get (i)step (i)number', &
          'random-get (i)number            ', &
          'number-repetitions (i)number    '  ]

    TYPE(moa_view_type)                :: x
    INTEGER, DIMENSION(:), ALLOCATABLE :: chunk
    INTEGER                            :: repetition
    INTEGER                            :: step
    INTEGER                            :: number_random
    INTEGER                            :: number_seq
    INTEGER                            :: number_chunks
    INTEGER                            :: array_size
    INTEGER                            :: chunk_size
    CHARACTER(LEN=40)                  :: filename
    CHARACTER(LEN=40)                  :: command
    INTEGER                            :: cmdidx
    INTEGER                            :: lurep = OUTPUT_UNIT
    INTEGER, DIMENSION(:), ALLOCATABLE :: array
    LOGICAL                            :: use_view = .true.

    INTEGER                            :: time_first, time_last, time_rate
    REAL                               :: cpu_first, cpu_last

    CALL parser%commands( cmds )

    !
    ! Get the name of the command file
    !
    IF ( COMMAND_ARGUMENT_COUNT() >= 1 ) THEN
        CALL GET_COMMAND_ARGUMENT( 1, filename )
        CALL parser%set_input( filename )
    ELSE
        WRITE(*,'(a)') 'Please specify the name of the commands file'
        STOP
    ENDIF

    !
    ! Process the commands
    !
    DO WHILE ( parser%has_next() )
        CALL parser%next( cmdidx, command )
        WRITE(*,'(a,a)') trim(command), '...'

        SELECT CASE( command )
            CASE( 'report-file' )
                IF ( lurep /= OUTPUT_UNIT ) THEN
                    CLOSE( lurep )
                ENDIF
                CALL parser%get_arg( 1, filename )
                OPEN( NEWUNIT = lurep, FILE = filename )
                CALL parser%set_output( lurep )

                WRITE( *, '(2a)' )    'Report written to ', TRIM(filename)

                WRITE( lurep, '(a)' ) 'Report of simulation'
                WRITE( lurep, '(a)' ) '--------------------'
                WRITE( lurep, '(a)' ) 'Compiler version: ', compiler_version()
                WRITE( lurep, '(a)' ) 'Compiler options: ', compiler_options()

            CASE( 'number-repetitions' )
                CALL parser%get_arg( 1, repetition )
                WRITE( lurep, '(/,a,i0)' ) 'Number of repetitions: ', repetition

            CASE( 'sequential-get' )
                CALL parser%get_arg( 1, step )
                CALL parser%get_arg( 2, number_seq )
                WRITE( lurep, '(/,a,i0)' ) 'Sequential get:'
                WRITE( lurep, '(a,i0)' )   '    Step size:       ', step
                WRITE( lurep, '(a,i0)' )   '    Number of steps: ', number_seq

                CALL sequential_get

            CASE( 'random-get' )
                CALL parser%get_arg( 1, number_random )
                WRITE( lurep, '(/,a,i0)' ) 'Random get:'
                WRITE( lurep, '(a,i0)' )   '    Number of steps: ', number_random

                CALL random_get

            CASE( 'allocate-view' )
                CALL parser%get_arg( 1, number_chunks )
                CALL parser%get_arg( 2, chunk_size )
                WRITE( lurep, '(/,a,i0)' ) 'Allocate view:'
                WRITE( lurep, '(a,i0)' )   '    Number of chunks: ', number_chunks
                WRITE( lurep, '(a,i0)' )   '    Chunk size:       ', chunk_size
                CALL allocate_view

            CASE( 'allocate-array' )
                CALL parser%get_arg( 1, array_size )
                WRITE( lurep, '(/,a,i0)' ) 'Allocate array:'
                WRITE( lurep, '(a,i0)' )   '    Array size:       ', array_size

                IF ( ALLOCATED(array) ) THEN
                    DEALLOCATE( array )
                ENDIF

                CALL start_timer
                ALLOCATE( array(array_size) )
                CALL stop_timer

            CASE( 'use-plain-array' )
                use_view = .false.
                WRITE( lurep, '(/,a,i0)' ) 'Note: using array'

            CASE( 'use-view' )
                use_view = .true.
                WRITE( lurep, '(/,a,i0)' ) 'Note: using view (catenation)'

            CASE DEFAULT
                WRITE( lurep, '(/,2a)' ) 'Unknown/unsupported command: ', TRIM(command)
        END SELECT
    ENDDO

    WRITE(*,'(a)') 'Done'

CONTAINS

! allocate_view --
!     Allocate a view of a given number of chunks of given size
!
! Arguments:
!     None
!
SUBROUTINE allocate_view
     INTEGER                        :: i
     INTEGER, DIMENSION(:), POINTER :: chunk1, chunk2

     CALL start_timer

     ALLOCATE( chunk1(chunk_size) )
     ALLOCATE( chunk2(chunk_size) )

     x = chunk1 // chunk2

     DO i = 3,number_chunks
         NULLIFY( chunk2 )
         ALLOCATE( chunk2(chunk_size) )
         x = x // chunk2
     ENDDO

     CALL stop_timer

END SUBROUTINE allocate_view

! sequential_get --
!     Sequentially get the elements of an array or a catenation
!
! Arguments:
!     None
!
! Note:
!     Sum the data merely to make sure an optimising compiler does not optimise away the loops
!
SUBROUTINE sequential_get
    INTEGER :: i, j, sz, repeat
    INTEGER :: total, subtotal

    CALL start_timer

    total = 0
    IF ( use_view ) THEN
        sz = SIZE(x)

        DO repeat = 1,repetition
            subtotal = 0
            DO i = 1,number_seq
                j = 1 + MOD((i-1) * step, sz )
                subtotal = subtotal + x%elem(j)
            ENDDO
            total = total + MOD(subtotal, 2)
        ENDDO
    ELSE
        sz = SIZE(array)

        DO repeat = 1,repetition
            subtotal = 0
            DO i = 1,number_seq
                j = 1 + MOD((i-1) * step, sz )
                subtotal = subtotal + array(j)
            ENDDO
            total = total + MOD(subtotal, 2)
        ENDDO
    ENDIF

    CALL stop_timer

END SUBROUTINE sequential_get

! random_get --
!     Randomly get the elements of an array or a catenation
!
! Arguments:
!     None
!
! Note:
!     Sum the data merely to make sure an optimising compiler does not optimise away the loops
!
SUBROUTINE random_get
    INTEGER                         :: i, j, sz, repeat
    INTEGER                         :: total, subtotal
    REAL, DIMENSION(:), ALLOCATABLE :: r

    !
    ! Keep the generation of the random numbers outside the timer bracket
    !
    ALLOCATE( r(number_random) )
    CALL RANDOM_NUMBER( r )

    CALL start_timer

    total = 0
    IF ( use_view ) THEN
        sz = SIZE(x)

        DO repeat = 1,repetition
            DO i = 1,number_random
                j = 1 + MOD( INT( sz * r(i) ), sz )
                subtotal = subtotal + x%elem(j)
            ENDDO
            total = total + MOD(subtotal, 2)
        ENDDO
    ELSE
        sz = SIZE(array)

        DO repeat = 1,repetition
            DO i = 1,number_random
                j = 1 + MOD( INT( sz * r(i) ), sz )
                subtotal = subtotal + array(j)
            ENDDO
            total = total + MOD(subtotal, 2)
        ENDDO
    ENDIF

    CALL stop_timer
END SUBROUTINE random_get

! start_timer
!     Initialise the time, so that we can keep track of how much wall clock/system time is spent
!
! Arguments:
!     None
!
SUBROUTINE start_timer

    CALL system_clock( count = time_first, count_rate = time_rate )
    CALL cpu_time( cpu_first )

END SUBROUTINE start_timer

! stop_timer
!     Measure the elapsed time and report it
!
! Arguments:
!     None
!
SUBROUTINE stop_timer

    CALL system_clock( count = time_last )
    CALL cpu_time( cpu_last )

    WRITE( lurep, '(a,g12.6)' ) 'Wall clock (s): ', (time_last - time_first) / real(time_rate)
    WRITE( lurep, '(a,g12.6)' ) 'CPU time (s):   ',  cpu_last  - cpu_first

END SUBROUTINE stop_timer

END PROGRAM moa_measure
