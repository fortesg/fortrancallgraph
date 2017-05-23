MODULE middle

  IMPLICIT NONE

  INTERFACE average

    MODULE PROCEDURE average_i
    MODULE PROCEDURE average_r
    MODULE PROCEDURE average_f

  ENDINTERFACE average

  LOGICAL, PARAMETER :: test = .TRUE.

  CONTAINS

  SUBROUTINE medium()

    USE bottom, ONLY : butt

    CALL butt

  ENDSUBROUTINE medium

  INTEGER FUNCTION average_i(numbers)

    INTEGER, DIMENSION(:) :: numbers

    average_i = 42

  ENDFUNCTION average_i

  REAL(kind=8) FUNCTION average_r(numbers)

    REAL(kind=8), DIMENSION(:) :: numbers

    average_r = 42

  END FUNCTION average_r

  REAL(kind=4) FUNCTION average_f(numbers)

    REAL(kind=4), DIMENSION(:) :: numbers

    average_f = 42

  ENDFUNCTION average_f

ENDMODULE middle

MODULE next

  IMPLICIT NONE

  TYPE kangaroo

    INTEGER :: feet(2), pouch
    REAL :: tail

  ENDTYPE kangaroo

  INTEGER, PARAMETER :: skippy = 42

ENDMODULE next
