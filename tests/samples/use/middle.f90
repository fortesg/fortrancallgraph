MODULE middle

  IMPLICIT NONE

  CONTAINS

  SUBROUTINE medium()

    USE bottom, ONLY : butt

    CALL butt

  ENDSUBROUTINE medium

  SUBROUTINE average()

    WRITE (*,*) "So random!"

  ENDSUBROUTINE average

ENDMODULE middle

MODULE next

  IMPLICIT NONE

  TYPE :: kangaroo

    INTEGER :: feet(2), pouch
    REAL :: tail

  END TYPE kangaroo

  INTEGER, PARAMETER :: skippy = 42

ENDMODULE next
