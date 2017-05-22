MODULE middle

  IMPLICIT NONE

  CONTAINS

  SUBROUTINE medium()

    USE bottom, ONLY : butt

    CALL butt

  ENDSUBROUTINE medium

ENDMODULE middle
