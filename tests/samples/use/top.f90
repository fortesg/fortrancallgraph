MODULE top

  USE middle, ONLY : medium
  USE next, ONLY : skippy

  IMPLICIT NONE

  CONTAINS

  SUBROUTINE tiptop()

    CALL medium()

  END SUBROUTINE tiptop

END MODULE top
