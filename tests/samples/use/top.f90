MODULE top

  USE middle, ONLY : medium

  IMPLICIT NONE

  CONTAINS

  SUBROUTINE tiptop()

    CALL medium()

  END SUBROUTINE tiptop

END MODULE top
