MODULE bottom

  IMPLICIT NONE

  TYPE :: foot

    INTEGER :: toes(5)
    LOGICAL :: left, right
    REAL, POINTER :: leg

  END TYPE foot

  CONTAINS

  SUBROUTINE butt()

    WRITE (*,*) 'Deep down'

  END SUBROUTINE butt

END MODULE bottom
