MODULE bottom

  IMPLICIT NONE

  TYPE :: foot

    INTEGER :: toes(5)
    LOGICAL :: left, right
    REAL, POINTER :: leg

  END TYPE foot

  INTERFACE butt

    MODULE PROCEDURE butt_x
    MODULE PROCEDURE butt_i0, butt_i1

  END INTERFACE butt

  CONTAINS

  SUBROUTINE butt_x()

    WRITE (*,*) 'Deep down'

  END SUBROUTINE butt_x

  SUBROUTINE butt_i0(times)

    INTEGER, INTENT(in) :: times

    WRITE (*,*) 'Deep down ', times

  END SUBROUTINE butt_i0

  SUBROUTINE butt_i1(times)

    INTEGER, INTENT(in) :: times(2)

    WRITE (*,*) 'Deep down ', times(1), ' x ', times(2)

  END SUBROUTINE butt_i1

END MODULE bottom
