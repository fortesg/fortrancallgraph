MODULE inner

  IMPLICIT NONE

  INTEGER a, ai1, ai2, aia, b, bi1, bi2, bib, c

  PUBLIC

  CONTAINS

  SUBROUTINE s0

    CALL sa
    CALL sb
    CALL i1

  END SUBROUTINE s0

  !sa
  SUBROUTINE sa

    WRITE(*,*) a

    CALL i1

    CONTAINS

    !i1
    SUBROUTINE i1

      WRITE(*,*) ai1
      CALL i2

    END SUBROUTINE i1

    !i2
    SUBROUTINE i2

      WRITE(*,*) ai2
      CALL ia

    END SUBROUTINE i2

    !ia
    SUBROUTINE ia

      WRITE(*,*) aia

    END SUBROUTINE ia

  END SUBROUTINE sa


  !sb
  SUBROUTINE sb

    WRITE(*,*) b

    CALL i1

    CONTAINS

    !i1
    SUBROUTINE i1

      WRITE(*,*) bi1
      CALL i2

    END SUBROUTINE i1

    !i2
    SUBROUTINE i2

      WRITE(*,*) bi2
      CALL ib

    END SUBROUTINE i2

    !ib
    SUBROUTINE ib

      WRITE(*,*) bib

    END SUBROUTINE ib

  END SUBROUTINE sb

  !i1
  SUBROUTINE i1

    WRITE(*,*) c

  END SUBROUTINE i1

END MODULE inner
