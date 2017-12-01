MODULE typeprocedure

  IMPLICIT NONE

  TYPE :: ttest
    INTEGER :: first(3)
    INTEGER :: second(3)
  CONTAINS
    PROCEDURE :: third => dump
  END TYPE ttest

CONTAINS

  INTEGER FUNCTION dump(test, var)
    CLASS(ttest), INTENT(in) :: test
    INTEGER, INTENT(in) :: var
    dump = var
  END FUNCTION dump

  SUBROUTINE test(t)

    TYPE(ttest), INTENT(in) :: t

    WRITE (*,*) 'first: ',  t%first(1)
    WRITE (*,*) 'second: ', t%second(2)
    WRITE (*,*) 'third: ',  t%third(3)

  END SUBROUTINE test

END MODULE typeprocedure
