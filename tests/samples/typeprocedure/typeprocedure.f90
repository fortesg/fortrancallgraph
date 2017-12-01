MODULE typeprocedure

  IMPLICIT NONE

  TYPE :: ttest
    INTEGER :: first(3)
    INTEGER :: second(3)
  CONTAINS
    PROCEDURE :: third => dump
    PROCEDURE, PASS :: fourth => addInt
  END TYPE ttest

CONTAINS

  INTEGER FUNCTION dump(test, var)
    CLASS(ttest), INTENT(in) :: test
    INTEGER, INTENT(in) :: var
    dump = var
  END FUNCTION dump

  INTEGER FUNCTION addInt(test, var)
    CLASS(ttest), INTENT(in) :: test
    INTEGER, INTENT(in) :: var

    addInt = test%second(2) + var
  END FUNCTION addInt

  SUBROUTINE test(t)

    TYPE(ttest), INTENT(in) :: t

    WRITE (*,*) 'first: ',  t%first(1)
    WRITE (*,*) 'second: ', t%second(2)
    WRITE (*,*) 'third: ',  t%third(3)

  END SUBROUTINE test

  SUBROUTINE testAdd(t)

    TYPE(ttest), INTENT(in) :: t

    WRITE (*,*) 'first: ',  t%first(1)
    WRITE (*,*) 'fourth: ',  t%fourth(4)

  END SUBROUTINE testAdd

END MODULE typeprocedure
