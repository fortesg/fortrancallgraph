MODULE typeprocedure

  IMPLICIT NONE

  TYPE :: ttest
    INTEGER :: first(3)
    INTEGER :: second(3)
  CONTAINS
    PROCEDURE :: third => dump
    PROCEDURE, PASS :: fourth => addInt
    PROCEDURE :: fifth => addAnother
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

  INTEGER FUNCTION addAnother(test1, test2)
    CLASS(ttest), INTENT(in) :: test1, test2

    addAnother = test1%second(2) + test2%second(3)
  END FUNCTION addAnother

  SUBROUTINE test(t)

    TYPE(ttest), INTENT(in) :: t

    WRITE (*,*) 'first: ',  t%first(1)
    WRITE (*,*) 'second: ', t%second(2)
    WRITE (*,*) 'third: ',  t%third(3)

  END SUBROUTINE test

  SUBROUTINE testIndirect(t)

    TYPE(ttest), INTENT(in) :: t

    CALL test(t)

  END SUBROUTINE testIndirect

  SUBROUTINE testAdd(t)

    TYPE(ttest), INTENT(in) :: t

    WRITE (*,*) 'first: ',  t%first(1)
    WRITE (*,*) 'fourth: ',  t%fourth(4)

  END SUBROUTINE testAdd

  SUBROUTINE testAnother(t1, t2)

    TYPE(ttest), INTENT(in) :: t1, t2

    WRITE (*,*) 'firstA: ',  t1%first(1)
    WRITE (*,*) 'firstB: ',  t2%first(2)
    WRITE (*,*) 'fifth: ',  t1%fifth(t2)

  END SUBROUTINE testAnother

END MODULE typeprocedure
