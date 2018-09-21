MODULE typeprocedure

  IMPLICIT NONE

  TYPE, ABSTRACT :: atest
  CONTAINS
    PROCEDURE(third_interface), DEFERRED :: third
  END TYPE atest

  TYPE, EXTENDS(atest) :: ttest
    INTEGER :: first(3)
    INTEGER :: second(3)
  CONTAINS
    PROCEDURE :: third => dump
    PROCEDURE, PASS :: fourth => addInt
    PROCEDURE :: fifth => addAnother
    GENERIC :: sixth => fourth, fifth
  END TYPE ttest

  TYPE, EXTENDS(ttest) :: child
  CONTAINS
    PROCEDURE :: third => new_dump
  END TYPE child

CONTAINS

  INTEGER FUNCTION third_interface(test, var)
    CLASS(atest), INTENT(in) :: test
    INTEGER, INTENT(in) :: var
  END FUNCTION third_interface

  INTEGER FUNCTION dump(test, var)
    CLASS(ttest), INTENT(in) :: test
    INTEGER, INTENT(in) :: var
    dump = var
  END FUNCTION dump

  INTEGER FUNCTION new_dump(test, var)
    CLASS(child), INTENT(in) :: test
    INTEGER, INTENT(in) :: var
    new_dump = var + test%ttest%fourth(var)
  END FUNCTION new_dump

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

  SUBROUTINE testGeneric(t1, t2)

    TYPE(ttest), INTENT(in) :: t1, t2

    WRITE (*,*) 'firstA: ',  t1%first(1)
    WRITE (*,*) 'firstB: ',  t2%first(2)
    WRITE (*,*) 'sixth: ',  t1%sixth(t2)

  END SUBROUTINE testGeneric

  SUBROUTINE testChild(t)

    TYPE(child), INTENT(in) :: t

    WRITE (*,*) 'third: ',  t%third(3)

  END SUBROUTINE testChild

  SUBROUTINE testDeferred(a)

    CLASS(atest), INTENT(in) :: a

    WRITE (*,*) 'third: ',  a%third(3)

  END SUBROUTINE testDeferred

END MODULE typeprocedure
