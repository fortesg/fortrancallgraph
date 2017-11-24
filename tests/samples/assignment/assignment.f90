PROGRAM assignment

  IMPLICIT NONE

  TYPE :: ttest
    INTEGER :: first
    INTEGER :: second
    INTEGER :: third
  END TYPE ttest

  TYPE(ttest) :: test

  test%first = 23
  test%second = 42
  test%third = 109

  CALL testDirect(test)

  CALL testIndirect(test)

CONTAINS

  SUBROUTINE testDirect(inarg)

    TYPE(ttest), INTENT(inout) :: inarg
    TYPE(ttest) :: var

    WRITE (*,*) 'first: ', inarg%first

    var = inarg

    inarg = inarg

    WRITE (*,*) 'second: ', var%second

  END SUBROUTINE testDirect

  SUBROUTINE testIndirect(inarg)

    TYPE(ttest), INTENT(inout) :: inarg
    TYPE(ttest) :: var

    WRITE (*,*) 'first: ', inarg%first

    var = inarg

    inarg = var

    WRITE (*,*) 'second: ', var%second

  END SUBROUTINE testIndirect

END PROGRAM assignment
