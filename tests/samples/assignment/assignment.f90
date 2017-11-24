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

  CALL stest(test)

CONTAINS

  SUBROUTINE stest(inarg)

    TYPE(ttest), INTENT(in) :: inarg
    TYPE(ttest) :: var

    WRITE (*,*) 'first: ', inarg%first

    var = inarg

    WRITE (*,*) 'second: ', var%second

    inarg = inarg

  END SUBROUTINE stest

END PROGRAM assignment
