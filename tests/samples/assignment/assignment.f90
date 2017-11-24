PROGRAM nested

  IMPLICIT NONE

  TYPE :: ttest
    INTEGER :: first
    INTEGER :: second
  END TYPE ttest

  TYPE(ttest) :: test

  test%first = 23
  test%second = 42

  CALL stest(test)

CONTAINS

  SUBROUTINE stest(inarg)

    TYPE(ttest), INTENT(in) :: inarg

    WRITE (*,*) inarg%first

  END SUBROUTINE stest

END PROGRAM nested
