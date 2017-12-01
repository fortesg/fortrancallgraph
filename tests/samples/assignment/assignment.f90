MODULE assignment

  IMPLICIT NONE

  TYPE :: ttest
    INTEGER :: first
    INTEGER :: second
    INTEGER :: third
  END TYPE ttest

  INTERFACE operator (+)
    MODULE PROCEDURE ttadd
  END INTERFACE

  TYPE :: membertest
    TYPE(ttest), POINTER :: ptr
  END TYPE membertest

CONTAINS

  FUNCTION ttadd(tt1, tt2) result (tt3)
   TYPE(ttest), intent(in) :: tt1, tt2
   TYPE(ttest) :: tt3

   tt3%first = tt1%first + tt2%first

  END FUNCTION ttadd

  TYPE(ttest) FUNCTION dump(tt1)
   TYPE(ttest), intent(in) :: tt1

   dump%first = 0
   dump%second = 0
   dump%third = 0

  END FUNCTION dump

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

  SUBROUTINE testMember(inarg)

    TYPE(ttest), INTENT(inout), TARGET :: inarg
    TYPE(membertest) :: var

    WRITE (*,*) 'first: ', inarg%first

    var%ptr => inarg

    WRITE (*,*) 'second: ', var%ptr%second

  END SUBROUTINE testMember

  SUBROUTINE testOperator(tt1, tt2)

    TYPE(ttest), INTENT(in), TARGET :: tt1, tt2
    TYPE(ttest) :: dummy2, dummy3

    dummy2 = tt1 + tt2
    dummy3 = dump(tt1)

    WRITE (*,*) 'dummy2: ', dummy2%second
    WRITE (*,*) 'dummy3: ', dummy3%third

  END SUBROUTINE testOperator


END MODULE assignment
