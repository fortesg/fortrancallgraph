MODULE assignment

  IMPLICIT NONE

  TYPE :: ttest
    INTEGER :: first
    INTEGER :: second
    INTEGER :: third
  END TYPE ttest
!
!  INTERFACE operator (+)
!    MODULE PROCEDURE ttadd
!  END INTERFACE

  TYPE :: membertest
    TYPE(ttest), POINTER :: ptr
  END TYPE membertest

!  TYPE(ttest) :: test
!
!  test%first = 23
!  test%second = 42
!  test%third = 109
!
!  CALL testDirect(test)
!  CALL testIndirect(test)
!  CALL testMember(test)

CONTAINS

!  FUNCTION ttadd(tt1, tt2) result (tt3)
!   TYPE(ttest), intent(in) :: tt1, tt2
!   TYPE(ttest) :: tt3
!
!   tt3%first = tt1%first + tt2%first ! Note the use of array operations
!
!  END FUNCTION ttadd

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

END MODULE assignment
