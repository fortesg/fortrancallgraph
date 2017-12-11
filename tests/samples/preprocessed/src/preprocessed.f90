MODULE preprocessed

  IMPLICIT NONE

#include "interface.inc"

  TYPE ttest
    INTEGER :: i0, i1(4), i2(2,5)
  END TYPE ttest

CONTAINS

  SUBROUTINE test_0d(src, dest)
    TYPE(ttest), INTENT(in) :: src
    INTEGER, INTENT(out) :: dest
    dest = src%i0
  END SUBROUTINE test_0d

  SUBROUTINE test_1d(src, dest)
    TYPE(ttest), INTENT(in) :: src
    INTEGER, INTENT(out) :: dest(:)
    dest = src%i1
  END SUBROUTINE test_1d

  SUBROUTINE test_2d(src, dest)
    TYPE(ttest), INTENT(in) :: src
    INTEGER, INTENT(out) :: dest(:,:)
    dest = src%i2
  END SUBROUTINE test_2d

  SUBROUTINE caller(t0, t1, t2)

    TYPE(ttest), INTENT(in) :: t0, t1, t2
    INTEGER :: i0, i1(4), i2(2,5)

    CALL test(t0, i0)
    CALL test(t1, i1)
    CALL test(t2, i2)

  END SUBROUTINE caller

END MODULE preprocessed
