MODULE functions

  IMPLICIT NONE

CONTAINS

  INTEGER FUNCTION func1(var)
    INTEGER, INTENT(in) :: var
    func1 = var
  END FUNCTION func1

  INTEGER FUNCTION func2(var) RESULT(r2)
    INTEGER, INTENT(in) :: var
    r2 = var
  END FUNCTION func2

  FUNCTION func3(var)
    INTEGER, INTENT(in) :: var
    INTEGER :: func3
    func3 = var
  END FUNCTION func3

  FUNCTION func4(var) RESULT(r4)
    INTEGER, INTENT(in) :: var
    INTEGER :: r4
    r4 = var
  END FUNCTION func4

  SUBROUTINE subr5(var, r5)
    INTEGER, INTENT(in) :: var
    INTEGER, INTENT(out) :: r5
    r5 = var
  END SUBROUTINE subr5

END MODULE functions
