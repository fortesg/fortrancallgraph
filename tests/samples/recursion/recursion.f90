MODULE recursion

  IMPLICIT NONE

  TYPE :: test
    INTEGER :: first
    INTEGER :: second
  END TYPE test

  TYPE :: rtest
    INTEGER :: first
    INTEGER :: second
    TYPE(rtest), POINTER :: next
  END TYPE rtest

CONTAINS

  RECURSIVE SUBROUTINE recurse(var)

    TYPE(test), INTENT(inout) :: var

    WRITE (*,*) 'recurse: ', var%first

    IF (var%first < 8) THEN
      var%first = var%first + 1
      CALL recurse(var)
    END IF

  END SUBROUTINE recurse

  RECURSIVE INTEGER FUNCTION refunc(var, start) RESULT(r)

    TYPE(test), INTENT(inout) :: var
    INTEGER, INTENT(in) :: start

    IF (start < var%first) THEN
      r = refunc(var, start * 2)
    ELSE
      r = start
    END IF

  END FUNCTION refunc

  SUBROUTINE indirect1(var)

    TYPE(test), INTENT(inout) :: var

    WRITE (*,*) 'indirect1: ', var%first

    IF (var%first < 12) THEN
      var%first = var%first + 1
      CALL indirect2(var)
    END IF

  END SUBROUTINE indirect1

  SUBROUTINE indirect2(var)

    TYPE(test), INTENT(inout) :: var

    WRITE (*,*) 'indirect2: ', var%second

    IF (var%second < 12) THEN
      var%second = var%second + 1
      CALL indirect1(var)
    END IF

  END SUBROUTINE indirect2

  RECURSIVE SUBROUTINE position(var1, var2, i)

    TYPE(test), INTENT(in) :: var1, var2
    INTEGER, INTENT(in) :: i

    IF (MOD(i, 2) == 1) THEN
      WRITE (*,*) 'first: ', var1%first
      WRITE (*,*) 'second: ', var2%second
    ELSE
      CALL position(var2, var1, i + 1)
    END IF

  END SUBROUTINE position

  SUBROUTINE recdata(r)
    TYPE(rtest), INTENT(in) :: r

    WRITE (*,*) r%next%next%next%first
    WRITE (*,*) r%second

  END SUBROUTINE recdata

END MODULE recursion
