MODULE recursion

  IMPLICIT NONE

  TYPE :: test
    INTEGER :: first
    INTEGER :: second
  END TYPE test

CONTAINS

  RECURSIVE SUBROUTINE recurse(var)

    TYPE(test), INTENT(inout) :: var

    WRITE (*,*) 'recurse: ', var%first

    IF (var%first < 8) THEN
      var%first = var%first + 1
      CALL recurse(var)
    END IF

  END SUBROUTINE recurse

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

END MODULE recursion
