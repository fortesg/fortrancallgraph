MODULE recursion

  IMPLICIT NONE

  TYPE :: test
    INTEGER :: counter
  END TYPE test

CONTAINS

  RECURSIVE SUBROUTINE recurse(var)

    TYPE(test), INTENT(inout) :: var

    WRITE (*,*) var%counter

    IF (var%counter < 42) THEN
      var%counter = var%counter + 1
      CALL recurse(var)
    END IF

  END SUBROUTINE recurse

END MODULE recursion
