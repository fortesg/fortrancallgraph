PROGRAM assignment

  IMPLICIT NONE

  TYPE :: test
    INTEGER :: counter
  END TYPE test

  TYPE(test) :: var
  var%counter = 0

  CALL recurse(var)

CONTAINS

  RECURSIVE SUBROUTINE recurse(var)

    TYPE(test), INTENT(inout) :: var

    WRITE (*,*) var%counter

    IF (var%counter < 42) THEN
      var%counter = var%counter + 1
      CALL recurse(var)
    END IF

  END SUBROUTINE recurse

END PROGRAM assignment
