MODULE subtype

  USE typeprocedure, ONLY: atest

  IMPLICIT NONE

  TYPE, EXTENDS(atest) :: othertest
    INTEGER :: abc(3)
    INTEGER :: def(3)
  CONTAINS
    PROCEDURE :: third => other_dump
  END TYPE othertest

CONTAINS

  INTEGER FUNCTION other_dump(test, var)
    CLASS(othertest), INTENT(in) :: test
    INTEGER, INTENT(in) :: var
    other_dump = var + test%abc(var)
  END FUNCTION other_dump

END MODULE subtype
