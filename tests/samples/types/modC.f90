MODULE modC

  USE modA, ONLY : typeA => type1

  IMPLICIT NONE

  TYPE, ABSTRACT :: type2
    LOGICAL :: member
  END TYPE type2

  TYPE, EXTENDS(type2) :: type3
    TYPE(typeA) :: member1
  END TYPE type3

END MODULE modC
