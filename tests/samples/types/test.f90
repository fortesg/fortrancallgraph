MODULE test

  USE modA, ONLY: type0
  USE modB, ONLY: type1, typeB => type2
  USE modC

  IMPLICIT NONE

  TYPE(type0) :: var0
  TYPE(type1) :: var1
  TYPE(typeB) :: var2
  TYPE(type3) :: var3

END MODULE test
