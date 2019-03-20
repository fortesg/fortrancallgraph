MODULE alwaysfull

  IMPLICIT NONE

  TYPE :: a
    INTEGER :: one
    INTEGER :: two
  END TYPE a

  TYPE :: b
    TYPE(a) :: a
    INTEGER :: three
  END TYPE b

  TYPE :: c
    INTEGER :: four
    INTEGER :: five
  END TYPE c

  TYPE :: d
    TYPE(b) :: b
  END TYPE d

  TYPE(b) :: globalb1, globalb2

CONTAINS

  SUBROUTINE test(argA, argB, argC, argD)

    TYPE(a), INTENT(in) :: argA
    TYPE(b), INTENT(in) :: argB
    TYPE(c), INTENT(in) :: argC
    TYPE(d), INTENT(in) :: argD

    globalb1 = argB
    CALL ignore(argA, argB, argC)
    CALL ignore(argA, argD%b, argC)

  END SUBROUTINE test

  SUBROUTINE ignore(argA, argB, argC)

    TYPE(a), INTENT(in) :: argA
    TYPE(b), INTENT(in) :: argB
    TYPE(c), INTENT(in) :: argC

    WRITE (*,*) argC%four
    WRITE (*,*) argC%five

  END SUBROUTINE ignore

END MODULE alwaysfull
