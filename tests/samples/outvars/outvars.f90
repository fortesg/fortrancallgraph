MODULE outvars

  IMPLICIT NONE

  TYPE :: ttest
    INTEGER :: first
    INTEGER :: second
    INTEGER :: third
  END TYPE ttest

  TYPE :: parent
    TYPE(ttest) :: child
  END TYPE parent

  TYPE :: grand
    TYPE(parent) :: child
  END TYPE grand

  TYPE(ttest) :: t1, t2

CONTAINS

  FUNCTION primitive()
    INTEGER :: primitive
    primitive = t1%second
  END FUNCTION primitive

  FUNCTION get(i)
   INTEGER, INTENT(in) :: i
   TYPE(ttest) :: get

   IF (i == 1) THEN
     get = t1
   ELSE
     get = t2
   END IF
  END FUNCTION get

  FUNCTION part(p)
   TYPE(parent) :: p
   TYPE(ttest)  :: part
   part = p%child
  END FUNCTION part

  SUBROUTINE withOutA(a, b, c)
    INTEGER, INTENT(in) :: a
    TYPE(parent), INTENT(inout) :: b
    TYPE(ttest), INTENT(out) :: c

    c = b%child
  END SUBROUTINE withOutA

  SUBROUTINE testFunc1()
    TYPE(ttest) :: var
    var = get(1)
    WRITE (*,*) 'first: ', var%first
  END SUBROUTINE testFunc1

  SUBROUTINE testFunc2(mother)
    TYPE(parent), INTENT(in) :: mother
    TYPE(ttest) :: temp
    temp = part(mother)
    WRITE (*,*) 'second: ', temp%second
  END SUBROUTINE testFunc2

  SUBROUTINE testFunc3(grandpa)
    TYPE(grand), INTENT(in) :: grandpa
    TYPE(ttest) :: temp
    temp = part(grandpa%child)
    WRITE (*,*) 'third: ', temp%third
  END SUBROUTINE testFunc3

  SUBROUTINE testFunc4(father)
    TYPE(parent), INTENT(inout) :: father
    TYPE(ttest) :: temp
    CALL withOutA(42, father, temp)
    WRITE (*,*) 'third: ', temp%first
  END SUBROUTINE testFunc4

END MODULE outvars
