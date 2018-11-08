MODULE outvars

  IMPLICIT NONE

  TYPE :: ttest
    INTEGER :: first
    INTEGER :: second
    INTEGER :: third
  END TYPE ttest

  TYPE :: parent
    TYPE(ttest) :: child
  CONTAINS
    PROCEDURE :: teil => part
  END TYPE parent

  TYPE :: grand
    TYPE(parent) :: child
  END TYPE grand

  TYPE(ttest) :: t1, t2

  INTERFACE g
    MODULE PROCEDURE :: get
  END INTERFACE g

  INTERFACE p
    MODULE PROCEDURE part
  END INTERFACE p

  INTERFACE withOut
    MODULE PROCEDURE withOutA
    MODULE PROCEDURE withOutG
  END INTERFACE withOut

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
   CLASS(parent) :: p
   TYPE(ttest)  :: part
   part = p%child
  END FUNCTION part

  SUBROUTINE withOutA(a, b, c)
    REAL, INTENT(in) :: a
    TYPE(parent), INTENT(inout) :: b
    TYPE(ttest), INTENT(out) :: c

    c = b%child
  END SUBROUTINE withOutA

  SUBROUTINE withOutG(a, b, c)
    INTEGER, INTENT(in) :: a
    TYPE(parent), INTENT(inout) :: b
    TYPE(ttest), INTENT(out) :: c

    c = t1
  END SUBROUTINE withOutG

  SUBROUTINE testFunc1()
    TYPE(ttest) :: var1, var2
    var1 = get(1)
    WRITE (*,*) 'first: ', var1%first
    var2 = g(1)
    WRITE (*,*) 'second: ', var2%second
  END SUBROUTINE testFunc1

  SUBROUTINE testFunc2(mother)
    TYPE(parent), INTENT(in) :: mother
    TYPE(ttest) :: temp2, temp3
    temp2 = part(mother)
    WRITE (*,*) 'second: ', temp2%second
    temp3 = part(mother)
    WRITE (*,*) 'third: ', temp3%third
  END SUBROUTINE testFunc2

  SUBROUTINE testFunc3(grandpa)
    TYPE(grand), INTENT(in) :: grandpa
    TYPE(ttest) :: temp
    temp = p(grandpa%child)
    WRITE (*,*) 'third: ', temp%third
  END SUBROUTINE testFunc3

  SUBROUTINE testFunc4(father)
    TYPE(parent), INTENT(inout) :: father
    TYPE(ttest) :: temp
    CALL withOut(SQRT(36.0), father, temp)
    WRITE (*,*) 'first: ', temp%first
    CALL withOut(INT(SQRT(36.0), 4), father, temp)
    WRITE (*,*) 'second: ', temp%second
  END SUBROUTINE testFunc4

  SUBROUTINE testFunc5(m)
    TYPE(parent), INTENT(inout) :: m
    TYPE(ttest) :: temp(2)
    temp(:) = m%teil()
    WRITE (*,*) 'first: ', temp(1)%first
  END SUBROUTINE testFunc5

END MODULE outvars
