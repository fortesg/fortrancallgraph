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
    PROCEDURE :: teil3 => part3
    PROCEDURE :: gib => get2
  END TYPE parent

  TYPE :: grand
    TYPE(parent) :: child
  CONTAINS
    PROCEDURE :: teil => part2
  END TYPE grand

  TYPE :: proxy
  CONTAINS
    PROCEDURE :: wa => witha
    PROCEDURE :: wg => withg
  END TYPE proxy

  TYPE(ttest) :: t1, t2

  INTERFACE g
    MODULE PROCEDURE get
    MODULE PROCEDURE get2
  END INTERFACE g

  INTERFACE p
    MODULE PROCEDURE part
    MODULE PROCEDURE part2
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

  FUNCTION get2(p, i)
   CLASS(parent), INTENT(in) :: p
   INTEGER, INTENT(in) :: i
   TYPE(ttest) :: get2

   IF (i == 1) THEN
     get2 = t1
   ELSE
     get2 = t2
   END IF
  END FUNCTION get2

  FUNCTION part(p)
   CLASS(parent), INTENT(in) :: p
   TYPE(ttest)  :: part
   part = p%child
  END FUNCTION part

  FUNCTION part2(g, p)
   CLASS(grand), INTENT(in) :: g
   TYPE(parent), INTENT(in) :: p
   TYPE(ttest)  :: part2
   part2 = p%child
  END FUNCTION part2

  SUBROUTINE part3(p, c)
   CLASS(parent), INTENT(in) :: p
   TYPE(ttest), INTENT(out)  :: c
   c = p%child
  END SUBROUTINE part3

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

  SUBROUTINE witha(p, a, b, c)
    CLASS(proxy) :: p
    REAL, INTENT(in) :: a
    TYPE(parent), INTENT(inout) :: b
    TYPE(ttest), INTENT(out) :: c
    CALL withouta(a, b, c)
  END SUBROUTINE witha

  SUBROUTINE withg(p, a, b, c)
    CLASS(proxy) :: p
    INTEGER, INTENT(in) :: a
    TYPE(parent), INTENT(inout) :: b
    TYPE(ttest), INTENT(out) :: c
    CALL withoutg(a, b, c)
  END SUBROUTINE withg

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
    TYPE(parent), INTENT(inout) :: m(:)
    TYPE(ttest) :: temp(2)
    temp(:) = m(1)%teil()
    WRITE (*,*) 'first: ', temp(1)%first
    temp(:) = m(INT(SQRT(36.0), 4))%gib(42)
    WRITE (*,*) 'second: ', temp(2)%second
  END SUBROUTINE testFunc5

  SUBROUTINE testFunc6(p, f)
    TYPE(proxy), INTENT(in) :: p
    TYPE(parent), INTENT(inout) :: f
    TYPE(ttest) :: temp
    CALL p%wa(SQRT(36.0), f, temp)
    WRITE (*,*) 'first: ', temp%first
    CALL p%wg(INT(SQRT(36.0), 4), f, temp)
    WRITE (*,*) 'third: ', temp%third
  END SUBROUTINE testFunc6

  SUBROUTINE testFunc7(grandma, stepmother)
    TYPE(grand), INTENT(in) :: grandma(:)
    TYPE(parent), INTENT(in) :: stepmother
    TYPE(ttest) :: temp
    temp = grandma(INT(SQRT(36.0), 4))%teil(stepmother)
    WRITE (*,*) 'first: ', temp%first
  END SUBROUTINE testFunc7

  SUBROUTINE testFunc8(stepfather)
    TYPE(parent), INTENT(inout) :: stepfather
    TYPE(ttest) :: temp
    CALL stepfather%teil3(temp)
    WRITE (*,*) 'third: ', temp%third
  END SUBROUTINE testFunc8

END MODULE outvars
