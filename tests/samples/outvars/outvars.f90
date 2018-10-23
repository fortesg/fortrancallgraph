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

  TYPE(ttest) :: t1

CONTAINS

  FUNCTION get()
   TYPE(ttest) :: get
   get = t1
  END FUNCTION get

  FUNCTION part(p)
   TYPE(parent) :: p
   TYPE(ttest)  :: part
   part = p%child
  END FUNCTION part

  SUBROUTINE testFunc1()
    TYPE(ttest) :: var
    var = get()
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

END MODULE outvars
