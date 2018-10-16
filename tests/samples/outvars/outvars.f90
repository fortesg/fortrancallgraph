MODULE outvars

  IMPLICIT NONE

  TYPE :: ttest
    INTEGER :: first
    INTEGER :: second
    INTEGER :: third
  END TYPE ttest

  TYPE(ttest) :: t1

CONTAINS

  FUNCTION get()
   TYPE(ttest) :: get
   get = t1
  END FUNCTION get

  SUBROUTINE testFunc()

    TYPE(ttest) :: var
    var = get()
    WRITE (*,*) 'first: ', var%first

  END SUBROUTINE testFunc

END MODULE outvars
