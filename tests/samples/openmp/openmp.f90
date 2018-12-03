PROGRAM openmp

  USE omp_lib

  IMPLICIT NONE

  TYPE :: test
    INTEGER :: var1
    INTEGER :: var2
  END TYPE test

  TYPE(test) :: tester(32)
  tester(:)%var1 = 23
  tester(:)%var2 = 42

  CALL start(tester)

CONTAINS

  SUBROUTINE start(arg)
    TYPE(test) :: arg(:)
    INTEGER :: i

    WRITE (*,*) 'Number of threads: ', omp_get_max_threads()

    !$OMP PARALLEL DO
    DO i = 1, SIZE(arg)
      arg(i)%var2 = random(i)
      WRITE (*,*) i, ': ', arg(i)%var2
    END DO
    !$OMP END PARALLEL DO

  END SUBROUTINE start

  INTEGER FUNCTION random(i)
    INTEGER, INTENT(in) :: i
    REAL :: harvest
    CALL RANDOM_NUMBER(harvest)
    random = INT(harvest * tester(i)%var2)
  END FUNCTION random

END PROGRAM openmp
