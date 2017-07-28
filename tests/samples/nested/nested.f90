MODULE nested

  IMPLICIT NONE

  INTEGER, PARAMETER :: max_rank = 7

  TYPE, PUBLIC :: extent
    SEQUENCE
    INTEGER :: first
    INTEGER :: size
  END TYPE extent

  TYPE global_array_desc
    INTEGER :: a_rank
    TYPE(extent) :: rect(max_rank)
    INTEGER :: element_dt
  END TYPE global_array_desc

  TYPE dist_mult_array
    INTEGER :: num_sub_arrays
    TYPE(global_array_desc), ALLOCATABLE :: sub_arrays_global_desc(:)
    TYPE(extent), ALLOCATABLE :: local_chunks(:, :, :)
    INTEGER :: comm
    INTEGER :: comm_size
    INTEGER :: comm_rank
    INTEGER :: win
    INTEGER :: exposure_status
    INTEGER :: sync_mode
    INTEGER :: access_stamp, valid_stamp
    INTEGER(KIND=8) :: max_win_size
  END TYPE dist_mult_array

  INTERFACE is_contained_in
    MODULE PROCEDURE is_contained_in_e
    MODULE PROCEDURE is_contained_in_e_nd
  END INTERFACE is_contained_in

CONTAINS

  ELEMENTAL FUNCTION is_contained_in_e(i, rng) RESULT(p)
    INTEGER, INTENT(in) :: i
    TYPE(extent), INTENT(in) :: rng
    LOGICAL :: p
    p = i >= rng%first .AND. i < rng%first + rng%size
  END FUNCTION is_contained_in_e

  FUNCTION is_contained_in_e_nd(i, rng) RESULT(p)
    INTEGER, INTENT(in) :: i(:)
    TYPE(extent), INTENT(in) :: rng(:)
    LOGICAL :: p
    p = ALL(i >= rng%first .AND. i < rng%first + rng%size)
  END FUNCTION is_contained_in_e_nd

  SUBROUTINE assertion(cond, source, line, msg)
    LOGICAL, INTENT(in) :: cond
    CHARACTER(*), INTENT(in) :: source, msg
    INTEGER, INTENT(in) :: line
    IF (.NOT. cond) THEN
      WRITE (*,*) "assertion "//msg//" failed", source, line
      STOP
    END IF
  END SUBROUTINE assertion

  SUBROUTINE test(dm_array, sub_array, coord, &
       ref_extent)

    TYPE(dist_mult_array), INTENT(inout) :: dm_array
    INTEGER, INTENT(in) :: sub_array
    INTEGER, INTENT(in) :: coord(:)
    INTEGER(KIND=8), INTENT(in) :: ref_extent

    INTEGER :: ref_rank
    INTEGER :: cache_idx, ierror
    INTEGER(KIND=8) :: lb, extent

    CALL assertion(is_contained_in(coord, &
         dm_array%sub_arrays_global_desc(sub_array)%rect(1:ref_rank)), &
         __FILE__, &
         __LINE__, "invalid coordinate")

  END SUBROUTINE test

END MODULE nested
