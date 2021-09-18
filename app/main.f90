program main
  use stdlib_optval, only: optval
  use stdlib_stats, only: mean
  use stdlib_kinds, only: int64
  implicit none

  integer(int64), parameter :: N = 1e7
  integer, parameter :: M = 100
  real :: x
  integer(int64) :: t0, t1, t
  integer :: i, j

  ! dummy to negate init time
  call system_clock(t0)
  x = 0.
  do i=1,N
      call s_noif(x)
  end do
  call system_clock(t1)

  t = 0
  do j=1,M
      x = 0.
      call system_clock(t0)
      do i=1,N
          call s_noif(x)
      end do
      call system_clock(t1)
      t = t + t1-t0
  end do
  print*, "noif:", t/M
  print*, x

  t = 0
  do j=1,M
      x = 0.
      call system_clock(t0)
      do i=1,N
          call s_nonopt(x, .true.)
      end do
      call system_clock(t1)
      t = t + t1-t0
  end do
  print*, "nonopt:", t/M
  print*, x

  t = 0
  do j=1,M
      x = 0.
      call system_clock(t0)
      do i=1,N
          call s_present(x, .true.)
      end do
      call system_clock(t1)
      t = t + t1-t0
  end do
  print*, "present:", t/M
  print*, x

  t = 0
  do j=1,M
      x = 0.
      call system_clock(t0)
      do i=1,N
          call s_optval(x, .true.)
      end do
      call system_clock(t1)
      t = t + t1-t0
  end do
  print*, "optval:", t/M
  print*, x

contains

subroutine s_optval(x, opt)
real, intent(inout) :: x
logical, intent(in), optional :: opt

if(optval(opt, .false.)) then
    x = x * x - 1
end if
end subroutine

subroutine s_present(x, opt)
real, intent(inout) :: x
logical, intent(in), optional :: opt

if(present(opt)) then
    if(opt) then
        x = x * x - 1
    end if
end if
end subroutine

subroutine s_nonopt(x, nonopt)
real, intent(inout) :: x
logical, intent(in) :: nonopt

if(nonopt) then
    x = x * x - 1
end if
end subroutine

subroutine s_noif(x)
real, intent(inout) :: x

x = x * x - 1
end subroutine

end program main
