module fortcov
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, fortcov!"
  end subroutine say_hello
end module fortcov
