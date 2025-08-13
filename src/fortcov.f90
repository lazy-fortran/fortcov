module fortcov
  use fortcov_config, only: config_t, parse_config, show_help
  implicit none
  private

  public :: say_hello
  public :: config_t, parse_config, show_help
contains
  subroutine say_hello
    print *, "Hello, fortcov!"
  end subroutine say_hello
end module fortcov
