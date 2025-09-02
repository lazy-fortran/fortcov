program main
  use fortcov_core, only: run_coverage_analysis
  use config_core, only: config_t, parse_config, show_help, show_version
  use system_exit_handler, only: exit_with_code
  use constants_core, only: EXIT_INVALID_CONFIG, EXIT_SUCCESS
  implicit none

  type(config_t) :: config
  character(len=:), allocatable :: args(:)
  character(len=:), allocatable :: error_message
  logical :: success
  integer :: exit_code, argc, i, arg_len, current_len

  ! Collect command-line arguments (excluding argv(0))
  argc = command_argument_count()

  if (argc > 0) then
    arg_len = 0
    do i = 1, argc
      call get_command_argument(i, length=current_len)
      arg_len = max(arg_len, current_len)
    end do
    allocate(character(len=arg_len) :: args(argc))
    do i = 1, argc
      call get_command_argument(i, args(i))
    end do
  else
    allocate(character(len=0) :: args(0))
  end if

  allocate(character(len=1024) :: error_message)

  ! Parse CLI into config (minimal flags supported by parser)
  call parse_config(args, config, success, error_message)
  if (.not. success) then
    write(*,'(A)') 'Error: ' // trim(error_message)
    call exit_with_code(EXIT_INVALID_CONFIG)
  end if

  ! Help/version short-circuit
  if (config%show_help) then
    call show_help()
    call exit_with_code(EXIT_SUCCESS)
  else if (config%show_version) then
    call show_version()
    call exit_with_code(EXIT_SUCCESS)
  end if

  ! Run coverage analysis and exit with its code
  exit_code = run_coverage_analysis(config)
  call exit_with_code(exit_code)

end program main
