program main
  use fortcov
  use fortcov_config, only: validate_config
  use error_handling
  implicit none
  
  type(config_t) :: config
  character(len=:), allocatable :: args(:)
  character(len=256) :: error_message
  type(error_context_t) :: error_ctx
  logical :: success
  integer :: exit_code, argc, i
  
  ! Get command line arguments
  argc = command_argument_count()
  
  if (argc > 0) then
    allocate(character(len=256) :: args(argc))
    do i = 1, argc
      call get_command_argument(i, args(i))
    end do
  else
    allocate(character(len=256) :: args(0))
  end if
  
  ! Parse configuration
  call parse_config(args, config, success, error_message)
  
  if (.not. success) then
    if (config%show_help) then
      call show_help()
      call exit(EXIT_SUCCESS)
    else if (config%show_version) then
      call show_version()
      call exit(EXIT_SUCCESS)
    else
      print *, "Error:", trim(error_message)
      call exit(EXIT_FAILURE)
    end if
  end if
  
  ! Validate configuration for security and accessibility
  call validate_config(config, error_ctx)
  if (error_ctx%error_code /= ERROR_SUCCESS) then
    print *, "Configuration validation failed:", trim(error_ctx%message)
    if (len_trim(error_ctx%suggestion) > 0) then
      print *, "Suggestion:", trim(error_ctx%suggestion)
    end if
    call exit(EXIT_FAILURE)
  end if
  
  ! Run coverage analysis
  exit_code = run_coverage_analysis(config)
  
  call exit(exit_code)
end program main
