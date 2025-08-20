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
  
  ! Get command line arguments (excluding argv(0) which contains executable path)
  ! command_argument_count() returns number of user arguments (not including argv(0))
  argc = command_argument_count()
  
  if (argc > 0) then
    allocate(character(len=256) :: args(argc))
    ! get_command_argument(i, ...) with i=1,2,... gets user arguments (not argv(0))
    do i = 1, argc
      call get_command_argument(i, args(i))
    end do
  else
    allocate(character(len=256) :: args(0))
  end if
  
  ! Parse configuration
  call parse_config(args, config, success, error_message)
  
  if (.not. success) then
    print *, "Error: " // trim(error_message)
    print *, ""
    print *, "Quick troubleshooting:"
    print *, "   * Run 'fortcov --help' for usage examples"
    print *, "   * Ensure source directory exists: ls -la <your_source_path>"
    print *, "   * Check if .gcov files are present: find . -name '*.gcov'"
    print *, "   * Try: fortcov --source=src --output=coverage.md"
    call exit(EXIT_FAILURE)
  end if
  
  ! Check for help/version/validate-config flags after successful parsing
  if (config%show_help) then
    call show_help()
    call exit(EXIT_SUCCESS)
  else if (config%show_version) then
    call show_version()
    call exit(EXIT_SUCCESS)
  else if (config%validate_config_only) then
    ! Only validate configuration, don't run analysis
    if (.not. validate_config(config)) then
      error_ctx%error_code = ERROR_INVALID_CONFIG
      error_ctx%message = "Configuration validation failed"
      print *, "Configuration validation failed: " // trim(error_ctx%message)
      call exit(EXIT_FAILURE)
    else
      print *, "Configuration is valid"
      call exit(EXIT_SUCCESS)
    end if
  end if
  
  ! Validate configuration for security and accessibility
  if (.not. validate_config(config)) then
    error_ctx%error_code = ERROR_INVALID_CONFIG
    error_ctx%message = "Configuration validation failed"
    print *, "Configuration validation failed: " // trim(error_ctx%message)
    print *, ""
    if (len_trim(error_ctx%suggestion) > 0) then
      print *, "Suggested fix: " // trim(error_ctx%suggestion)
      print *, ""
    end if
    print *, "For configuration help:"
    print *, "   • See example: cat fortcov.nml.example"
    print *, "   • Documentation: https://github.com/lazy-fortran/fortcov"
    call exit(EXIT_FAILURE)
  end if
  
  ! Run coverage analysis
  exit_code = run_coverage_analysis(config)
  
  call exit(exit_code)
end program main
