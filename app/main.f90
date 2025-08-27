program main
  use fortcov, only: run_coverage_analysis
  use fortcov_config, only: config_t, parse_config, show_help, show_version, &
                                   validate_config, validate_config_with_context
  use error_handling, only: error_context_t, ERROR_SUCCESS
  use foundation_constants, only: EXIT_SUCCESS, EXIT_FAILURE
  use zero_config_auto_discovery_integration, only: enhance_zero_config_with_auto_discovery, &
                                                   execute_zero_config_complete_workflow
  use coverage_workflows, only: launch_coverage_tui_mode
  implicit none
  
  type(config_t) :: config
  character(len=:), allocatable :: args(:)
  character(len=256) :: error_message
  character(len=:), allocatable :: enhancement_error
  type(error_context_t) :: error_ctx
  logical :: success, enhancement_success
  integer :: exit_code, argc, i
  
  ! CRITICAL: Clean up any stale fork bomb prevention markers from previous runs
  ! This ensures that crashed or killed previous executions don't block current runs
  call execute_command_line('rm -f .fortcov_execution_marker')
  
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

  ! Apply enhanced auto-discovery integration for zero-configuration mode (Issue #281)
  if (config%zero_configuration_mode) then
    call enhance_zero_config_with_auto_discovery(config, enhancement_success, enhancement_error)
    if (.not. enhancement_success .and. .not. config%quiet) then
      print *, "⚠️  Auto-discovery enhancement failed: " // trim(enhancement_error)
      print *, "   Continuing with basic zero-configuration mode"
    end if
  end if
  
  ! Check for help/version/validate-config flags after successful parsing
  if (config%show_help) then
    call show_help()
    ! In quiet mode, help/version should exit with failure code
    if (config%quiet) then
      call exit(EXIT_FAILURE)
    else
      call exit(EXIT_SUCCESS)
    end if
  else if (config%show_version) then
    call show_version()
    ! In quiet mode, help/version should exit with failure code
    if (config%quiet) then
      call exit(EXIT_FAILURE)
    else
      call exit(EXIT_SUCCESS)
    end if
  else if (config%validate_config_only) then
    ! Only validate configuration, don't run analysis
    call validate_config_with_context(config, error_ctx)
    if (error_ctx%error_code /= ERROR_SUCCESS) then
      print *, "Configuration validation failed: " // trim(error_ctx%message)
      call exit(EXIT_FAILURE)
    else
      print *, "Configuration is valid"
      call exit(EXIT_SUCCESS)
    end if
  end if
  
  ! CRITICAL: Fork bomb prevention - check before validation (Issue #432)
  block
    logical :: marker_exists
    inquire(file='.fortcov_execution_marker', exist=marker_exists)
    if (marker_exists) then
      if (.not. config%quiet) then
        print *, "🛡️  Fork bomb prevention: fortcov execution disabled"
        print *, "    (fortcov detected it's running within a test environment)"
      end if
      call exit(EXIT_SUCCESS)
    end if
  end block
  
  ! Validate configuration for security and accessibility
  call validate_config_with_context(config, error_ctx)
  if (error_ctx%error_code /= ERROR_SUCCESS) then
    print *, trim(error_ctx%message)
    if (len_trim(error_ctx%suggestion) > 0) then
      print *, ""
      print *, "Suggested fix: " // trim(error_ctx%suggestion)
    end if
    print *, ""
    print *, "For configuration help:"
    print *, "   • See example: cat fortcov.nml.example"
    print *, "   • Documentation: https://github.com/lazy-fortran/fortcov"
    call exit(EXIT_FAILURE)
  end if
  
  ! Check for TUI mode
  if (config%tui_mode) then
    ! Launch Terminal User Interface
    exit_code = launch_coverage_tui_mode(config)
  else if (config%zero_configuration_mode) then
    ! Run coverage analysis - use complete auto-workflow in zero-configuration mode
    call execute_zero_config_complete_workflow(config, exit_code)
  else
    exit_code = run_coverage_analysis(config)
  end if
  
  call exit(exit_code)
end program main
