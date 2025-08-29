program main
  use fortcov_core, only: run_coverage_analysis
  use config_core, only: config_t, parse_config, show_help, show_version, &
                                   validate_config, validate_config_with_context
  use error_handling_core, only: error_context_t, ERROR_SUCCESS, &
                                    ERROR_INVALID_CONFIG, clear_error_context
  use constants_core, only: EXIT_SUCCESS, EXIT_FAILURE
  use zero_config_core, only: enhance_zero_config_with_auto_discovery, &
                                                   execute_zero_config_complete_workflow
  use coverage_workflows, only: launch_coverage_tui_mode
  use size_enforcement_core, only: enforce_size_limits_for_ci, &
                                   size_enforcement_config_t, ci_enforcement_result_t
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
  else if (config%validate_architecture) then
    ! Only validate architectural size compliance, don't run coverage analysis
    call handle_architectural_validation(config, error_ctx)
    if (error_ctx%error_code /= ERROR_SUCCESS) then
      call exit(EXIT_FAILURE)
    else
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

contains

  subroutine handle_architectural_validation(config, error_ctx)
    !! Handle architectural size validation and exit appropriately
    type(config_t), intent(in) :: config
    type(error_context_t), intent(out) :: error_ctx
    
    type(size_enforcement_config_t) :: enforcement_config
    type(ci_enforcement_result_t) :: enforcement_result
    
    ! Initialize error context
    call clear_error_context(error_ctx)
    
    ! Configure size enforcement based on user flags
    enforcement_config%fail_on_warnings = config%fail_on_size_warnings
    enforcement_config%fail_on_violations = .true.  ! Always fail on violations
    enforcement_config%generate_github_annotations = &
        (trim(config%architecture_output_format) == "ci")
    enforcement_config%base_directory = "."
    enforcement_config%output_format = config%architecture_output_format
    enforcement_config%verbose_output = config%verbose
    
    ! Run architectural size enforcement
    call enforce_size_limits_for_ci(enforcement_config, enforcement_result)
    
    ! Display results
    if (.not. config%quiet) then
      print *, trim(enforcement_result%summary_message)
      if (config%verbose .and. len_trim(enforcement_result%detailed_report) > 0) then
        print *, ""
        print *, trim(enforcement_result%detailed_report)
      end if
      if (len_trim(enforcement_result%remediation_actions) > 0) then
        print *, ""
        print *, trim(enforcement_result%remediation_actions)
      end if
    end if
    
    ! Set appropriate error context based on results and user flags
    if (enforcement_result%should_block_merge) then
      error_ctx%error_code = ERROR_INVALID_CONFIG
      error_ctx%message = "Architectural size violations detected"
    else if (config%fail_on_size_warnings .and. enforcement_result%warnings_count > 0) then
      error_ctx%error_code = ERROR_INVALID_CONFIG
      error_ctx%message = "Architectural size warnings detected (fail-on-warnings enabled)"
    end if
    
  end subroutine handle_architectural_validation

end program main
