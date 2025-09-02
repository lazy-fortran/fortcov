program main
  use fortcov_core, only: run_coverage_analysis
  use config_core, only: config_t, parse_config, show_help, show_version, &
                                   validate_config, validate_config_with_context
  use error_handling_core, only: error_context_t, ERROR_SUCCESS, &
                                     ERROR_INVALID_CONFIG, ERROR_THRESHOLD_NOT_MET, clear_error_context
  use constants_core, only: EXIT_SUCCESS, EXIT_FAILURE, EXIT_THRESHOLD_NOT_MET, &
                              EXIT_NO_COVERAGE_DATA, EXIT_INVALID_CONFIG, &
                              EXIT_FILE_ACCESS_ERROR, EXIT_MEMORY_ERROR, &
                              EXIT_VALIDATION_ERROR
  use system_exit_handler, only: exit_success_clean, exit_failure_clean, &
                                    exit_invalid_config_clean, exit_threshold_not_met_clean, &
                                    exit_no_coverage_data_clean, exit_file_access_error_clean, &
                                    exit_memory_error_clean, exit_validation_error_clean
  use zero_config_core, only: enhance_zero_config_with_auto_discovery, &
                                                   execute_zero_config_complete_workflow
  ! TUI removed: no interactive mode
  use size_enforcement_core, only: enforce_size_limits_for_ci, &
                                   size_enforcement_config_t, ci_enforcement_result_t
  use file_ops_secure, only: safe_remove_file
  implicit none
  
  type(config_t) :: config
  character(len=:), allocatable :: args(:)
  character(len=:), allocatable :: error_message
  character(len=:), allocatable :: enhancement_error
  type(error_context_t) :: error_ctx
  type(error_context_t) :: marker_error_ctx
  logical :: success, enhancement_success
  integer :: exit_code, argc, i, arg_len, current_len
  
  ! Removed fork-bomb prevention marker cleanup
  
  ! Get command line arguments (excluding argv(0) which contains executable path)
  ! command_argument_count() returns number of user arguments (not including argv(0))
  argc = command_argument_count()
  
  if (argc > 0) then
    ! Find maximum argument length for dynamic allocation
    arg_len = 0
    do i = 1, argc
      call get_command_argument(i, length=current_len)
      arg_len = max(arg_len, current_len)
    end do
    
    ! Allocate with dynamic length based on actual arguments
    allocate(character(len=arg_len) :: args(argc))
    
    ! get_command_argument(i, ...) with i=1,2,... gets user arguments (not argv(0))
    do i = 1, argc
      call get_command_argument(i, args(i))
    end do
  else
    allocate(character(len=0) :: args(0))
  end if
  
  ! Ensure error_message is allocated with sufficient length for error reporting
  allocate(character(len=1024) :: error_message)
  
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
    call exit_invalid_config_clean()
  end if

  ! Apply enhanced auto-discovery integration for zero-configuration mode (Issue #281)
  if (config%zero_configuration_mode) then
    call enhance_zero_config_with_auto_discovery(config, enhancement_success, enhancement_error)
    if (.not. enhancement_success .and. .not. config%quiet) then
      print *, "WARNING: Auto-discovery enhancement failed: " // trim(enhancement_error)
      print *, "   Continuing with basic zero-configuration mode"
    end if
  end if
  
  ! Check for help/version/validate-config flags after successful parsing
  if (config%show_help) then
    call show_help()
    ! In quiet mode, help/version should exit with failure code
    if (config%quiet) then
      call exit_failure_clean()
    else
      call exit_success_clean()
    end if
  else if (config%show_version) then
    call show_version()
    ! In quiet mode, help/version should exit with failure code
    if (config%quiet) then
      call exit_failure_clean()
    else
      call exit_success_clean()
    end if
  else if (config%validate_config_only) then
    ! Only validate configuration, don't run analysis
    call validate_config_with_context(config, error_ctx)
    if (error_ctx%error_code /= ERROR_SUCCESS) then
      print *, "Configuration validation failed: " // trim(error_ctx%message)
      call exit_invalid_config_clean()
    else
      print *, "Configuration is valid"
      call exit_success_clean()
    end if
  else if (config%validate_architecture) then
    ! Only validate architectural size compliance, don't run coverage analysis
    call handle_architectural_validation(config, error_ctx)
    if (error_ctx%error_code /= ERROR_SUCCESS) then
      call exit_failure_clean()
    else
      call exit_success_clean()
    end if
  end if
  
  ! Removed fork-bomb prevention early exit
  
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
    print *, "   * See example: cat fortcov.nml.example"
    print *, "   * Documentation: https://github.com/lazy-fortran/fortcov"
    ! EPIC 1 FIX: Map data availability errors to proper exit codes
    if (index(error_ctx%message, "Source path not found") > 0 .or. &
        index(error_ctx%message, "Coverage file not found") > 0) then
      call exit_no_coverage_data_clean()
    ! ZERO-CONFIG EXIT CODE FIX: Issue #1061 - Zero-config mode should return exit code 3
    else if (index(error_ctx%message, "No input sources specified") > 0 .and. &
             config%zero_configuration_mode) then
      call exit_no_coverage_data_clean()
    else
      call exit_invalid_config_clean()
    end if
  end if
  if (config%zero_configuration_mode) then
    ! Run coverage analysis - use complete auto-workflow in zero-configuration mode
    call execute_zero_config_complete_workflow(config, exit_code)
  else
    exit_code = run_coverage_analysis(config)
  end if
  
  if (exit_code == EXIT_SUCCESS) then
    call exit_success_clean()
  else if (exit_code == EXIT_THRESHOLD_NOT_MET) then
    call exit_threshold_not_met_clean()
  else if (exit_code == EXIT_NO_COVERAGE_DATA) then
    call exit_no_coverage_data_clean()
  else if (exit_code == EXIT_INVALID_CONFIG) then
    call exit_invalid_config_clean()
  else if (exit_code == EXIT_FILE_ACCESS_ERROR) then
    call exit_file_access_error_clean()
  else if (exit_code == EXIT_MEMORY_ERROR) then
    call exit_memory_error_clean()
  else if (exit_code == EXIT_VALIDATION_ERROR) then
    call exit_validation_error_clean()
  else
    call exit_failure_clean()
  end if

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
    
    ! Display results - output format depends on architecture_output_format
    if (.not. config%quiet) then
      if (trim(config%architecture_output_format) == "json") then
        ! For JSON format, only output the detailed report which contains JSON
        if (len_trim(enforcement_result%detailed_report) > 0) then
          print '(A)', trim(enforcement_result%detailed_report)
        end if
      else
        ! For human and CI formats, output the formatted messages
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
    end if
    
    ! CRITICAL FIX: Use enforcement_result%exit_code directly instead of manual logic
    ! This properly integrates size_enforcement_core exit codes with main.f90 error handling
    if (enforcement_result%exit_code /= 0) then  ! CI_SUCCESS = 0, any non-zero is an error
      ! Map size_enforcement_core exit codes to application error codes
      if (enforcement_result%exit_code == 1) then  ! CI_SUCCESS_WITH_WARNINGS
        ! Any non-success error code triggers exit_failure_clean() -> EXIT_FAILURE = 1
        error_ctx%error_code = ERROR_THRESHOLD_NOT_MET
        error_ctx%message = trim(enforcement_result%summary_message)
      else if (enforcement_result%exit_code == 2) then  ! CI_FAILURE_VIOLATIONS
        error_ctx%error_code = ERROR_INVALID_CONFIG
        error_ctx%message = "Architectural size violations detected"
      else  ! CI_FAILURE_ERROR = 3 or other errors
        error_ctx%error_code = ERROR_INVALID_CONFIG
        error_ctx%message = "Architectural size validation failed: " // &
                           trim(enforcement_result%summary_message)
      end if
    end if
    
  end subroutine handle_architectural_validation

end program main
