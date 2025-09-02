module zero_config_core
    use config_types, only: config_t
    use build_detector_core, only: build_system_info_t
    use build_system_validation, only: detect_and_validate_build_system
    use coverage_test_executor, only: execute_auto_test_workflow
    use zero_config_manager
    use error_handling_core, only: error_context_t, ERROR_SUCCESS, clear_error_context
    use constants_core, only: EXIT_SUCCESS, EXIT_FAILURE, EXIT_NO_COVERAGE_DATA, &
                              EXIT_INVALID_CONFIG
    use fortcov_core, only: run_coverage_analysis
    implicit none
    private
    
    ! Public procedures
    public :: enhance_zero_config_with_auto_discovery
    public :: configure_auto_discovery_defaults
    public :: integrate_build_system_detection
    public :: setup_auto_test_execution
    public :: provide_zero_config_user_feedback
    public :: execute_zero_config_complete_workflow
    
    ! Internal helper to detect self-invocation within FortCov repo
    interface
    end interface
    
    ! Constants for default configuration
    integer, parameter :: DEFAULT_TEST_TIMEOUT = 300  ! 5 minutes
    integer, parameter :: DEFAULT_MAX_FILES = 1000
    
contains

    subroutine enhance_zero_config_with_auto_discovery(config, success, error_message)
        !! Main entry point for zero-configuration mode enhancement
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message
        
        type(error_context_t) :: error_ctx
        
        call clear_error_context(error_ctx)
        success = .false.
        error_message = ""
        
        ! Apply zero configuration defaults if not already set
        if (.not. allocated(config%output_path) .or. &
            len_trim(config%output_path) == 0) then
            block
                character(len=:), allocatable :: default_output_path
                character(len=:), allocatable :: default_output_format
                character(len=:), allocatable :: default_input_format
                character(len=:), allocatable :: default_exclude_patterns(:)
                
                call apply_zero_configuration_defaults(default_output_path, &
                                                      default_output_format, &
                                                      default_input_format, &
                                                      default_exclude_patterns)
                
                ! Apply defaults to config
                if (.not. allocated(config%output_path)) then
                    config%output_path = default_output_path
                end if
                if (.not. allocated(config%output_format)) then
                    config%output_format = default_output_format
                end if
                if (.not. allocated(config%input_format)) then
                    config%input_format = default_input_format
                end if
                if (.not. allocated(config%exclude_patterns)) then
                    config%exclude_patterns = default_exclude_patterns
                end if
            end block
        end if
        
        ! Configure auto-discovery
        call configure_auto_discovery_defaults(config)
        
        ! Integrate build system detection
        call integrate_build_system_detection(config, success, error_message)
        if (.not. success) return
        
        success = .true.
    end subroutine enhance_zero_config_with_auto_discovery

    subroutine configure_auto_discovery_defaults(config)
        !! Configure auto-discovery default settings
        type(config_t), intent(inout) :: config
        
        ! Set reasonable defaults for auto-discovery
        config%auto_discovery = .true.
        config%test_timeout_seconds = DEFAULT_TEST_TIMEOUT
        config%max_files = DEFAULT_MAX_FILES
    end subroutine configure_auto_discovery_defaults

    subroutine integrate_build_system_detection(config, success, error_message)
        !! Integrate build system detection into configuration
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message
        
        type(build_system_info_t) :: build_info
        type(error_context_t) :: error_ctx
        
        call clear_error_context(error_ctx)
        success = .false.
        error_message = ""
        
        ! Detect and validate build system via unified helper
        if (detect_and_validate_build_system(config, build_info, error_ctx, '.') /= EXIT_SUCCESS) then
            error_message = "Build system detection failed: " // trim(error_ctx%message)
            return
        end if
        
        ! Build system detected - configuration can use the build_info
        ! for test commands
        ! Note: config_t doesn't have build_command field,
        ! build_info provides test commands
        
        success = .true.
    end subroutine integrate_build_system_detection

    subroutine setup_auto_test_execution(config, success, error_message)
        !! Setup automatic test execution configuration
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message
        
        success = .true.
        error_message = ""
        
        ! Configure test execution settings
        ! Enable automatic test execution if not explicitly disabled
        config%auto_test_execution = .true.
        config%test_timeout_seconds = DEFAULT_TEST_TIMEOUT
    end subroutine setup_auto_test_execution

    subroutine provide_zero_config_user_feedback(config, build_info)
        !! Provide user feedback for zero-configuration mode
        type(config_t), intent(in) :: config
        type(build_system_info_t), intent(in) :: build_info
        
        if (config%verbose) then
            print '(A)', "Zero-configuration mode enabled"
            print '(A)', "Detected build system: " // trim(build_info%system_type)
            if (len_trim(build_info%test_command) > 0) then
                print '(A)', "Using test command: " // trim(build_info%test_command)
            end if
        end if
    end subroutine provide_zero_config_user_feedback

    subroutine execute_zero_config_complete_workflow(config, exit_code)
        !! Execute complete zero-configuration workflow
        type(config_t), intent(inout) :: config
        integer, intent(out) :: exit_code
        
        logical :: success
        character(len=:), allocatable :: error_message
        type(error_context_t) :: error_ctx
        
        ! Removed fork-bomb prevention recursion marker check
        
        if (.not. config%quiet) then
            print '(A)', "FortCov: Starting zero-configuration coverage analysis..."
        end if
        
        exit_code = EXIT_FAILURE
        
        ! Enhance configuration with auto-discovery
        call enhance_zero_config_with_auto_discovery(config, success, error_message)
        if (.not. success) then
            call show_zero_configuration_error_guidance()
            exit_code = EXIT_INVALID_CONFIG  ! Exit code 4 for configuration issues
            return
        end if
        
        ! Execute auto test workflow if enabled (non-blocking in zero-config mode)
        if (config%auto_test_execution) then
            exit_code = execute_auto_test_workflow(config)
            if (exit_code /= 0) then
                if (.not. config%quiet) then
                    print '(A)', &
                        "FortCov: Auto-test execution failed, proceeding " // &
                        "with existing coverage files..."
                end if
                ! Continue with existing coverage files instead of failing
                exit_code = EXIT_SUCCESS  
            end if
        end if
        
        ! Ensure output directory structure
        call ensure_output_directory_structure(config%output_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call show_zero_configuration_error_guidance()
            exit_code = EXIT_INVALID_CONFIG  ! Exit code 4 for directory access issues
            return
        end if
        
        ! Auto-discover coverage files and source paths
        call populate_zero_config_with_discovered_files(config, success, error_message)
        if (.not. success) then
            if (.not. config%quiet) then
                print '(A)', "FortCov: " // trim(error_message)
            end if
            call show_zero_configuration_error_guidance()
            ! CRITICAL FIX: Return proper exit code based on the specific error
            if (index(error_message, "No coverage files found") > 0) then
                exit_code = EXIT_NO_COVERAGE_DATA  ! Exit code 3
            else
                exit_code = EXIT_FAILURE  ! Exit code 1 for other errors
            end if
            return
        end if
        
        ! Execute the actual coverage analysis
        exit_code = run_coverage_analysis(config)
        
        if (.not. config%quiet) then
            if (exit_code == EXIT_SUCCESS) then
                print '(A)', &
                    "FortCov: Zero-configuration coverage analysis completed " // &
                    "successfully!"
            else
                print '(A)', "FortCov: Zero-configuration coverage analysis failed."
            end if
        end if
    end subroutine execute_zero_config_complete_workflow

    ! Self-repo detection removed: auto-tests should run unless disabled with --no-auto-test

    subroutine populate_zero_config_with_discovered_files(config, success, &
                                                         error_message)
        !! Auto-discover and populate configuration with coverage files and source paths
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message
        
        character(len=:), allocatable :: discovered_coverage_files(:)
        character(len=:), allocatable :: discovered_source_paths(:)
        
        success = .false.
        error_message = ""
        
        ! Auto-discover coverage files using priority search
        if (.not. config%quiet) then
            print '(A)', "FortCov: Auto-discovering coverage files..."
        end if
        
        discovered_coverage_files = auto_discover_coverage_files_priority()
        
        if (.not. config%quiet) then
            if (allocated(discovered_coverage_files)) then
                print '(A,I0,A)', "FortCov: Discovery returned ", &
                                   size(discovered_coverage_files), " files"
            else
                print '(A)', "FortCov: Discovery returned no allocated array"
            end if
        end if
        
        if (.not. allocated(discovered_coverage_files) .or. &
            size(discovered_coverage_files) == 0) then
            error_message = "No coverage files found in standard locations"
            return
        end if
        
        ! Auto-discover source paths
        discovered_source_paths = auto_discover_source_files_priority()
        
        ! Populate configuration with discovered files
        if (allocated(config%coverage_files)) deallocate(config%coverage_files)
        config%coverage_files = discovered_coverage_files
        
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        config%source_paths = discovered_source_paths
        
        if (.not. config%quiet) then
            print '(A,I0,A)', "FortCov: Found ", &
                              size(discovered_coverage_files), " coverage files"
            print '(A,I0,A)', "FortCov: Using ", &
                              size(discovered_source_paths), " source paths"
        end if
        
        success = .true.
        
    end subroutine populate_zero_config_with_discovered_files

end module zero_config_core
