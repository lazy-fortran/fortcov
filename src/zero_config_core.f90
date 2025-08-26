module zero_config_core
    use config_types, only: config_t
    use build_system_detector, only: build_system_info_t, detect_build_system
    use coverage_auto_test_executor, only: execute_auto_test_workflow
    use zero_configuration_manager
    use error_handling, only: error_context_t, ERROR_SUCCESS, clear_error_context
    use foundation_constants, only: EXIT_SUCCESS, EXIT_FAILURE
    implicit none
    private
    
    ! Public procedures
    public :: enhance_zero_config_with_auto_discovery
    public :: configure_auto_discovery_defaults
    public :: integrate_build_system_detection
    public :: setup_auto_test_execution
    public :: provide_zero_config_user_feedback
    public :: execute_zero_config_complete_workflow
    
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
        if (.not. allocated(config%output_path) .or. len_trim(config%output_path) == 0) then
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
                if (.not. allocated(config%output_path)) config%output_path = default_output_path
                if (.not. allocated(config%output_format)) config%output_format = default_output_format
                if (.not. allocated(config%input_format)) config%input_format = default_input_format
                if (.not. allocated(config%exclude_patterns)) config%exclude_patterns = default_exclude_patterns
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
        
        ! Detect build system
        call detect_build_system(".", build_info, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            error_message = "Build system detection failed: " // trim(error_ctx%message)
            return
        end if
        
        ! Build system detected - configuration can use the build_info for test commands
        ! Note: config_t doesn't have build_command field, build_info provides test commands
        
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
        
        exit_code = EXIT_FAILURE
        
        ! Enhance configuration with auto-discovery
        call enhance_zero_config_with_auto_discovery(config, success, error_message)
        if (.not. success) then
            call show_zero_configuration_error_guidance()
            return
        end if
        
        ! Execute auto test workflow if enabled
        if (config%auto_test_execution) then
            exit_code = execute_auto_test_workflow(config)
            if (exit_code /= 0) then
                call show_zero_configuration_error_guidance()
                return
            end if
        end if
        
        ! Ensure output directory structure
        call ensure_output_directory_structure(config%output_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call show_zero_configuration_error_guidance()
            return
        end if
        
        exit_code = EXIT_SUCCESS
    end subroutine execute_zero_config_complete_workflow

end module zero_config_core