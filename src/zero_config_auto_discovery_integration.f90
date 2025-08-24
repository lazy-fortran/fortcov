module zero_config_auto_discovery_integration
    !! Zero-Configuration Mode Auto-Discovery Integration (Issue #281)
    !!
    !! Integrates all auto-discovery components into zero-configuration mode
    !! for seamless user experience. Builds on completed dependencies:
    !! - Issue #278: Build system detection (build_system_detector.f90)
    !! - Issue #279: Auto-test execution (coverage_workflows.f90) 
    !! - Issue #280: Configuration options (auto_discovery flags)
    !!
    !! This module provides the main integration layer that ties together:
    !! - Build system auto-detection
    !! - Automatic test execution with timeout handling
    !! - Coverage file auto-discovery with priority ordering
    !! - Graceful fallback and error handling
    !! - User feedback and configuration reporting
    !!
    !! The integration maintains backward compatibility while providing
    !! a seamless zero-configuration experience for new users.
    
    use config_types, only: config_t
    use build_system_detector, only: build_system_info_t, detect_build_system
    use coverage_workflows, only: execute_auto_test_workflow
    use test_build_gcov_auto_discovery, only: execute_complete_auto_workflow, &
                                              complete_workflow_result_t
    use zero_configuration_manager, only: apply_zero_configuration_defaults, &
                                         ensure_output_directory_structure, &
                                         show_zero_configuration_error_guidance
    use error_handling, only: error_context_t, ERROR_SUCCESS, clear_error_context
    use foundation_constants, only: EXIT_SUCCESS, EXIT_FAILURE
    implicit none
    private
    
    ! Public interface for zero-configuration enhancement
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

    subroutine enhance_zero_config_with_auto_discovery(config, success, &
                                                      error_message)
        !! Main entry point for zero-configuration mode enhancement
        !!
        !! Integrates all auto-discovery components to provide seamless
        !! zero-configuration experience. Detects build systems, configures
        !! auto-test execution, sets up coverage discovery, and provides
        !! graceful fallback behavior.
        !!
        !! Args:
        !!   config: Configuration object to enhance
        !!   success: True if enhancement succeeded
        !!   error_message: Error details if enhancement failed
        
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        type(build_system_info_t) :: build_info
        type(error_context_t) :: error_ctx
        logical :: build_integration_success
        
        success = .true.
        error_message = ""
        
        ! Step 1: Apply auto-discovery defaults
        call configure_auto_discovery_defaults(config)
        
        ! Step 2: Apply zero-configuration defaults if needed
        call apply_zero_config_defaults_if_needed(config)
        
        ! Step 3: Integrate build system detection
        call integrate_build_system_detection(config, build_info, &
                                             build_integration_success, &
                                             error_message)
        
        if (.not. build_integration_success) then
            ! Build system integration failed, but continue with degraded mode
            call handle_build_system_failure(config, error_message)
        end if
        
        ! Step 4: Setup auto-test execution configuration
        call setup_auto_test_execution(config, build_info)
        
        ! Step 5: Ensure output directory structure exists
        call ensure_output_directory_structure(config%output_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            success = .false.
            error_message = trim(error_ctx%message)
            return
        end if
        
        ! Step 6: Provide user feedback about configuration
        call provide_zero_config_user_feedback(config, build_info)
        
    end subroutine enhance_zero_config_with_auto_discovery
    
    subroutine configure_auto_discovery_defaults(config)
        !! Configure auto-discovery default settings
        !!
        !! Sets up the configuration flags and defaults needed for
        !! auto-discovery functionality to work seamlessly.
        !!
        !! Args:
        !!   config: Configuration object to configure
        
        type(config_t), intent(inout) :: config
        
        ! Enable auto-discovery features
        config%auto_discovery = .true.
        config%zero_configuration_mode = .true.
        
        ! Configure auto-test execution with reasonable defaults
        if (.not. config%auto_test_execution) then
            config%auto_test_execution = .true.
        end if
        
        ! Set reasonable timeout if not already configured
        if (config%test_timeout_seconds <= 0) then
            config%test_timeout_seconds = DEFAULT_TEST_TIMEOUT
        end if
        
        ! Set file processing limits if not configured
        if (config%max_files <= 0) then
            config%max_files = DEFAULT_MAX_FILES
        end if
        
    end subroutine configure_auto_discovery_defaults
    
    subroutine integrate_build_system_detection(config, build_info, success, &
                                               error_message)
        !! Integrate build system detection into zero-configuration flow
        !!
        !! Detects the build system using the build_system_detector module
        !! and validates that the detected system is usable. Provides
        !! graceful handling when no build system is found.
        !!
        !! Args:
        !!   config: Configuration object
        !!   build_info: Populated with build system information
        !!   success: True if integration succeeded
        !!   error_message: Error details if integration failed
        
        type(config_t), intent(in) :: config
        type(build_system_info_t), intent(out) :: build_info
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        type(error_context_t) :: error_ctx
        
        success = .true.
        error_message = ""
        
        ! Detect build system in current directory
        call detect_build_system('.', build_info, error_ctx)
        
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            success = .false.
            error_message = "Build system detection failed: " // &
                           trim(error_ctx%message)
            return
        end if
        
        ! Validate detected build system
        call validate_detected_build_system(build_info, success, error_message)
        
    end subroutine integrate_build_system_detection
    
    subroutine setup_auto_test_execution(config, build_info)
        !! Setup auto-test execution based on detected build system
        !!
        !! Configures auto-test execution settings based on the detected
        !! build system information. Adjusts timeout and execution strategy
        !! based on build system capabilities.
        !!
        !! Args:
        !!   config: Configuration object to setup
        !!   build_info: Build system information from detection
        
        type(config_t), intent(inout) :: config
        type(build_system_info_t), intent(in) :: build_info
        
        ! Configure auto-test execution based on build system
        if (build_info%system_type /= 'unknown' .and. &
            build_info%tool_available) then
            
            ! Build system available - enable auto-test execution
            config%auto_test_execution = .true.
            
            ! Adjust timeout based on build system
            call adjust_timeout_for_build_system(config, build_info%system_type)
            
        else
            ! No usable build system - disable auto-test execution
            ! but still allow coverage analysis
            config%auto_test_execution = .false.
        end if
        
    end subroutine setup_auto_test_execution
    
    subroutine provide_zero_config_user_feedback(config, build_info)
        !! Provide user feedback about zero-configuration setup
        !!
        !! Reports the auto-detected configuration to the user, including
        !! build system detection results, auto-test execution status,
        !! and output configuration.
        !!
        !! Args:
        !!   config: Configuration object
        !!   build_info: Build system information
        
        type(config_t), intent(in) :: config
        type(build_system_info_t), intent(in) :: build_info
        
        if (config%quiet) return
        
        print *, "🔧 Zero-configuration mode active"
        print *, ""
        
        ! Report build system detection
        call report_build_system_status(build_info)
        
        ! Report auto-test execution status
        call report_auto_test_status(config)
        
        ! Report output configuration
        call report_output_configuration(config)
        
        print *, ""
        
    end subroutine provide_zero_config_user_feedback
    
    ! Internal helper subroutines
    
    subroutine apply_zero_config_defaults_if_needed(config)
        !! Apply zero-configuration defaults only if not already configured
        type(config_t), intent(inout) :: config
        
        character(len=:), allocatable :: default_output_path
        character(len=:), allocatable :: default_output_format
        character(len=:), allocatable :: default_input_format
        character(len=:), allocatable :: default_exclude_patterns(:)
        
        ! Apply defaults only for unallocated/empty fields
        if (.not. allocated(config%output_path) .or. &
            len_trim(config%output_path) == 0) then
            
            call apply_zero_configuration_defaults(default_output_path, &
                                                  default_output_format, &
                                                  default_input_format, &
                                                  default_exclude_patterns)
            
            config%output_path = default_output_path
            
            if (.not. allocated(config%output_format) .or. &
                len_trim(config%output_format) == 0) then
                config%output_format = default_output_format
            end if
            
            if (.not. allocated(config%input_format) .or. &
                len_trim(config%input_format) == 0) then
                config%input_format = default_input_format
            end if
            
            ! Only apply exclude patterns if none exist
            if (.not. allocated(config%exclude_patterns)) then
                config%exclude_patterns = default_exclude_patterns
            end if
        end if
        
    end subroutine apply_zero_config_defaults_if_needed
    
    subroutine validate_detected_build_system(build_info, success, error_message)
        !! Validate that detected build system is usable
        type(build_system_info_t), intent(in) :: build_info
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        success = .true.
        error_message = ""
        
        if (build_info%system_type == 'unknown') then
            ! Unknown build system is okay - just no auto-test execution
            success = .true.
            error_message = "No known build system detected"
            return
        end if
        
        if (.not. build_info%tool_available) then
            success = .false.
            error_message = "Build tool not available for " // &
                           trim(build_info%system_type) // " build system"
            return
        end if
        
        if (len_trim(build_info%test_command) == 0) then
            success = .false.
            error_message = "No test command available for " // &
                           trim(build_info%system_type) // " build system"
            return
        end if
        
    end subroutine validate_detected_build_system
    
    subroutine handle_build_system_failure(config, error_message)
        !! Handle build system integration failure gracefully
        type(config_t), intent(inout) :: config
        character(len=*), intent(in) :: error_message
        
        ! Disable auto-test execution since build system failed
        config%auto_test_execution = .false.
        
        ! Still enable coverage auto-discovery
        config%auto_discovery = .true.
        
        if (.not. config%quiet) then
            print *, "⚠️  " // trim(error_message)
            print *, "   Continuing with coverage analysis only"
        end if
        
    end subroutine handle_build_system_failure
    
    subroutine adjust_timeout_for_build_system(config, system_type)
        !! Adjust test timeout based on build system characteristics
        type(config_t), intent(inout) :: config
        character(len=*), intent(in) :: system_type
        
        select case (trim(system_type))
        case ('fpm')
            ! FPM is generally fast
            if (config%test_timeout_seconds == DEFAULT_TEST_TIMEOUT) then
                config%test_timeout_seconds = 120  ! 2 minutes
            end if
        case ('cmake')
            ! CMake can be slower due to build step
            if (config%test_timeout_seconds == DEFAULT_TEST_TIMEOUT) then
                config%test_timeout_seconds = 600  ! 10 minutes
            end if
        case ('make')
            ! Make can be variable depending on complexity
            if (config%test_timeout_seconds == DEFAULT_TEST_TIMEOUT) then
                config%test_timeout_seconds = 300  ! 5 minutes (default)
            end if
        case ('meson')
            ! Meson is usually efficient
            if (config%test_timeout_seconds == DEFAULT_TEST_TIMEOUT) then
                config%test_timeout_seconds = 180  ! 3 minutes
            end if
        case default
            ! Keep default timeout
        end select
        
    end subroutine adjust_timeout_for_build_system
    
    subroutine report_build_system_status(build_info)
        !! Report build system detection status
        type(build_system_info_t), intent(in) :: build_info
        
        if (build_info%system_type == 'unknown') then
            print *, "📋 Build system: Not detected"
            print *, "   Auto-test execution: Disabled"
        else
            print *, "📋 Build system: " // trim(build_info%system_type)
            if (build_info%tool_available) then
                print *, "   Tool status: Available"
                print *, "   Test command: " // trim(build_info%test_command)
            else
                print *, "   Tool status: Not available"
                print *, "   Auto-test execution: Disabled"
            end if
        end if
        
    end subroutine report_build_system_status
    
    subroutine report_auto_test_status(config)
        !! Report auto-test execution configuration
        type(config_t), intent(in) :: config
        
        if (config%auto_test_execution) then
            print *, "🧪 Auto-test execution: Enabled"
            write(*, '(A,I0,A)') "   Timeout: ", config%test_timeout_seconds, " seconds"
        else
            print *, "🧪 Auto-test execution: Disabled"
        end if
        
    end subroutine report_auto_test_status
    
    subroutine report_output_configuration(config)
        !! Report output configuration
        type(config_t), intent(in) :: config
        
        print *, "📄 Output configuration:"
        if (allocated(config%output_path)) then
            print *, "   Path: " // trim(config%output_path)
        end if
        if (allocated(config%output_format)) then
            print *, "   Format: " // trim(config%output_format)
        end if
        write(*, '(A,I0)') "   Max files: ", config%max_files
        
    end subroutine report_output_configuration

    function execute_zero_config_complete_workflow(config) result(exit_code)
        !! Execute complete auto-discovery workflow in zero-configuration mode
        !!
        !! Integrates the complete auto-discovery workflow (Issue #277) with
        !! zero-configuration mode. This provides the seamless experience of
        !! running `fortcov` with no arguments and getting complete coverage
        !! analysis with auto-detected build system, auto-test execution,
        !! and auto-gcov processing.
        !!
        !! This is the main integration point that ties together:
        !! - Zero-configuration setup and defaults
        !! - Complete auto-discovery workflow
        !! - User feedback and error reporting
        !!
        !! Args:
        !!   config: Configuration object (enhanced with zero-config)
        !!
        !! Returns:
        !!   exit_code: 0 for success, non-zero for various failure conditions
        
        type(config_t), intent(in) :: config
        integer :: exit_code

        type(complete_workflow_result_t) :: workflow_result

        exit_code = EXIT_SUCCESS

        ! Execute the complete auto-discovery workflow
        call execute_complete_auto_workflow(config, workflow_result)

        ! Report workflow results to user
        call report_workflow_results(config, workflow_result)

        ! Set exit code based on workflow success
        if (.not. workflow_result%success) then
            if (workflow_result%timed_out) then
                exit_code = 124  ! Standard timeout exit code
            else
                exit_code = EXIT_FAILURE
            end if
        end if

    end function execute_zero_config_complete_workflow

    subroutine report_workflow_results(config, result)
        !! Report complete workflow results to user
        type(config_t), intent(in) :: config
        type(complete_workflow_result_t), intent(in) :: result

        if (config%quiet) return

        print *, "📊 Auto-discovery workflow results:"

        ! Report test execution results
        if (result%test_executed) then
            if (result%tests_passed) then
                print *, "   ✅ Tests: Passed"
            else if (result%timed_out) then
                print *, "   ⏱️  Tests: Timed out"
            else
                print *, "   ❌ Tests: Failed"
            end if
        else
            print *, "   ⏭️  Tests: Skipped (no build system or disabled)"
        end if

        ! Report gcov processing results
        if (result%used_manual_files) then
            print *, "   📁 Coverage files: Using manual specification"
        else if (result%gcov_processed) then
            print *, "   ✅ Coverage processing: Auto-discovered and processed"
        else
            print *, "   ❌ Coverage processing: No coverage data found"
        end if

        ! Report coverage generation
        if (result%coverage_generated) then
            print *, "   📈 Coverage report: Generated"
        else
            print *, "   ❌ Coverage report: Failed to generate"
        end if

        ! Report overall status
        if (result%success) then
            print *, "   🎉 Overall: Success"
        else
            print *, "   ❌ Overall: Failed"
            if (len_trim(result%error_message) > 0) then
                print *, "   💡 Error: " // trim(result%error_message)
            end if
        end if

        print *, ""

    end subroutine report_workflow_results

end module zero_config_auto_discovery_integration