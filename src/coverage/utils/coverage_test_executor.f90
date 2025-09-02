module coverage_test_executor
    !! Auto-test workflow orchestrator - refactored for SRP compliance
    !! 
    !! Coordinates automatic test execution by delegating to specialized modules.
    !! Decomposed from 471 lines for Issue #718 size management.
    use constants_core, only: EXIT_SUCCESS, EXIT_FAILURE
    use config_core, only: config_t
    use build_detector_core, only: build_system_info_t
    use error_handling_core, only: error_context_t, ERROR_SUCCESS
    use build_system_validation, only: detect_and_validate_build_system
    use test_executor_core
    use test_environment_detector
    use test_reporter_core
    implicit none
    private

    public :: execute_auto_test_workflow

contains

    function execute_auto_test_workflow(config) result(exit_code)
        !! Execute automatic test workflow - orchestrates specialized modules
        !!
        !! Coordinates build system detection, test execution, and reporting.
        !! Delegated to specialized modules for SRP compliance.
        !!
        !! Args:
        !!   config: Configuration object with auto_test_execution settings
        !!
        !! Returns:
        !!   exit_code: 0 for success, non-zero for various failure conditions
        
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        type(build_system_info_t) :: build_info
        type(error_context_t) :: error_ctx
        integer :: test_exit_code
        logical :: execution_success
        
        
        exit_code = EXIT_SUCCESS
        
        ! CRITICAL: Fork bomb prevention
        if (is_running_in_test_environment()) then
            call report_fork_bomb_prevention(config)
            return
        end if
        
        ! Skip if disabled
        if (.not. config%auto_test_execution) then
            call report_auto_test_disabled(config)
            return
        end if
        
        call report_workflow_start(config)
        
        ! Detect and validate build system
        exit_code = detect_and_validate_build_system(config, build_info, error_ctx, '.')
        if (exit_code /= EXIT_SUCCESS) return
        
        ! Check if build system is usable
        if (build_info%system_type == 'unknown' .or. &
            .not. build_info%tool_available) then
            return
        end if
        
        ! Execute auto-test with guaranteed cleanup
        block
            logical :: marker_created
            
            marker_created = .false.
            call prepare_for_auto_test_execution()
            marker_created = .true.
            
            call execute_tests_with_timeout(build_info%test_command, config, &
                                           test_exit_code, execution_success)
            
            exit_code = handle_test_execution_results(config, test_exit_code, &
                                                    execution_success)
            
            if (marker_created) then
                call cleanup_recursion_marker()
            end if
            
        end block
        
    end function execute_auto_test_workflow
    
    ! Note: build system detection/validation unified in build_system_validation
    
    ! Helper function for handling test execution results
    function handle_test_execution_results(config, test_exit_code, execution_success) result(exit_code)
        !! Handle test execution results and report outcome
        type(config_t), intent(in) :: config
        integer, intent(in) :: test_exit_code
        logical, intent(in) :: execution_success
        integer :: exit_code
        
        if (.not. execution_success) then
            exit_code = test_exit_code
            call report_test_failure(config, test_exit_code)
        else
            exit_code = EXIT_SUCCESS
            call report_test_success(config)
        end if
    end function handle_test_execution_results

end module coverage_test_executor
