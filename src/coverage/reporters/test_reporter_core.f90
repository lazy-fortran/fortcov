module test_reporter_core
    !! Test Workflow Reporting
    !! 
    !! Extracted from coverage_test_executor.f90 for SRP compliance (Issue #718).
    !! Handles all user-facing reporting for test workflow operations.
    use config_core, only: config_t
    use build_detector_core, only: build_system_info_t
    use error_handling_core, only: error_context_t
    use string_utils, only: int_to_string
    use test_executor_core, only: format_timeout_message
    implicit none
    private
    
    public :: report_auto_test_disabled
    public :: report_workflow_start
    public :: report_build_detection_failed
    public :: report_unknown_build_system
    public :: report_build_tool_unavailable
    public :: report_build_system_detected
    public :: report_test_failure
    public :: report_test_success
    ! fork-bomb prevention reporting removed
    
contains
    
    subroutine report_auto_test_disabled(config)
        !! Report that auto-test execution is disabled
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "Note: Auto-test execution disabled"
        end if
    end subroutine report_auto_test_disabled
    
    subroutine report_workflow_start(config)
        !! Report start of auto test workflow
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "Executing automatic test workflow..."
        end if
    end subroutine report_workflow_start
    
    subroutine report_build_detection_failed(config, error_ctx)
        !! Report build system detection failure
        type(config_t), intent(in) :: config
        type(error_context_t), intent(in) :: error_ctx
        
        if (.not. config%quiet) then
            print *, "Error: Build system detection failed: " // &
                     trim(error_ctx%message)
        end if
    end subroutine report_build_detection_failed
    
    subroutine report_unknown_build_system(config)
        !! Report unknown build system detected
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "Warning: No known build system detected, skipping tests"
            print *, "   Supported: FPM, CMake, Make, Meson"
        end if
    end subroutine report_unknown_build_system
    
    subroutine report_build_tool_unavailable(config, build_info)
        !! Report build tool not available
        type(config_t), intent(in) :: config
        type(build_system_info_t), intent(in) :: build_info
        
        if (.not. config%quiet) then
            print *, "Warning: Build tool not available for " // &
                     trim(build_info%system_type) // ", skipping tests"
        end if
    end subroutine report_build_tool_unavailable
    
    subroutine report_build_system_detected(config, build_info)
        !! Report successful build system detection
        type(config_t), intent(in) :: config
        type(build_system_info_t), intent(in) :: build_info
        
        if (.not. config%quiet) then
            print *, "Build system detected: " // trim(build_info%system_type)
        end if
    end subroutine report_build_system_detected
    
    subroutine report_test_failure(config, test_exit_code)
        !! Report test execution failure
        type(config_t), intent(in) :: config
        integer, intent(in) :: test_exit_code
        
        if (.not. config%quiet) then
            if (test_exit_code == 124) then
                print *, "Test execution timed out after " // &
                         format_timeout_message(config%test_timeout_seconds)
            else
                print *, "Error: Test execution failed with exit code: " // &
                         int_to_string(test_exit_code)
            end if
        end if
    end subroutine report_test_failure
    
    subroutine report_test_success(config)
        !! Report test execution success
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "Tests completed successfully"
        end if
    end subroutine report_test_success
    
end module test_reporter_core
