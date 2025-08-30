module test_result_reporting
    !! Test Result Reporting and Status Management
    !! 
    !! Handles reporting of test execution results and status messages.
    !! Extracted from coverage_test_executor.f90 for SRP compliance (Issue #718).
    use constants_core, only: EXIT_SUCCESS
    use config_core, only: config_t
    use string_utils, only: int_to_string
    use test_execution_core, only: format_timeout_message
    implicit none
    private
    
    public :: handle_test_execution_results
    public :: report_auto_test_disabled, report_workflow_start
    public :: report_test_failure, report_test_success

contains

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
    
    subroutine report_auto_test_disabled(config)
        !! Report that auto-test execution is disabled
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "ℹ️  Auto-test execution disabled"
        end if
    end subroutine report_auto_test_disabled
    
    subroutine report_workflow_start(config)
        !! Report start of auto test workflow
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "🚀 Executing automatic test workflow..."
        end if
    end subroutine report_workflow_start
    
    subroutine report_test_failure(config, test_exit_code)
        !! Report test execution failure
        type(config_t), intent(in) :: config
        integer, intent(in) :: test_exit_code
        
        if (.not. config%quiet) then
            if (test_exit_code == 124) then
                print *, "⏱️  Test execution timed out after " // &
                         format_timeout_message(config%test_timeout_seconds)
            else
                print *, "❌ Test execution failed with exit code: " // &
                         int_to_string(test_exit_code)
            end if
        end if
    end subroutine report_test_failure
    
    subroutine report_test_success(config)
        !! Report test execution success
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "✅ Tests completed successfully"
        end if
    end subroutine report_test_success

end module test_result_reporting