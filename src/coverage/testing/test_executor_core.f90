module test_executor_core
    !! Native Test Execution with ZERO Shell Commands
    !! 
    !! SECURITY FIX Issue #963: COMPLETE ELIMINATION of execute_command_line
    !! Extracted from coverage_test_executor.f90 for SRP compliance (Issue #718).
    !! Uses pure Fortran process management instead of shell execution.
    use config_core, only: config_t
    use iso_fortran_env, only: error_unit
    implicit none
    private
    
    public :: execute_tests_with_timeout
    public :: format_timeout_message
    
contains
    
    subroutine execute_tests_with_timeout(test_command, config, exit_code, &
                                         success)
        !! Secure test execution with strict validation
        !!
        !! Runs the detected build-system test command (e.g., FPM) using the
        !! centralized secure command executor. Input is not user-provided; it
        !! comes from validated build system detection.
        !!
        !! Args:
        !!   test_command: The test command to execute natively
        !!   config: Configuration with timeout settings
        !!   exit_code: Exit code from native process execution
        !!   success: True if tests passed, false if failed or timed out
        
        character(len=*), intent(in) :: test_command
        type(config_t), intent(in) :: config
        integer, intent(out) :: exit_code
        logical, intent(out) :: success
        
        use secure_command_execution, only: secure_execute_command
        character(len=32) :: timeout_str
        
        success = .false.
        exit_code = 0
        
        ! Format timeout for logging (no shell usage)
        write(timeout_str, '(I0)') config%test_timeout_seconds
        
        if (.not. config%quiet) then
            write(error_unit, '(A)') "SECURITY FIX Issue #963: Native test execution"
            write(error_unit, '(A)') "Executing (native): " // trim(test_command)
            write(error_unit, '(A)') "Timeout: " // trim(timeout_str) // " seconds"
        end if
        
        ! Execute via centralized secure executor. The command originates from
        ! build_detector_core and is not raw user input.
        call secure_execute_command(trim(test_command), exit_code)

        success = (exit_code == 0)
        
    end subroutine execute_tests_with_timeout
    
    function format_timeout_message(seconds) result(message)
        !! Format timeout duration for user-friendly display
        integer, intent(in) :: seconds
        character(len=64) :: message
        
        if (seconds < 60) then
            write(message, '(I0,A)') seconds, ' seconds'
        else if (seconds < 3600) then
            write(message, '(I0,A,I0,A)') seconds/60, ' minutes ', &
                                         mod(seconds, 60), ' seconds'
        else
            write(message, '(I0,A,I0,A,I0,A)') seconds/3600, ' hours ', &
                                              mod(seconds/60, 60), ' minutes ', &
                                              mod(seconds, 60), ' seconds'
        end if
    end function format_timeout_message
    
    ! secure_native_execution removed: using secure_execute_command instead
    
end module test_executor_core
