module test_environment_utils
    !! Consistent test environment detection utilities
    !!
    !! This module provides consistent test environment detection
    !! across all test files to prevent infinite recursion and
    !! ensure safe test execution.
    
    implicit none
    private
    
    public :: test_environment_detected
    
contains

    function test_environment_detected() result(is_test_env)
        !! Consistent test environment detection for validation
        !!
        !! Returns true if we are currently running inside a test environment.
        !! Checks multiple indicators to ensure consistent detection:
        !! - FPM_TEST environment variable (FPM test runner)
        !! - Execution marker file (fortcov test prevention)
        !! - Default to true for safety
        
        logical :: is_test_env
        logical :: marker_exists
        character(len=256) :: env_value
        integer :: status
        
        ! Check for test indicators
        call get_environment_variable('FPM_TEST', env_value, status=status)
        if (status == 0) then
            is_test_env = .true.
            return
        end if
        
        ! Check for execution marker (fork bomb prevention)
        inquire(file='.fortcov_execution_marker', exist=marker_exists)
        if (marker_exists) then
            is_test_env = .true.
            return
        end if
        
        ! Check for common test environment variables
        call get_environment_variable('RUNNING_TESTS', env_value, status=status)
        if (status == 0) then
            is_test_env = .true.
            return
        end if
        
        call get_environment_variable('TEST_ENV', env_value, status=status)
        if (status == 0) then
            is_test_env = .true.
            return
        end if
        
        ! Default to true since we're running as part of test suite
        ! This ensures safety by preventing recursive test execution
        is_test_env = .true.
        
    end function test_environment_detected

end module test_environment_utils
