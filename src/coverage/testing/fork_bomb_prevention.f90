module fork_bomb_prevention
    !! Fork Bomb Prevention for Test Execution
    !! 
    !! Handles detection of test execution environments to prevent infinite recursion.
    !! Extracted from coverage_test_executor.f90 for SRP compliance (Issue #718).
    ! Note: reporting handled by coverage_test_executor via test_reporter_core
    use file_ops_secure, only: safe_remove_file
    use error_handling_core, only: error_context_t
    implicit none
    private
    
    public :: is_running_in_test_environment
    public :: prepare_for_auto_test_execution, cleanup_recursion_marker

contains

    function is_running_in_test_environment() result(in_test_env)
        !! Detect if fortcov is running within a test execution environment
        !! 
        !! This prevents infinite recursion fork bombs when test suites call fortcov.
        !! Uses multiple detection strategies including recursion marker files.
        logical :: in_test_env
        
        in_test_env = .false.
        
        ! CRITICAL: Check for recursion marker first
        if (check_recursion_marker()) then
            in_test_env = .true.
            return
        end if
        
        ! Check standard test environment indicators
        if (check_standard_test_indicators()) then
            in_test_env = .true.
            return
        end if
        
        ! Check process parent command line (if accessible)
        call check_process_ancestry(in_test_env)
        
    end function is_running_in_test_environment
    
    subroutine prepare_for_auto_test_execution()
        !! Create recursion marker before auto-test execution
        call create_recursion_marker()
    end subroutine prepare_for_auto_test_execution
    
    function check_recursion_marker() result(marker_exists)
        !! Check if recursion marker file exists
        logical :: marker_exists
        
        ! Check for marker file that indicates we're already in a fortcov execution
        inquire(file='.fortcov_execution_marker', exist=marker_exists)
        
    end function check_recursion_marker
    
    subroutine create_recursion_marker()
        !! Create marker file to prevent recursion
        integer :: unit_num
        
        ! Create temporary marker file
        open(newunit=unit_num, file='.fortcov_execution_marker', &
             status='replace', action='write')
        write(unit_num, '(A)') 'FORTCOV_AUTO_TEST_EXECUTION_IN_PROGRESS'
        close(unit_num)
        
    end subroutine create_recursion_marker
    
    subroutine cleanup_recursion_marker()
        !! Clean up recursion marker file
        !! SECURITY FIX Issue #963: Use secure file removal instead of execute_command_line
        type(error_context_t) :: error_ctx
        
        ! Use secure file removal instead of shell command
        call safe_remove_file('.fortcov_execution_marker', error_ctx)
        ! Note: Ignore errors - marker file may not exist, which is fine
        
    end subroutine cleanup_recursion_marker
    
    function check_standard_test_indicators() result(in_test_env)
        !! Check standard test environment indicators
        logical :: in_test_env
        character(len=1024) :: env_value
        integer :: stat
        
        in_test_env = .false.
        
        ! Check for FPM test environment variables
        call get_environment_variable('FPM_TEST', env_value, status=stat)
        if (stat == 0 .and. len_trim(env_value) > 0) then
            in_test_env = .true.
            return
        end if
        
        ! Check for CMake/CTest environment
        call get_environment_variable('CTEST_BINARY_DIRECTORY', env_value, status=stat)
        if (stat == 0 .and. len_trim(env_value) > 0) then
            in_test_env = .true.
            return
        end if
        
        call get_environment_variable('CMAKE_BINARY_DIR', env_value, status=stat)
        if (stat == 0 .and. len_trim(env_value) > 0) then
            in_test_env = .true.
            return
        end if
        
        ! Check for generic test environment indicators
        call get_environment_variable('CI', env_value, status=stat)
        if (stat == 0 .and. (trim(env_value) == 'true' .or. trim(env_value) == '1')) then
            ! In CI environment, be more conservative about auto-test execution
            in_test_env = .true.
            return
        end if
        
        ! Check if current process was started by a test runner
        call get_environment_variable('_', env_value, status=stat)
        if (stat == 0) then
            if (index(env_value, 'fpm') > 0 .and. index(env_value, 'test') > 0) then
                in_test_env = .true.
                return
            end if
            if (index(env_value, 'ctest') > 0 .or. index(env_value, 'make') > 0) then
                in_test_env = .true.
                return
            end if
        end if
        
    end function check_standard_test_indicators
    
    subroutine check_process_ancestry(in_test_env)
        !! Check if any parent process indicates test environment
        logical, intent(out) :: in_test_env
        
        in_test_env = .false.
        
        ! Simplified approach: skip complex process checking for now
        ! Focus on environment variables and file-based detection
        ! which are more reliable and portable
        
        ! Fallback: Check if we're being run from a build directory
        ! (Heuristic: presence of common build artifacts)
        call check_build_directory_context(in_test_env)
        
    end subroutine check_process_ancestry
    
    subroutine check_build_directory_context(in_test_env)
        !! Check if we're running from a build/test context
        logical, intent(out) :: in_test_env
        
        logical :: file_exists
        
        in_test_env = .false.
        
        ! Check for common test/build artifacts that suggest we're in test execution
        inquire(file='CMakeCache.txt', exist=file_exists)
        if (file_exists) then
            in_test_env = .true.
            return
        end if
        
        inquire(file='build/CMakeCache.txt', exist=file_exists)
        if (file_exists) then
            in_test_env = .true.
            return
        end if
        
        inquire(file='Testing/Temporary', exist=file_exists)
        if (file_exists) then
            in_test_env = .true.
            return
        end if
        
    end subroutine check_build_directory_context
    
    ! Reporting is provided by test_reporter_core to avoid duplication

end module fork_bomb_prevention
