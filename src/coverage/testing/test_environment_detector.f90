module test_environment_detector
    !! Test Environment Detection and Fork Bomb Prevention
    !! 
    !! Extracted from coverage_test_executor.f90 for SRP compliance (Issue #718).
    !! Handles detection of test execution environments to prevent recursion.
    use file_ops_secure, only: safe_remove_file
    use error_handling_core, only: error_context_t
    implicit none
    private
    
    public :: is_running_in_test_environment
    public :: prepare_for_auto_test_execution
    public :: cleanup_recursion_marker
    
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
        logical :: file_exists
        
        ! SECURITY FIX Issue #963: Use secure file removal instead of execute_command_line
        block
            type(error_context_t) :: error_ctx
            call safe_remove_file('.fortcov_execution_marker', error_ctx)
            ! Note: Ignore errors - marker file may not exist, which is fine
        end block
        
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
        
        ! Check for test runner process names
        call check_build_directory_context(in_test_env)
        
    end function check_standard_test_indicators
    
    subroutine check_process_ancestry(in_test_env)
        !! Check process ancestry for test indicators (platform dependent)
        logical, intent(out) :: in_test_env
        
        ! This is a simplified check - in practice might use ps or proc filesystem
        ! For now, just check common parent process patterns
        in_test_env = .false.
        
        ! Future enhancement: could check /proc/ppid/cmdline on Linux
        ! or use system-specific process inspection
        
    end subroutine check_process_ancestry
    
    subroutine check_build_directory_context(in_test_env)
        !! Check if we're in a build directory context
        logical, intent(out) :: in_test_env
        logical :: exists
        
        in_test_env = .false.
        
        ! Check for build directory indicators
        inquire(file='CMakeCache.txt', exist=exists)
        if (exists) then
            in_test_env = .true.
            return
        end if
        
        inquire(file='Makefile', exist=exists)
        if (exists) then
            in_test_env = .true.
            return
        end if
        
        inquire(file='build.zig', exist=exists)
        if (exists) then
            in_test_env = .true.
            return
        end if
        
        ! Check for FPM build directory
        inquire(file='build', exist=exists)
        if (exists) then
            in_test_env = .true.
            return
        end if
        
    end subroutine check_build_directory_context
    
end module test_environment_detector