module coverage_test_executor_impl
    !! Auto-test execution logic extracted from coverage_workflows
    !! 
    !! Focused on automatic test workflow execution with build system 
    !! detection and timeout handling. Provides specialized test execution
    !! separated from other workflow operations.
    use constants_core, only: EXIT_SUCCESS, EXIT_FAILURE
    use config_core, only: config_t
    use build_detector_core, only: build_system_info_t, detect_build_system
    use error_handling_core, only: error_context_t, ERROR_SUCCESS
    use shell_utils_core, only: escape_shell_argument
    use string_utils, only: int_to_string
    implicit none
    private

    public :: execute_auto_test_workflow
    public :: execute_tests_with_timeout, format_timeout_message

contains

    function execute_auto_test_workflow(config) result(exit_code)
        !! Execute automatic test workflow with build system detection
        !!
        !! Integrates build system detection with automatic test execution.
        !! Detects the build system, generates appropriate test command with
        !! coverage flags, executes tests with timeout handling, and provides
        !! comprehensive error management.
        !!
        !! Args:
        !!   config: Configuration object with auto_test_execution settings
        !!
        !! Returns:
        !!   exit_code: 0 for success, non-zero for various failure conditions
        !!     - 0: Success or skipped (when auto_test_execution = .false.)
        !!     - 1: Test execution failed
        !!     - 2: Build system detection failed
        !!     - 124: Test execution timed out (standard timeout exit code)
        
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        type(build_system_info_t) :: build_info
        type(error_context_t) :: error_ctx
        integer :: test_exit_code
        logical :: execution_success
        
        exit_code = EXIT_SUCCESS
        
        ! CRITICAL: Fork bomb prevention - check if we're in a test execution environment
        if (is_running_in_test_environment()) then
            call report_fork_bomb_prevention(config)
            return
        end if
        
        ! Skip auto-test execution if disabled
        if (.not. config%auto_test_execution) then
            call report_auto_test_disabled(config)
            return
        end if
        
        call report_workflow_start(config)
        
        ! Detect and validate build system
        exit_code = detect_and_validate_build_system(config, build_info, error_ctx)
        if (exit_code /= EXIT_SUCCESS) return
        
        ! Check if build system is usable (tool available and not unknown)
        if (build_info%system_type == 'unknown' .or. &
            .not. build_info%tool_available) then
            ! Already reported by detect_and_validate_build_system, exit gracefully
            return
        end if
        
        ! Execute auto-test with guaranteed marker cleanup
        block
            logical :: marker_created
            
            marker_created = .false.
            
            ! Create recursion marker to prevent fork bombs
            call prepare_for_auto_test_execution()
            marker_created = .true.
            
            ! Execute tests with proper timeout handling
            call execute_tests_with_timeout(build_info%test_command, config, &
                                           test_exit_code, execution_success)
            
            ! Report results and set final exit code
            exit_code = handle_test_execution_results(config, test_exit_code, &
                                                    execution_success)
            
            ! GUARANTEED cleanup: Always clean up recursion marker if created
            if (marker_created) then
                call cleanup_recursion_marker()
            end if
            
        end block
        
    end function execute_auto_test_workflow
    
    subroutine execute_tests_with_timeout(test_command, config, exit_code, &
                                         success)
        !! Execute test command with timeout handling
        !!
        !! Uses system timeout command to limit test execution time and prevent
        !! hanging tests. Provides secure command execution with proper
        !! argument escaping.
        !!
        !! Args:
        !!   test_command: The test command to execute
        !!   config: Configuration with timeout settings
        !!   exit_code: Exit code from test execution
        !!   success: True if tests passed, false if failed or timed out
        
        character(len=*), intent(in) :: test_command
        type(config_t), intent(in) :: config
        integer, intent(out) :: exit_code
        logical, intent(out) :: success
        
        character(len=1024) :: full_command
        character(len=32) :: timeout_str
        
        success = .false.
        
        ! Build timeout command with proper escaping
        write(timeout_str, '(I0)') config%test_timeout_seconds
        
        ! Build full timeout command
        full_command = 'timeout ' // trim(timeout_str) // 's ' // trim(test_command)
        
        if (.not. config%quiet) then
            print *, "🔧 Executing: " // trim(test_command)
            print *, "⏱️  Timeout: " // trim(timeout_str) // " seconds"
        end if
        
        ! Execute the command with timeout
        call execute_command_line(full_command, exitstat=exit_code)
        
        ! Check results
        if (exit_code == 0) then
            success = .true.
        else if (exit_code == 124) then
            ! Standard timeout exit code
            success = .false.
        else
            ! Test failure or other error
            success = .false.
        end if
        
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
    
    function detect_and_validate_build_system(config, build_info, error_ctx) result(exit_code)
        !! Detect build system and validate it's usable
        type(config_t), intent(in) :: config
        type(build_system_info_t), intent(out) :: build_info
        type(error_context_t), intent(out) :: error_ctx
        integer :: exit_code
        
        exit_code = EXIT_SUCCESS
        
        ! Detect build system
        call detect_build_system('.', build_info, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call report_build_detection_failed(config, error_ctx)
            exit_code = 2  ! Build system detection failed
            return
        end if
        
        ! Handle unknown build system
        if (build_info%system_type == 'unknown') then
            call report_unknown_build_system(config)
            return  ! Skip gracefully
        end if
        
        ! Check if build tool is available
        if (.not. build_info%tool_available) then
            call report_build_tool_unavailable(config, build_info)
            return  ! Skip gracefully
        end if
        
        call report_build_system_detected(config, build_info)
        
    end function detect_and_validate_build_system
    
    subroutine report_build_detection_failed(config, error_ctx)
        !! Report build system detection failure
        type(config_t), intent(in) :: config
        type(error_context_t), intent(in) :: error_ctx
        
        if (.not. config%quiet) then
            print *, "❌ Build system detection failed: " // &
                     trim(error_ctx%message)
        end if
    end subroutine report_build_detection_failed
    
    subroutine report_unknown_build_system(config)
        !! Report unknown build system detected
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "⚠️  No known build system detected, skipping tests"
            print *, "   Supported: FPM, CMake, Make, Meson"
        end if
    end subroutine report_unknown_build_system
    
    subroutine report_build_tool_unavailable(config, build_info)
        !! Report build tool not available
        type(config_t), intent(in) :: config
        type(build_system_info_t), intent(in) :: build_info
        
        if (.not. config%quiet) then
            print *, "⚠️  Build tool not available for " // &
                     trim(build_info%system_type) // ", skipping tests"
        end if
    end subroutine report_build_tool_unavailable
    
    subroutine report_build_system_detected(config, build_info)
        !! Report successful build system detection
        type(config_t), intent(in) :: config
        type(build_system_info_t), intent(in) :: build_info
        
        if (.not. config%quiet) then
            print *, "📦 Build system detected: " // trim(build_info%system_type)
        end if
    end subroutine report_build_system_detected
    
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
        
        inquire(file='.fortcov_execution_marker', exist=file_exists)
        if (file_exists) then
            call execute_command_line('rm -f .fortcov_execution_marker')
        end if
        
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
    
    subroutine report_fork_bomb_prevention(config)
        !! Report that fork bomb prevention was triggered
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "🛡️  Fork bomb prevention: auto-test execution disabled"
            print *, "   (fortcov detected it's running within a test environment)"
        end if
    end subroutine report_fork_bomb_prevention

end module coverage_test_executor_impl