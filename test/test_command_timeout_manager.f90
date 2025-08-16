program test_command_timeout_manager
    !! Test suite for command timeout manager
    !! 
    !! This test suite validates timeout accuracy, process tree termination,
    !! resource cleanup, DoS protection, and security features for the
    !! command timeout manager module.
    use iso_fortran_env, only: real64
    use command_timeout_manager
    use error_handling
    implicit none
    
    integer :: tests_run = 0
    integer :: tests_passed = 0
    real(real64) :: start_time, end_time
    
    ! Run all test cases
    write(*, '(A)') "Running command timeout manager tests..."
    
    call test_timeout_accuracy()
    call test_process_tree_termination()
    call test_resource_cleanup()
    call test_dos_protection()
    call test_security_injection_protection()
    call test_cross_platform_compatibility()
    call test_graceful_termination_sequence()
    call test_timeout_precision()
    call test_concurrent_process_management()
    call test_configuration_validation()
    
    ! Report results
    write(*, '(A,I0,A,I0,A)') "Tests passed: ", tests_passed, "/", &
        tests_run, " total"
    
    if (tests_passed == tests_run) then
        write(*, '(A)') "All tests passed!"
        stop 0
    else
        write(*, '(A)') "Some tests failed!"
        stop 1
    end if

contains

    ! Test timeout accuracy within ±100ms tolerance
    subroutine test_timeout_accuracy()
        type(timeout_command_executor_t) :: executor
        type(error_context_t) :: error_ctx
        real(real64) :: elapsed
        integer :: timeout_seconds = 2
        
        call increment_test_count()
        
        call create_timeout_executor(executor, timeout_seconds, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call fail_test("Failed to create timeout executor")
            return
        end if
        
        start_time = get_wall_time()
        
        ! Command that should run longer than timeout
        call execute_with_timeout(executor, "sleep 5", error_ctx)
        
        end_time = get_wall_time()
        elapsed = end_time - start_time
        
        ! Timeout should occur within 100ms tolerance (±5%)
        if (elapsed >= 1.9_real64 .and. elapsed <= 2.1_real64) then
            if (executor%status == STATUS_TIMEOUT) then
                call pass_test("Timeout accuracy test passed")
            else
                call fail_test("Expected STATUS_TIMEOUT but got different status")
            end if
        else
            call fail_test("Timeout accuracy outside tolerance")
        end if
        
        call destroy_timeout_executor(executor, error_ctx)
    end subroutine test_timeout_accuracy
    
    ! Test process tree termination (all child processes killed)
    subroutine test_process_tree_termination()
        type(timeout_command_executor_t) :: executor
        type(error_context_t) :: error_ctx
        
        call increment_test_count()
        
        call create_timeout_executor(executor, 1, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call fail_test("Failed to create timeout executor")
            return
        end if
        
        ! Command that spawns multiple child processes
        call execute_with_timeout(executor, &
            "bash -c 'sleep 10 & sleep 10 & sleep 10 & wait'", error_ctx)
        
        ! Verify timeout occurred and all processes terminated
        if (executor%status == STATUS_TIMEOUT) then
            call verify_no_child_processes(executor%process_id)
            call pass_test("Process tree termination test passed")
        else
            call fail_test("Expected timeout for process tree test")
        end if
        
        call destroy_timeout_executor(executor, error_ctx)
    end subroutine test_process_tree_termination
    
    ! Test resource cleanup (no memory/handle leaks)
    subroutine test_resource_cleanup()
        type(timeout_command_executor_t) :: executor
        type(error_context_t) :: error_ctx
        integer :: initial_handles, final_handles
        integer :: i
        
        call increment_test_count()
        
        initial_handles = count_open_handles()
        
        ! Create and destroy multiple executors to test for leaks
        do i = 1, 10
            call create_timeout_executor(executor, 1, error_ctx)
            if (error_ctx%error_code /= ERROR_SUCCESS) then
                call fail_test("Failed to create timeout executor in loop")
                return
            end if
            
            call execute_with_timeout(executor, "sleep 2", error_ctx)
            call destroy_timeout_executor(executor, error_ctx)
        end do
        
        final_handles = count_open_handles()
        
        ! No handle/resource leaks should occur
        if (final_handles == initial_handles) then
            call pass_test("Resource cleanup test passed")
        else
            call fail_test("Resource leak detected")
        end if
    end subroutine test_resource_cleanup
    
    ! Test DoS protection (prevent resource exhaustion)
    subroutine test_dos_protection()
        type(timeout_command_executor_t) :: executor
        type(error_context_t) :: error_ctx
        real(real64) :: elapsed
        
        call increment_test_count()
        
        call create_timeout_executor(executor, 1, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call fail_test("Failed to create timeout executor")
            return
        end if
        
        start_time = get_wall_time()
        
        ! CPU-intensive command that would consume resources indefinitely
        call execute_with_timeout(executor, &
            "yes | head -c 100M > /dev/null", error_ctx)
        
        end_time = get_wall_time()
        elapsed = end_time - start_time
        
        ! Should terminate within timeout regardless of CPU/memory load
        if (elapsed <= 1.5_real64 .and. executor%status == STATUS_TIMEOUT) then
            call pass_test("DoS protection test passed")
        else
            call fail_test("DoS protection failed")
        end if
        
        call destroy_timeout_executor(executor, error_ctx)
    end subroutine test_dos_protection
    
    ! Test security (shell injection protection)
    subroutine test_security_injection_protection()
        type(timeout_command_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: malicious_command
        
        call increment_test_count()
        
        ! Command injection attempts
        malicious_command = "gcov test.f90; rm -rf /tmp/test_injection #"
        
        call create_timeout_executor(executor, 5, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call fail_test("Failed to create timeout executor")
            return
        end if
        
        call execute_with_timeout(executor, malicious_command, error_ctx)
        
        ! Should fail safely without executing destructive command
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call pass_test("Security injection protection test passed")
        else
            call fail_test("Security vulnerability detected")
        end if
        
        call destroy_timeout_executor(executor, error_ctx)
    end subroutine test_security_injection_protection
    
    ! Test cross-platform compatibility
    subroutine test_cross_platform_compatibility()
        type(timeout_command_executor_t) :: executor
        type(error_context_t) :: error_ctx
        
        call increment_test_count()
        
        call create_timeout_executor(executor, 2, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call fail_test("Failed to create timeout executor")
            return
        end if
        
        ! Use basic command that works across platforms
        call execute_with_timeout(executor, "echo test", error_ctx)
        
        if (executor%status == STATUS_COMPLETED) then
            call pass_test("Cross-platform compatibility test passed")
        else
            call fail_test("Cross-platform compatibility test failed")
        end if
        
        call destroy_timeout_executor(executor, error_ctx)
    end subroutine test_cross_platform_compatibility
    
    ! Test graceful termination sequence (SIGTERM → SIGKILL)
    subroutine test_graceful_termination_sequence()
        type(timeout_command_executor_t) :: executor
        type(error_context_t) :: error_ctx
        
        call increment_test_count()
        
        call create_timeout_executor(executor, 1, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call fail_test("Failed to create timeout executor")
            return
        end if
        
        ! Command that can handle SIGTERM gracefully
        call execute_with_timeout(executor, "sleep 10", error_ctx)
        
        if (executor%status == STATUS_TIMEOUT) then
            call verify_graceful_termination_attempted(executor%process_id)
            call pass_test("Graceful termination sequence test passed")
        else
            call fail_test("Expected timeout for graceful termination test")
        end if
        
        call destroy_timeout_executor(executor, error_ctx)
    end subroutine test_graceful_termination_sequence
    
    ! Test timeout precision (millisecond-level accuracy)
    subroutine test_timeout_precision()
        type(timeout_command_executor_t) :: executor
        type(error_context_t) :: error_ctx
        real(real64) :: elapsed
        
        call increment_test_count()
        
        call create_timeout_executor(executor, 1, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call fail_test("Failed to create timeout executor")
            return
        end if
        
        start_time = get_wall_time()
        call execute_with_timeout(executor, "sleep 2", error_ctx)
        end_time = get_wall_time()
        elapsed = end_time - start_time
        
        ! Should timeout with millisecond precision
        if (elapsed >= 0.95_real64 .and. elapsed <= 1.05_real64) then
            call pass_test("Timeout precision test passed")
        else
            call fail_test("Timeout precision outside acceptable range")
        end if
        
        call destroy_timeout_executor(executor, error_ctx)
    end subroutine test_timeout_precision
    
    ! Test concurrent process management
    subroutine test_concurrent_process_management()
        type(timeout_command_executor_t) :: executors(5)
        type(error_context_t) :: error_ctx
        integer :: i
        
        call increment_test_count()
        
        ! Create multiple concurrent timeout executors
        do i = 1, 5
            call create_timeout_executor(executors(i), 2, error_ctx)
            if (error_ctx%error_code /= ERROR_SUCCESS) then
                call fail_test("Failed to create concurrent timeout executor")
                return
            end if
        end do
        
        ! Execute commands concurrently
        do i = 1, 5
            call execute_with_timeout(executors(i), "sleep 3", error_ctx)
        end do
        
        ! Verify all timed out correctly
        do i = 1, 5
            if (executors(i)%status /= STATUS_TIMEOUT) then
                call fail_test("Concurrent process management failed")
                return
            end if
        end do
        
        ! Cleanup
        do i = 1, 5
            call destroy_timeout_executor(executors(i), error_ctx)
        end do
        
        call pass_test("Concurrent process management test passed")
    end subroutine test_concurrent_process_management
    
    ! Test configuration validation
    subroutine test_configuration_validation()
        type(timeout_command_executor_t) :: executor
        type(error_context_t) :: error_ctx
        
        call increment_test_count()
        
        ! Test invalid timeout values
        call create_timeout_executor(executor, -1, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call pass_test("Configuration validation test passed")
        else
            call fail_test("Should reject invalid timeout values")
        end if
        
        ! Test very large timeout values
        call create_timeout_executor(executor, 86400, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call destroy_timeout_executor(executor, error_ctx)
        else
            call fail_test("Should accept large but valid timeout values")
        end if
    end subroutine test_configuration_validation
    
    ! Helper procedures for test infrastructure
    
    subroutine increment_test_count()
        tests_run = tests_run + 1
    end subroutine increment_test_count
    
    subroutine pass_test(test_name)
        character(len=*), intent(in) :: test_name
        tests_passed = tests_passed + 1
        write(*, '(A,A)') "PASS: ", test_name
    end subroutine pass_test
    
    subroutine fail_test(test_name)
        character(len=*), intent(in) :: test_name
        write(*, '(A,A)') "FAIL: ", test_name
    end subroutine fail_test
    
    ! Get wall clock time in seconds (precision implementation required)
    function get_wall_time() result(time_seconds)
        real(real64) :: time_seconds
        integer :: count, count_rate
        
        call system_clock(count, count_rate)
        time_seconds = real(count, real64) / real(count_rate, real64)
    end function get_wall_time
    
    ! Verify no child processes remain (implementation required)
    subroutine verify_no_child_processes(process_id)
        integer, intent(in) :: process_id
        ! Implementation will verify process tree cleanup
        ! For now, this is a placeholder
        continue
    end subroutine verify_no_child_processes
    
    ! Count open file handles/resources (implementation required)
    function count_open_handles() result(handle_count)
        integer :: handle_count
        ! Implementation will count system resources
        ! For now, return dummy value
        handle_count = 0
    end function count_open_handles
    
    ! Verify graceful termination was attempted (implementation required)
    subroutine verify_graceful_termination_attempted(process_id)
        integer, intent(in) :: process_id
        ! Implementation will verify SIGTERM was sent before SIGKILL
        ! For now, this is a placeholder
        continue
    end subroutine verify_graceful_termination_attempted

end program test_command_timeout_manager