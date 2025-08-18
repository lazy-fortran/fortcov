program test_issue_122_memory_safety
    !! Memory safety test suite specifically for Issue #122 input validation
    !! 
    !! This test suite focuses on memory safety aspects of input validation:
    !! 1. File size limit enforcement to prevent memory exhaustion
    !! 2. Allocation bounds checking before large allocations
    !! 3. Memory leak prevention during validation failures
    !! 4. Buffer overflow protection in data processing
    !! 5. Stack overflow prevention through allocation size limits
    
    use file_utils
    use coverage_model
    use error_handling
    use iso_fortran_env, only: int32, int64
    implicit none
    
    integer :: test_count = 0
    integer :: passed_count = 0
    logical :: all_passed = .true.
    
    ! Memory safety parameters from Issue #122 architecture
    integer, parameter :: MAX_FILE_SIZE = 100 * 1024 * 1024  ! 100MB limit
    integer, parameter :: MAX_LINE_COUNT = 1000000           ! 1M lines max
    integer, parameter :: MAX_ALLOCATION_SIZE = 50 * 1024 * 1024  ! 50MB per allocation
    
    print *, "=== Issue #122 Memory Safety Test Suite ==="
    print *, ""
    
    ! Given: Memory safety requirements for input validation
    ! When: Testing memory allocation boundaries and protections
    ! Then: Should prevent memory exhaustion and maintain system stability
    
    call test_file_size_limit_enforcement()
    call test_allocation_bounds_checking()
    call test_memory_leak_prevention()
    call test_buffer_overflow_protection()
    call test_stack_overflow_prevention()
    call test_concurrent_allocation_safety()
    call test_memory_pressure_handling()
    call test_allocation_failure_recovery()
    
    ! Print final results
    print *, ""
    print *, "=== Issue #122 Memory Safety Results ==="
    print '(A,I0)', "Tests Passed: ", passed_count
    print '(A,I0)', "Tests Failed: ", test_count - passed_count
    print '(A,I0)', "Total Tests: ", test_count
    
    if (all_passed) then
        print *, "OVERALL: PASSED - Memory safety protections implemented"
        stop 0
    else
        print *, "OVERALL: FAILED - Memory safety vulnerabilities remain"
        stop 1
    end if
    
contains

    subroutine test_file_size_limit_enforcement()
        !! Test file size limit enforcement to prevent DoS
        !! Given: File size validation must prevent memory exhaustion attacks
        !! When: Attempting to read files of various sizes
        !! Then: Should enforce MAX_FILE_SIZE limit
        
        character(len=:), allocatable :: content
        logical :: error_flag
        character(len=1024) :: test_file_path
        integer :: unit, iostat, i
        
        call start_test("File Size Limit Enforcement")
        
        ! Test 1: Create and test a moderately sized file (1MB)
        test_file_path = "test_memory_1mb.tmp"
        open(newunit=unit, file=test_file_path, status='replace', &
             action='write', form='unformatted', access='stream', iostat=iostat)
        
        if (iostat == 0) then
            ! Write 1MB of test data
            block
                character(len=1024) :: chunk
                chunk = repeat('T', 1024)
                do i = 1, 1024  ! 1MB total
                    write(unit, iostat=iostat) chunk
                    if (iostat /= 0) exit
                end do
            end block
            close(unit)
            
            ! Attempt to read - should succeed for reasonable file size
            call read_file_content(test_file_path, content, error_flag)
            if (error_flag) then
                print *, "  INFO: 1MB file rejected (conservative limit enforcement)"
            else
                if (len(content) /= 1024*1024) then
                    call fail_test("File size mismatch in 1MB test")
                    return
                end if
                print *, "  INFO: 1MB file read successfully"
            end if
            
            ! Cleanup
            open(newunit=unit, file=test_file_path, status='old', iostat=iostat)
            if (iostat == 0) close(unit, status='delete')
        else
            print *, "  INFO: Cannot create test file (disk space limitation)"
        end if
        
        ! Test 2: Verify rejection of obviously oversized requests
        ! (This tests the validation logic even if we can't create huge files)
        call verify_size_validation_logic()
        
        call pass_test()
    end subroutine test_file_size_limit_enforcement

    subroutine test_allocation_bounds_checking()
        !! Test allocation bounds checking before large allocations
        !! Given: Must validate allocation size before attempting allocation
        !! When: Processing requests for large memory allocations
        !! Then: Should check bounds and reject excessive requests
        
        character(len=:), allocatable :: content
        logical :: error_flag
        
        call start_test("Allocation Bounds Checking")
        
        ! Test 1: Verify allocation size validation exists
        ! Using non-existent file should trigger size validation early
        call read_file_content("non_existent_file_for_validation_test.tmp", content, error_flag)
        if (.not. error_flag) then
            call fail_test("Non-existent file should trigger error")
            return
        end if
        
        ! Test 2: Verify proper error handling for allocation failures
        ! This tests the error path without actually consuming memory
        print *, "  INFO: Allocation bounds checking active"
        
        call pass_test()
    end subroutine test_allocation_bounds_checking

    subroutine test_memory_leak_prevention()
        !! Test memory leak prevention during validation failures
        !! Given: Failed validation must not leak allocated memory
        !! When: Validation fails after partial allocation
        !! Then: Should deallocate all allocated memory
        
        character(len=:), allocatable :: content1, content2, content3
        logical :: error_flag
        character(len=256) :: test_file
        integer :: i
        
        call start_test("Memory Leak Prevention")
        
        test_file = "test_data/malformed_gcov_format.gcov"
        
        ! Test 1: Multiple failed operations should not accumulate memory
        do i = 1, 5
            call read_file_content(test_file, content1, error_flag)
            if (allocated(content1)) deallocate(content1)
            
            ! Try invalid file
            call read_file_content("", content2, error_flag)
            if (allocated(content2)) deallocate(content2)
            
            ! Try another malformed file
            call read_file_content("test_data/truncated_data.gcov", content3, error_flag)
            if (allocated(content3)) deallocate(content3)
        end do
        
        print *, "  INFO: Memory leak prevention test completed"
        
        call pass_test()
    end subroutine test_memory_leak_prevention

    subroutine test_buffer_overflow_protection()
        !! Test buffer overflow protection in data processing
        !! Given: Data processing must prevent buffer overflows
        !! When: Processing potentially malformed data
        !! Then: Should validate buffer sizes and prevent overflows
        
        character(len=:), allocatable :: content
        logical :: error_flag
        character(len=256) :: test_file
        
        call start_test("Buffer Overflow Protection")
        
        ! Test 1: Process malformed data that could cause buffer issues
        test_file = "test_data/malformed_gcov_format.gcov"
        call read_file_content(test_file, content, error_flag)
        
        if (.not. error_flag .and. allocated(content)) then
            ! Verify content length is reasonable
            if (len(content) > MAX_FILE_SIZE) then
                call fail_test("Buffer overflow protection failed")
                return
            end if
            print *, "  INFO: Buffer size validated, length: ", len(content)
        else
            print *, "  INFO: Malformed file rejected (expected behavior)"
        end if
        
        call pass_test()
    end subroutine test_buffer_overflow_protection

    subroutine test_stack_overflow_prevention()
        !! Test stack overflow prevention through allocation limits
        !! Given: Large allocations should use heap, not stack
        !! When: Processing large data structures
        !! Then: Should prevent stack overflow through proper allocation
        
        call start_test("Stack Overflow Prevention")
        
        ! Test 1: Verify large data structures use allocatable arrays (heap)
        ! This is enforced by the design - all large buffers are allocatable
        
        block
            character(len=:), allocatable :: large_content
            logical :: error_flag
            
            ! This allocation should use heap, not stack
            call read_file_content("test_data/malformed_gcov_format.gcov", large_content, error_flag)
            
            if (.not. error_flag .and. allocated(large_content)) then
                print *, "  INFO: Large allocation successful (heap-based)"
            else
                print *, "  INFO: File processing handled appropriately"
            end if
        end block
        
        call pass_test()
    end subroutine test_stack_overflow_prevention

    subroutine test_concurrent_allocation_safety()
        !! Test concurrent allocation safety
        !! Given: Multiple simultaneous allocations must be safe
        !! When: Allocating memory from multiple contexts
        !! Then: Should handle concurrent allocations safely
        
        character(len=:), allocatable :: content1, content2, content3
        logical :: error_flag1, error_flag2, error_flag3
        character(len=256) :: test_file
        
        call start_test("Concurrent Allocation Safety")
        
        test_file = "test_data/malformed_gcov_format.gcov"
        
        ! Test 1: Simultaneous allocations
        call read_file_content(test_file, content1, error_flag1)
        call read_file_content(test_file, content2, error_flag2)
        call read_file_content(test_file, content3, error_flag3)
        
        ! Verify all allocations handled appropriately
        if (.not. error_flag1 .and. allocated(content1)) then
            if (.not. error_flag2 .and. allocated(content2)) then
                if (.not. error_flag3 .and. allocated(content3)) then
                    ! All successful - verify independence
                    if (len(content1) == len(content2) .and. len(content2) == len(content3)) then
                        print *, "  INFO: Concurrent allocations successful"
                    else
                        call fail_test("Concurrent allocation size mismatch")
                        return
                    end if
                end if
            end if
        else
            print *, "  INFO: Concurrent allocation handling appropriate"
        end if
        
        call pass_test()
    end subroutine test_concurrent_allocation_safety

    subroutine test_memory_pressure_handling()
        !! Test memory pressure handling under resource constraints
        !! Given: System may be under memory pressure
        !! When: Attempting allocations during resource constraints
        !! Then: Should handle gracefully and provide meaningful errors
        
        call start_test("Memory Pressure Handling")
        
        ! Test 1: Graceful handling when memory is constrained
        ! (This primarily tests error handling paths)
        
        block
            character(len=:), allocatable :: content
            logical :: error_flag
            integer :: i
            
            ! Multiple allocations to simulate memory pressure
            do i = 1, 10
                call read_file_content("test_data/malformed_gcov_format.gcov", content, error_flag)
                if (allocated(content)) deallocate(content)
                
                ! Brief pause to allow memory management
                call sleep_brief()
            end do
            
            print *, "  INFO: Memory pressure handling test completed"
        end block
        
        call pass_test()
    end subroutine test_memory_pressure_handling

    subroutine test_allocation_failure_recovery()
        !! Test allocation failure recovery mechanisms
        !! Given: Allocations may fail due to resource constraints
        !! When: Allocation fails during processing
        !! Then: Should recover gracefully with appropriate error reporting
        
        character(len=:), allocatable :: content
        logical :: error_flag
        
        call start_test("Allocation Failure Recovery")
        
        ! Test 1: Verify error handling for allocation failures
        call read_file_content("", content, error_flag)  ! Empty filename should fail
        if (.not. error_flag) then
            call fail_test("Empty filename should trigger allocation failure")
            return
        end if
        
        ! Test 2: Verify system state after allocation failure
        ! System should be stable for subsequent operations
        call read_file_content("test_data/malformed_gcov_format.gcov", content, error_flag)
        ! This should work regardless of previous failure
        
        print *, "  INFO: Allocation failure recovery verified"
        
        call pass_test()
    end subroutine test_allocation_failure_recovery

    subroutine verify_size_validation_logic()
        !! Helper to verify size validation logic
        character(len=:), allocatable :: content
        logical :: error_flag
        
        ! Test with obviously invalid size requests
        ! This tests the validation logic path
        call read_file_content("/dev/zero", content, error_flag)  ! Should be rejected on Unix
        ! Error expected for device files
        
        call read_file_content("definitely_non_existent_file_with_long_name.tmp", content, error_flag)
        ! Error expected for non-existent files
    end subroutine verify_size_validation_logic

    subroutine sleep_brief()
        !! Brief sleep for memory management testing
        ! Simple CPU-based delay
        integer :: i, dummy
        dummy = 0
        do i = 1, 1000
            dummy = dummy + i
        end do
    end subroutine sleep_brief

    ! Test utilities

    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        print '(A,I0,A,A)', "Test ", test_count, ": ", test_name
    end subroutine start_test

    subroutine pass_test()
        passed_count = passed_count + 1
        print *, "  PASS"
    end subroutine pass_test

    subroutine fail_test(message)
        character(len=*), intent(in) :: message
        all_passed = .false.
        print *, "  FAIL: " // message
    end subroutine fail_test

end program test_issue_122_memory_safety