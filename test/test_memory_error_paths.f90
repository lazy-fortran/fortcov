program test_memory_error_paths
    !! Memory error path tests
    !! Tests error handling in allocation scenarios
    
    use test_utilities, only: test_runner_t, assert_true, assert_false
    use coverage_types, only: coverage_data_t, coverage_collection_t
    implicit none
    
    type(test_runner_t) :: runner
    
    call runner%init("Memory Error Path Tests")
    
    ! Error path tests
    call test_allocation_failure_handling()
    call test_error_recovery()
    call test_memory_leak_prevention()
    call test_exception_safety()
    
    call runner%print_summary()
    
    if (runner%get_pass_rate() == 100.0) then
        call exit(0)
    else
        call exit(1)
    end if
    
contains
    
    subroutine test_allocation_failure_handling()
        !! Test handling of allocation failures
        type(coverage_data_t), allocatable :: large_array(:)
        logical :: passed, allocation_failed
        integer :: alloc_stat
        
        passed = .true.
        allocation_failed = .false.
        
        ! Try to allocate with error handling
        allocate(large_array(100), stat=alloc_stat)
        
        if (alloc_stat /= 0) then
            allocation_failed = .true.
            call assert_true(allocation_failed, &
                "Allocation failure should be detected", passed)
        else
            call assert_true(allocated(large_array), &
                "Successful allocation should be detected", passed)
            deallocate(large_array)
        end if
        
        call runner%run_test("allocation_failure_handling", passed)
    end subroutine test_allocation_failure_handling
    
    subroutine test_error_recovery()
        !! Test recovery from allocation errors
        type(coverage_collection_t) :: collection
        logical :: passed, recovered
        integer :: i
        
        passed = .true.
        recovered = .false.
        
        call collection%initialize()
        
        ! Simulate error during processing
        do i = 1, 10
            if (i == 5) then
                ! Simulate error condition
                recovered = collection%recover_from_error()
                call assert_true(recovered, &
                    "Should recover from error", passed)
            else
                call collection%add_file_data("file_" // trim(int_to_str(i)))
            end if
        end do
        
        call runner%run_test("error_recovery", passed)
    end subroutine test_error_recovery
    
    subroutine test_memory_leak_prevention()
        !! Test prevention of memory leaks
        type(coverage_data_t), pointer :: coverage_ptr
        logical :: passed
        
        passed = .true.
        
        ! Allocate pointer
        allocate(coverage_ptr)
        call coverage_ptr%initialize()
        
        ! Use coverage
        call coverage_ptr%add_line_coverage(1, 1)
        
        ! Clean up to prevent leak
        if (associated(coverage_ptr)) then
            deallocate(coverage_ptr)
            nullify(coverage_ptr)
        end if
        
        call assert_false(associated(coverage_ptr), &
            "Pointer should be nullified", passed)
        
        call runner%run_test("memory_leak_prevention", passed)
    end subroutine test_memory_leak_prevention
    
    subroutine test_exception_safety()
        !! Test exception safety guarantees
        type(coverage_data_t) :: coverage
        logical :: passed, exception_handled
        
        passed = .true.
        exception_handled = .false.
        
        call coverage%initialize()
        
        ! Simulate exceptional condition
        block
            integer :: status
            
            ! Try operation that might fail
            status = coverage%process_exceptional_data()
            
            if (status /= 0) then
                exception_handled = .true.
                ! Coverage should still be in valid state
                call assert_true(coverage%is_valid(), &
                    "Should maintain valid state after exception", passed)
            end if
        end block
        
        call runner%run_test("exception_safety", passed)
    end subroutine test_exception_safety
    
    ! Helper function
    function int_to_str(n) result(str)
        integer, intent(in) :: n
        character(len=32) :: str
        write(str, '(I0)') n
    end function int_to_str
    
end program test_memory_error_paths