program test_memory_allocation_core
    !! Memory allocation core functionality tests
    !! Tests memory_management_core and coverage_data_utils allocation patterns
    
    use memory_management_core
    use coverage_data_utils
    use coverage_data_core
    implicit none
    
    logical :: all_tests_passed = .true.
    
    print *, "=========================================="
    print *, "Memory Allocation Core Tests"
    print *, "=========================================="
    
    call test_safe_character_array_allocation()
    call test_safe_integer_array_allocation()
    call test_coverage_data_memory_management()
    call test_memory_status_validation()
    call test_memory_error_handling()
    
    if (all_tests_passed) then
        print *, "✅ All memory allocation tests passed"
    else
        print *, "❌ Some memory allocation tests failed"
        stop 1
    end if
    
contains

    subroutine test_safe_character_array_allocation()
        character(len=:), allocatable :: test_array(:)
        logical :: success
        character(len=256) :: error_msg
        
        print *, "Testing safe character array allocation..."
        
        ! Test normal allocation
        call safe_allocate_character_array(test_array, 5, success, error_msg)
        if (.not. success) then
            print *, "❌ Failed to allocate character array: ", trim(error_msg)
            all_tests_passed = .false.
            return
        end if
        
        if (.not. allocated(test_array)) then
            print *, "❌ Character array not properly allocated"
            all_tests_passed = .false.
            return
        end if
        
        if (size(test_array) /= 5) then
            print *, "❌ Character array wrong size: expected 5, got ", size(test_array)
            all_tests_passed = .false.
            return
        end if
        
        ! Test deallocation
        call safe_deallocate_character_array(test_array, success, error_msg)
        if (.not. success) then
            print *, "❌ Failed to deallocate character array: ", trim(error_msg)
            all_tests_passed = .false.
            return
        end if
        
        if (allocated(test_array)) then
            print *, "❌ Character array still allocated after deallocation"
            all_tests_passed = .false.
            return
        end if
        
        print *, "✓ Character array allocation/deallocation successful"
    end subroutine test_safe_character_array_allocation

    subroutine test_safe_integer_array_allocation()
        integer, allocatable :: test_array(:)
        logical :: success
        character(len=256) :: error_msg
        
        print *, "Testing safe integer array allocation..."
        
        ! Test normal allocation
        call safe_allocate_integer_array(test_array, 10, success, error_msg)
        if (.not. success) then
            print *, "❌ Failed to allocate integer array: ", trim(error_msg)
            all_tests_passed = .false.
            return
        end if
        
        if (.not. allocated(test_array)) then
            print *, "❌ Integer array not properly allocated"
            all_tests_passed = .false.
            return
        end if
        
        if (size(test_array) /= 10) then
            print *, "❌ Integer array wrong size: expected 10, got ", size(test_array)
            all_tests_passed = .false.
            return
        end if
        
        ! Test deallocation
        call safe_deallocate_integer_array(test_array, success, error_msg)
        if (.not. success) then
            print *, "❌ Failed to deallocate integer array: ", trim(error_msg)
            all_tests_passed = .false.
            return
        end if
        
        if (allocated(test_array)) then
            print *, "❌ Integer array still allocated after deallocation"
            all_tests_passed = .false.
            return
        end if
        
        print *, "✓ Integer array allocation/deallocation successful"
    end subroutine test_safe_integer_array_allocation

    subroutine test_coverage_data_memory_management()
        type(coverage_data_t) :: test_data
        logical :: success, is_valid
        character(len=256) :: error_msg
        
        print *, "Testing coverage_data_t memory management..."
        
        ! Initialize coverage data
        call initialize_coverage_data(test_data)
        
        ! Test files array allocation
        call allocate_files_with_error_handling(test_data, 3, success, error_msg)
        if (.not. success) then
            print *, "❌ Failed to allocate files array: ", trim(error_msg)
            all_tests_passed = .false.
            return
        end if
        
        ! Validate allocation state
        is_valid = validate_files_allocation(test_data)
        if (.not. is_valid) then
            print *, "❌ Files allocation validation failed"
            all_tests_passed = .false.
            return
        end if
        
        if (.not. allocated(test_data%files)) then
            print *, "❌ Files array not allocated"
            all_tests_passed = .false.
            return
        end if
        
        if (size(test_data%files) /= 3) then
            print *, "❌ Files array wrong size: expected 3, got ", size(test_data%files)
            all_tests_passed = .false.
            return
        end if
        
        if (test_data%total_files /= 3) then
            print *, "❌ total_files incorrect: expected 3, got ", test_data%total_files
            all_tests_passed = .false.
            return
        end if
        
        ! Test safe deallocation
        call safely_deallocate_files(test_data)
        
        if (allocated(test_data%files)) then
            print *, "❌ Files array still allocated after deallocation"
            all_tests_passed = .false.
            return
        end if
        
        if (test_data%total_files /= 0) then
            print *, "❌ total_files not reset after deallocation: ", test_data%total_files
            all_tests_passed = .false.
            return
        end if
        
        print *, "✓ Coverage data memory management successful"
    end subroutine test_coverage_data_memory_management

    subroutine test_memory_status_validation()
        type(memory_status_t) :: status
        
        print *, "Testing memory status validation..."
        
        ! Get current memory status
        status = get_memory_status()
        
        ! Basic validation - status should have meaningful fields
        if (len_trim(status%status_message) == 0) then
            print *, "❌ Memory status message is empty"
            all_tests_passed = .false.
            return
        end if
        
        if (status%unbalanced_allocations < 0) then
            print *, "❌ Unbalanced allocations cannot be negative: ", &
                     status%unbalanced_allocations
            all_tests_passed = .false.
            return
        end if
        
        print *, "✓ Memory status validation successful"
        print *, "  Status: ", trim(status%status_message)
        print *, "  Balanced: ", status%is_balanced
        print *, "  Unbalanced allocations: ", status%unbalanced_allocations
    end subroutine test_memory_status_validation

    subroutine test_memory_error_handling()
        character(len=:), allocatable :: bad_array(:)
        type(coverage_data_t) :: test_data
        logical :: success
        character(len=256) :: error_msg
        
        print *, "Testing memory error handling..."
        
        ! Test negative size allocation (should fail gracefully)
        call safe_allocate_character_array(bad_array, -1, success, error_msg)
        if (success) then
            print *, "❌ Negative size allocation should have failed"
            all_tests_passed = .false.
            return
        end if
        
        if (len_trim(error_msg) == 0) then
            print *, "❌ Error message should be provided for failed allocation"
            all_tests_passed = .false.
            return
        end if
        
        if (allocated(bad_array)) then
            print *, "❌ Array should not be allocated after failure"
            all_tests_passed = .false.
            return
        end if
        
        ! Test coverage data negative size allocation
        call initialize_coverage_data(test_data)
        
        ! Ensure files array starts unallocated for this test
        if (allocated(test_data%files)) then
            call safely_deallocate_files(test_data)
        end if
        
        call allocate_files_with_error_handling(test_data, -5, success, error_msg)
        if (success) then
            print *, "❌ Negative size files allocation should have failed"
            all_tests_passed = .false.
            return
        end if
        
        if (len_trim(error_msg) == 0) then
            print *, "❌ Error message should be provided for failed files allocation"
            all_tests_passed = .false.
            return
        end if
        
        if (allocated(test_data%files)) then
            print *, "❌ Files array should not be allocated after failure"
            all_tests_passed = .false.
            return
        end if
        
        print *, "✓ Memory error handling successful"
    end subroutine test_memory_error_handling

end program test_memory_allocation_core
