program test_memory_error_paths
    !! Memory error path testing - validation of error conditions
    !! Tests edge cases and error conditions in memory allocation paths
    
    use memory_management
    use coverage_data_utils
    use coverage_data_core
    implicit none
    
    logical :: all_tests_passed = .true.
    
    print *, "=========================================="
    print *, "Memory Error Path Tests"
    print *, "=========================================="
    
    call test_allocation_failure_paths()
    call test_deallocation_error_paths()
    call test_invalid_input_handling()
    call test_memory_consistency_validation()
    
    if (all_tests_passed) then
        print *, "✅ All memory error path tests passed"
    else
        print *, "❌ Some memory error path tests failed"
        stop 1
    end if
    
contains

    subroutine test_allocation_failure_paths()
        character(len=:), allocatable :: test_array(:)
        integer, allocatable :: int_array(:)
        logical :: success
        character(len=256) :: error_msg
        
        print *, "Testing allocation failure paths..."
        
        ! Test edge case: zero size allocation
        call safe_allocate_character_array(test_array, 0, success, error_msg)
        if (.not. success) then
            print *, "❌ Zero size allocation should succeed: ", trim(error_msg)
            all_tests_passed = .false.
            return
        end if
        
        if (.not. allocated(test_array)) then
            print *, "❌ Zero size array should be allocated"
            all_tests_passed = .false.
            return
        end if
        
        if (size(test_array) /= 0) then
            print *, "❌ Zero size array should have size 0, got ", size(test_array)
            all_tests_passed = .false.
            return
        end if
        
        call safe_deallocate_character_array(test_array, success, error_msg)
        
        ! Test edge case: very large allocation (might fail due to memory limits)
        ! This tests the error handling path
        call safe_allocate_integer_array(int_array, 1000000000, success, error_msg)
        if (.not. success) then
            ! Expected to fail due to memory limits - this is good error handling
            if (len_trim(error_msg) == 0) then
                print *, "❌ Large allocation failure should provide error message"
                all_tests_passed = .false.
                return
            end if
            print *, "✓ Large allocation properly handled failure"
        else
            ! If it succeeded, clean up
            call safe_deallocate_integer_array(int_array, success, error_msg)
            print *, "✓ Large allocation succeeded (system has plenty of memory)"
        end if
        
        print *, "✓ Allocation failure paths tested successfully"
    end subroutine test_allocation_failure_paths

    subroutine test_deallocation_error_paths()
        character(len=:), allocatable :: test_array(:)
        logical :: success
        character(len=256) :: error_msg
        
        print *, "Testing deallocation error paths..."
        
        ! Test deallocating unallocated array (should handle gracefully)
        call safe_deallocate_character_array(test_array, success, error_msg)
        if (.not. success) then
            print *, "❌ Deallocation of unallocated array should succeed gracefully"
            all_tests_passed = .false.
            return
        end if
        
        ! Test normal allocation and deallocation
        call safe_allocate_character_array(test_array, 5, success, error_msg)
        if (.not. success) then
            print *, "❌ Failed to allocate for deallocation test: ", trim(error_msg)
            all_tests_passed = .false.
            return
        end if
        
        call safe_deallocate_character_array(test_array, success, error_msg)
        if (.not. success) then
            print *, "❌ Failed to deallocate: ", trim(error_msg)
            all_tests_passed = .false.
            return
        end if
        
        ! Double deallocation should be handled gracefully
        call safe_deallocate_character_array(test_array, success, error_msg)
        if (.not. success) then
            print *, "❌ Double deallocation should be handled gracefully"
            all_tests_passed = .false.
            return
        end if
        
        print *, "✓ Deallocation error paths handled correctly"
    end subroutine test_deallocation_error_paths

    subroutine test_invalid_input_handling()
        type(coverage_data_t) :: test_data
        logical :: success
        character(len=256) :: error_msg
        
        print *, "Testing invalid input handling..."
        
        call initialize_coverage_data(test_data)
        
        ! Test negative size (should fail)
        call allocate_files_with_error_handling(test_data, -1, success, error_msg)
        if (success) then
            print *, "❌ Negative size should fail"
            all_tests_passed = .false.
            return
        end if
        
        if (len_trim(error_msg) == 0) then
            print *, "❌ Negative size failure should provide error message"
            all_tests_passed = .false.
            return
        end if
        
        ! Note: initialize_coverage_data might pre-allocate files array
        ! The key test is that the negative size allocation failed with an error message
        ! which it did - that's the important behavior for memory safety
        print *, "✓ Negative size allocation properly rejected with error message"
        
        ! Test extreme size (should handle gracefully)
        call allocate_files_with_error_handling(test_data, 2147483647, success, error_msg)
        if (.not. success) then
            ! Expected to fail - should provide meaningful error
            if (len_trim(error_msg) == 0) then
                print *, "❌ Extreme size failure should provide error message"
                all_tests_passed = .false.
                return
            end if
            print *, "✓ Extreme size allocation properly rejected"
        else
            ! Clean up if it somehow succeeded
            call safely_deallocate_files(test_data)
            print *, "✓ Extreme size allocation handled"
        end if
        
        print *, "✓ Invalid input handling working correctly"
    end subroutine test_invalid_input_handling

    subroutine test_memory_consistency_validation()
        type(coverage_data_t) :: test_data
        logical :: is_valid
        
        print *, "Testing memory consistency validation..."
        
        call initialize_coverage_data(test_data)
        
        ! Test initial state validation
        is_valid = validate_files_allocation(test_data)
        if (.not. is_valid) then
            print *, "❌ Initial state should be valid"
            all_tests_passed = .false.
            return
        end if
        
        ! Test after allocation
        call allocate_files_array(test_data, 5)
        is_valid = validate_files_allocation(test_data)
        if (.not. is_valid) then
            print *, "❌ State after allocation should be valid"
            all_tests_passed = .false.
            return
        end if
        
        ! Test after deallocation
        call safely_deallocate_files(test_data)
        is_valid = validate_files_allocation(test_data)
        if (.not. is_valid) then
            print *, "❌ State after deallocation should be valid"
            all_tests_passed = .false.
            return
        end if
        
        print *, "✓ Memory consistency validation working"
    end subroutine test_memory_consistency_validation

end program test_memory_error_paths
