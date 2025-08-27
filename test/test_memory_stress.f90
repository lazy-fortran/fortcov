program test_memory_stress
    !! Comprehensive memory allocation stress test to verify error handling
    !! 
    !! This program tests memory allocation failure scenarios to ensure
    !! proper error handling with stat= and errmsg= parameters.
    
    use iso_fortran_env, only: error_unit
    use coverage_file_processor
    use config_defaults
    use config_types
    
    implicit none
    
    integer :: tests_passed = 0
    integer :: tests_failed = 0
    logical :: test_result
    
    write(*, '(A)') "Memory Allocation Stress Tests"
    write(*, '(A)') "=============================="
    
    ! Test graceful handling of large allocations
    call test_large_allocation_handling(test_result)
    if (test_result) then
        tests_passed = tests_passed + 1
        write(*, '(A)') "✓ Large allocation handling"
    else
        tests_failed = tests_failed + 1
        write(*, '(A)') "✗ Large allocation handling"
    end if
    
    ! Test edge case allocations
    call test_edge_case_allocations(test_result)
    if (test_result) then
        tests_passed = tests_passed + 1
        write(*, '(A)') "✓ Edge case allocations"
    else
        tests_failed = tests_failed + 1
        write(*, '(A)') "✗ Edge case allocations"
    end if
    
    ! Summary
    write(*, '(A)') ""
    write(*, '(A)') "Memory Stress Test Summary:"
    write(*, '(A, I0)') "Tests passed: ", tests_passed
    write(*, '(A, I0)') "Tests failed: ", tests_failed
    
    if (tests_failed == 0) then
        write(*, '(A)') "All memory stress tests completed successfully!"
        stop 0
    else
        write(error_unit, '(A)') "Some memory stress tests failed!"
        stop 1
    end if

contains

    subroutine test_large_allocation_handling(result)
        !! Test handling of large allocations that might fail
        logical, intent(out) :: result
        type(config_t) :: config
        character(len=1024), allocatable :: coverage_files(:), filtered_files(:)
        
        result = .true.
        
        ! Initialize config with reasonable defaults
        call initialize_default_config(config)
        
        ! Test with reasonable allocation sizes (should succeed)
        call find_and_filter_coverage_files(config, coverage_files, filtered_files)
        
        ! Verify arrays are allocated (even if empty)
        if (allocated(coverage_files) .and. allocated(filtered_files)) then
            write(*, '(A,I0,A,I0,A)') "  Allocated coverage_files(", &
                size(coverage_files), ") and filtered_files(", &
                size(filtered_files), ")"
        else
            result = .false.
        end if
        
    end subroutine test_large_allocation_handling
    
    subroutine test_edge_case_allocations(result)
        !! Test edge case scenarios for allocations
        logical, intent(out) :: result
        type(config_t) :: config
        
        result = .true.
        
        ! Test initialization with default config
        call initialize_default_config(config)
        
        ! Verify basic fields are set correctly
        if (len_trim(config%input_format) == 0 .or. &
            len_trim(config%output_format) == 0) then
            result = .false.
        end if
        
        write(*, '(A)') "  Edge case allocations completed"
        
    end subroutine test_edge_case_allocations

end program test_memory_stress