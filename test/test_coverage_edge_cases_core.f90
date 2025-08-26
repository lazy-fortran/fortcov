program test_coverage_edge_cases_core
    !! Core coverage edge case tests
    !! Tests malformed data and boundary conditions
    
    use test_utilities, only: test_runner_t, assert_true, assert_false, &
                              assert_equals_int
    use coverage_types, only: coverage_data_t
    implicit none
    
    type(test_runner_t) :: runner
    
    call runner%init("Coverage Edge Cases Core Tests")
    
    ! Core edge case tests
    call test_malformed_gcov_basic()
    call test_empty_coverage_data()
    call test_invalid_line_numbers()
    call test_boundary_values()
    
    call runner%print_summary()
    
    if (runner%get_pass_rate() == 100.0) then
        call exit(0)
    else
        call exit(1)
    end if
    
contains
    
    subroutine test_malformed_gcov_basic()
        !! Test handling of malformed gcov data
        type(coverage_data_t) :: coverage
        logical :: passed, success
        
        passed = .true.
        
        ! Test empty file
        call coverage%initialize()
        success = coverage%parse_gcov_line("")
        call assert_false(success, &
            "Empty line should fail parsing", passed)
        
        ! Test invalid format
        success = coverage%parse_gcov_line("invalid format")
        call assert_false(success, &
            "Invalid format should fail parsing", passed)
        
        ! Test missing columns
        success = coverage%parse_gcov_line("    5:")
        call assert_false(success, &
            "Missing columns should fail parsing", passed)
        
        call runner%run_test("malformed_gcov_basic", passed)
    end subroutine test_malformed_gcov_basic
    
    subroutine test_empty_coverage_data()
        !! Test handling of empty coverage data
        type(coverage_data_t) :: coverage
        logical :: passed
        real :: percentage
        
        passed = .true.
        
        call coverage%initialize()
        percentage = coverage%get_coverage_percentage()
        
        ! Empty coverage should return 0%
        call assert_equals_int(int(percentage), 0, &
            "Empty coverage should be 0%", passed)
        
        call runner%run_test("empty_coverage_data", passed)
    end subroutine test_empty_coverage_data
    
    subroutine test_invalid_line_numbers()
        !! Test handling of invalid line numbers
        type(coverage_data_t) :: coverage
        logical :: passed, success
        
        passed = .true.
        
        call coverage%initialize()
        
        ! Test negative line numbers
        success = coverage%add_line_coverage(-1, 1)
        call assert_false(success, &
            "Negative line numbers should fail", passed)
        
        ! Test zero line number
        success = coverage%add_line_coverage(0, 1)
        call assert_false(success, &
            "Zero line number should fail", passed)
        
        ! Test very large line numbers
        success = coverage%add_line_coverage(999999, 1)
        call assert_true(success, &
            "Large line numbers should be handled", passed)
        
        call runner%run_test("invalid_line_numbers", passed)
    end subroutine test_invalid_line_numbers
    
    subroutine test_boundary_values()
        !! Test boundary value conditions
        type(coverage_data_t) :: coverage
        logical :: passed
        integer :: max_hits
        
        passed = .true.
        max_hits = huge(0)
        
        call coverage%initialize()
        
        ! Test maximum hit count
        call coverage%add_line_coverage(1, max_hits)
        call assert_equals_int(coverage%get_line_hits(1), max_hits, &
            "Should handle maximum hit count", passed)
        
        ! Test zero hits
        call coverage%add_line_coverage(2, 0)
        call assert_equals_int(coverage%get_line_hits(2), 0, &
            "Should handle zero hits", passed)
        
        call runner%run_test("boundary_values", passed)
    end subroutine test_boundary_values
    
end program test_coverage_edge_cases_core