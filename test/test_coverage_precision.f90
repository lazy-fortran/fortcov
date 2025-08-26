program test_coverage_precision
    !! Coverage calculation precision tests
    !! Tests accurate percentage calculations and rounding
    
    use test_utilities, only: test_runner_t, assert_true
    use coverage_types, only: coverage_data_t
    implicit none
    
    type(test_runner_t) :: runner
    real :: tolerance = 0.001  ! Precision tolerance
    
    call runner%init("Coverage Precision Tests")
    
    ! Precision tests
    call test_percentage_calculation()
    call test_rounding_behavior()
    call test_edge_percentages()
    call test_large_file_precision()
    
    call runner%print_summary()
    
    if (runner%get_pass_rate() == 100.0) then
        call exit(0)
    else
        call exit(1)
    end if
    
contains
    
    subroutine test_percentage_calculation()
        !! Test accurate percentage calculations
        type(coverage_data_t) :: coverage
        logical :: passed
        real :: percentage, expected
        
        passed = .true.
        
        call coverage%initialize()
        call coverage%add_line_coverage(1, 1)  ! Covered
        call coverage%add_line_coverage(2, 0)  ! Not covered
        call coverage%add_line_coverage(3, 1)  ! Covered
        
        percentage = coverage%get_coverage_percentage()
        expected = 66.667  ! 2 out of 3 lines
        
        call assert_true(abs(percentage - expected) < tolerance, &
            "Should calculate 2/3 coverage accurately", passed)
        
        call runner%run_test("percentage_calculation", passed)
    end subroutine test_percentage_calculation
    
    subroutine test_rounding_behavior()
        !! Test proper rounding of percentages
        type(coverage_data_t) :: coverage
        logical :: passed
        real :: percentage
        
        passed = .true.
        
        ! Test 1/3 coverage (33.333...)
        call coverage%initialize()
        call coverage%add_line_coverage(1, 1)
        call coverage%add_line_coverage(2, 0)
        call coverage%add_line_coverage(3, 0)
        
        percentage = coverage%get_coverage_percentage()
        call assert_true(abs(percentage - 33.333) < tolerance, &
            "Should round 1/3 coverage properly", passed)
        
        call runner%run_test("rounding_behavior", passed)
    end subroutine test_rounding_behavior
    
    subroutine test_edge_percentages()
        !! Test edge percentage values
        type(coverage_data_t) :: coverage
        logical :: passed
        real :: percentage
        
        passed = .true.
        
        ! Test 100% coverage
        call coverage%initialize()
        call coverage%add_line_coverage(1, 1)
        call coverage%add_line_coverage(2, 5)
        call coverage%add_line_coverage(3, 10)
        
        percentage = coverage%get_coverage_percentage()
        call assert_true(abs(percentage - 100.0) < tolerance, &
            "Should handle 100% coverage", passed)
        
        ! Test 0% coverage
        call coverage%initialize()
        call coverage%add_line_coverage(1, 0)
        call coverage%add_line_coverage(2, 0)
        
        percentage = coverage%get_coverage_percentage()
        call assert_true(abs(percentage - 0.0) < tolerance, &
            "Should handle 0% coverage", passed)
        
        call runner%run_test("edge_percentages", passed)
    end subroutine test_edge_percentages
    
    subroutine test_large_file_precision()
        !! Test precision with large numbers of lines
        type(coverage_data_t) :: coverage
        logical :: passed
        real :: percentage, expected
        integer :: i, covered_lines, total_lines
        
        passed = .true.
        total_lines = 10000
        covered_lines = 7523  ! Arbitrary coverage
        
        call coverage%initialize()
        
        ! Add covered lines
        do i = 1, covered_lines
            call coverage%add_line_coverage(i, 1)
        end do
        
        ! Add uncovered lines
        do i = covered_lines + 1, total_lines
            call coverage%add_line_coverage(i, 0)
        end do
        
        percentage = coverage%get_coverage_percentage()
        expected = real(covered_lines) / real(total_lines) * 100.0
        
        call assert_true(abs(percentage - expected) < tolerance, &
            "Should maintain precision with large files", passed)
        
        call runner%run_test("large_file_precision", passed)
    end subroutine test_large_file_precision
    
end program test_coverage_precision