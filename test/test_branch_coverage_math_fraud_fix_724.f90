program test_branch_coverage_math_fraud_fix_724
    !! Test fix for Issue #724: Branch coverage calculation mathematical fraud
    !!
    !! Tests the fix for 0/0 = 100% branch coverage fraud and proper
    !! conditional statement detection improvements.
    
    use coverage_model_core
    use system_diff, only: calculate_branch_coverage_rate
    use coverage_stats_core, only: calculate_branch_coverage
    use input_validation_core, only: safe_percentage_calculation
    implicit none
    
    integer :: total_tests = 0
    integer :: passed_tests = 0
    logical :: all_tests_passed = .true.
    
    print *, "============================================================="
    print *, "Issue #724: Branch Coverage Mathematical Fraud Fix Tests"
    print *, "============================================================="
    print *, ""
    
    ! Test the mathematical fraud fix
    call test_zero_branches_returns_zero_percent()
    call test_partial_branch_coverage_math()
    call test_full_branch_coverage_math()
    call test_safe_percentage_calculation_zero_division()
    
    ! Test branch coverage calculation uses proper branch data (not flawed line parsing)
    call test_proper_branch_data_usage()
    
    ! Summary
    print *, ""
    print *, "============================================================="
    if (all_tests_passed) then
        print '(A,I0,A,I0,A)', "✓ ALL MATHEMATICAL FRAUD TESTS PASSED (", passed_tests, "/", total_tests, ")"
        print *, "Issue #724: MATHEMATICAL CORRECTNESS ACHIEVED"
        call exit(0)
    else
        print '(A,I0,A,I0,A)', "✗ MATHEMATICAL FRAUD TESTS FAILED (", passed_tests, "/", total_tests, " passed)"
        call exit(1)
    end if
    
contains

    subroutine test_zero_branches_returns_zero_percent()
        !! Test: 0 branches should return 0% coverage, not 100% (mathematical fraud)
        type(coverage_data_t) :: empty_coverage
        real :: branch_rate
        
        print *, "Test: Zero branches returns 0% (not 100% fraud)"
        total_tests = total_tests + 1
        
        ! Create minimal coverage data with no branch data
        ! Don't call init() if it causes allocation issues - just work with empty structure
        empty_coverage%version = "1.0"
        empty_coverage%tool = "fortcov"
        
        ! Allocate minimal structure without branches
        allocate(empty_coverage%files(1))
        call empty_coverage%files(1)%init("test.f90")
        
        ! Calculate branch rate - should be 0.0, not 1.0 (mathematical correctness)
        branch_rate = calculate_branch_coverage_rate(empty_coverage)
        
        if (abs(branch_rate - 0.0) < 1e-6) then
            print *, "  ✓ PASS: Zero branches correctly returns 0% coverage"
            passed_tests = passed_tests + 1
        else
            print '(A,F6.2,A)', "  ✗ FAIL: Zero branches returns ", branch_rate * 100.0, "% instead of 0%"
            all_tests_passed = .false.
        end if
    end subroutine test_zero_branches_returns_zero_percent
    
    subroutine test_partial_branch_coverage_math()
        !! Test: Partial branch coverage calculates correctly
        type(coverage_data_t) :: partial_coverage
        real :: branch_rate
        
        print *, "Test: Partial branch coverage mathematical accuracy"
        total_tests = total_tests + 1
        
        partial_coverage%version = "1.0"
        partial_coverage%tool = "fortcov"
        allocate(partial_coverage%files(1))
        call partial_coverage%files(1)%init("test.f90")
        
        ! Add function with mixed branch coverage
        allocate(partial_coverage%files(1)%functions(1))
        call partial_coverage%files(1)%functions(1)%init("mixed_func", "test.f90", 1)
        
        ! Add branches: 1 taken, 1 not taken
        allocate(partial_coverage%files(1)%functions(1)%branches(2))
        call partial_coverage%files(1)%functions(1)%branches(1)%init("test.f90", 3, 1, 1, 0)  ! taken
        call partial_coverage%files(1)%functions(1)%branches(2)%init("test.f90", 5, 2, 0, 1)  ! not taken
        
        ! Calculate branch rate - should be 50% (1 out of 2 branches covered)
        branch_rate = calculate_branch_coverage_rate(partial_coverage)
        
        if (abs(branch_rate - 0.5) < 1e-6) then
            print *, "  ✓ PASS: Partial branch coverage correctly calculates 50%"
            passed_tests = passed_tests + 1
        else
            print '(A,F6.2,A)', "  ✗ FAIL: Partial branch coverage returns ", branch_rate * 100.0, "% instead of 50%"
            all_tests_passed = .false.
        end if
    end subroutine test_partial_branch_coverage_math
    
    subroutine test_full_branch_coverage_math()
        !! Test: Full branch coverage calculates to 100%
        type(coverage_data_t) :: full_coverage
        real :: branch_rate
        
        print *, "Test: Full branch coverage mathematical accuracy"
        total_tests = total_tests + 1
        
        full_coverage%version = "1.0"
        full_coverage%tool = "fortcov"
        allocate(full_coverage%files(1))
        call full_coverage%files(1)%init("test.f90")
        
        ! Add function with all branches covered
        allocate(full_coverage%files(1)%functions(1))
        call full_coverage%files(1)%functions(1)%init("full_func", "test.f90", 1)
        
        ! Add branches: both taken
        allocate(full_coverage%files(1)%functions(1)%branches(2))
        call full_coverage%files(1)%functions(1)%branches(1)%init("test.f90", 3, 1, 1, 0)  ! taken
        call full_coverage%files(1)%functions(1)%branches(2)%init("test.f90", 5, 2, 1, 0)  ! also taken
        
        ! Calculate branch rate - should be 100% (2 out of 2 branches covered)
        branch_rate = calculate_branch_coverage_rate(full_coverage)
        
        if (abs(branch_rate - 1.0) < 1e-6) then
            print *, "  ✓ PASS: Full branch coverage correctly calculates 100%"
            passed_tests = passed_tests + 1
        else
            print '(A,F6.2,A)', "  ✗ FAIL: Full branch coverage returns ", branch_rate * 100.0, "% instead of 100%"
            all_tests_passed = .false.
        end if
    end subroutine test_full_branch_coverage_math
    
    subroutine test_safe_percentage_calculation_zero_division()
        !! Test: safe_percentage_calculation handles 0/0 correctly
        real :: result
        
        print *, "Test: Safe percentage calculation handles 0/0 = 0%"
        total_tests = total_tests + 1
        
        ! Test 0/0 case
        result = safe_percentage_calculation(0, 0)
        
        if (abs(result - 0.0) < 1e-6) then
            print *, "  ✓ PASS: safe_percentage_calculation(0, 0) = 0%"
            passed_tests = passed_tests + 1
        else
            print '(A,F6.2,A)', "  ✗ FAIL: safe_percentage_calculation(0, 0) = ", result, "% instead of 0%"
            all_tests_passed = .false.
        end if
        
        ! Test normal case for comparison
        total_tests = total_tests + 1
        result = safe_percentage_calculation(3, 4)
        if (abs(result - 75.0) < 1e-6) then
            print *, "  ✓ PASS: safe_percentage_calculation(3, 4) = 75%"
            passed_tests = passed_tests + 1
        else
            print '(A,F6.2,A)', "  ✗ FAIL: safe_percentage_calculation(3, 4) = ", result, "% instead of 75%"
            all_tests_passed = .false.
        end if
    end subroutine test_safe_percentage_calculation_zero_division
    
    subroutine test_proper_branch_data_usage()
        !! Test: Branch coverage calculation uses proper gcov branch data, not flawed line parsing
        type(coverage_data_t) :: coverage_with_branches
        type(coverage_function_t) :: test_function
        real :: branch_rate
        
        print *, "Test: Proper branch data usage (not line parsing)"
        total_tests = total_tests + 1
        
        ! Create coverage data with proper branch structure
        coverage_with_branches%version = "1.0"
        coverage_with_branches%tool = "fortcov"
        allocate(coverage_with_branches%files(1))
        call coverage_with_branches%files(1)%init("test_branches.f90")
        
        ! Add function with branches (simulating gcov branch data)
        allocate(coverage_with_branches%files(1)%functions(1))
        call coverage_with_branches%files(1)%functions(1)%init("test_func", "test_branches.f90", 1)
        
        ! Add branch data (this is how real branch coverage works)
        allocate(coverage_with_branches%files(1)%functions(1)%branches(2))
        call coverage_with_branches%files(1)%functions(1)%branches(1)%init("test_branches.f90", 3, 1, 1, 0)  ! taken
        call coverage_with_branches%files(1)%functions(1)%branches(2)%init("test_branches.f90", 5, 2, 0, 1)  ! not taken
        
        ! Calculate branch rate - should be 50% (1 out of 2 branches taken)
        branch_rate = calculate_branch_coverage_rate(coverage_with_branches)
        
        if (abs(branch_rate - 0.5) < 1e-6) then
            print *, "  ✓ PASS: Uses proper gcov branch data for calculation"
            passed_tests = passed_tests + 1
        else
            print '(A,F6.2,A)', "  ✗ FAIL: Branch calculation returns ", branch_rate * 100.0, "% instead of 50%"
            all_tests_passed = .false.
        end if
    end subroutine test_proper_branch_data_usage

end program test_branch_coverage_math_fraud_fix_724
