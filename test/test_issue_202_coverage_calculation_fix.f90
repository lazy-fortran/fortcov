program test_issue_202_coverage_calculation_fix
    !! Test Issue #202: coverage.md shows 0 lines and 100% coverage always
    use coverage_model
    implicit none
    
    integer, parameter :: total_tests = 3
    integer :: passed_tests = 0
    
    write(*, '(A)') "=== Testing Issue #202: Coverage Calculation Fix ==="
    
    ! Test 1: Empty coverage data should show 0.00% not 100.00%
    if (test_empty_coverage_data()) then
        passed_tests = passed_tests + 1
        write(*, '(A)') "✅ Test 1: Empty coverage data shows 0.00% coverage"
    else
        write(*, '(A)') "❌ Test 1: Empty coverage data handling FAILED"
    end if
    
    ! Test 2: Coverage data with executable lines
    if (test_coverage_with_executable_lines()) then
        passed_tests = passed_tests + 1
        write(*, '(A)') "✅ Test 2: Coverage with executable lines calculated correctly"
    else
        write(*, '(A)') "❌ Test 2: Coverage with executable lines FAILED"
    end if
    
    ! Test 3: Mixed executable and non-executable lines
    if (test_mixed_line_types()) then
        passed_tests = passed_tests + 1
        write(*, '(A)') "✅ Test 3: Mixed line types handled correctly"
    else
        write(*, '(A)') "❌ Test 3: Mixed line types FAILED"
    end if
    
    write(*, '(A)') ""
    write(*, '(A, I0, A, I0, A)') "Results: ", passed_tests, "/", total_tests, " tests passed"
    
    if (passed_tests == total_tests) then
        write(*, '(A)') "🎉 All Issue #202 coverage calculation tests PASSED!"
        stop 0
    else
        write(*, '(A)') "💥 Some Issue #202 tests FAILED!"
        stop 1
    end if

contains

    function test_empty_coverage_data() result(success)
        logical :: success
        type(coverage_data_t) :: coverage_data
        type(coverage_file_t) :: empty_file
        type(coverage_line_t), allocatable :: empty_lines(:)
        
        success = .false.
        
        ! Create empty coverage data
        call coverage_data%init()
        allocate(coverage_data%files(1))
        
        ! Initialize with empty lines array
        allocate(empty_lines(0))
        call coverage_data%files(1)%init("empty.f90", empty_lines)
        
        ! Calculate coverage - should be 0 lines, 0.00% coverage
        call coverage_data%files(1)%calculate_coverage()
        
        if (coverage_data%files(1)%total_lines == 0 .and. &
            abs(coverage_data%files(1)%line_coverage) < 0.01) then
            success = .true.
        end if
        
    end function test_empty_coverage_data
    
    function test_coverage_with_executable_lines() result(success)
        logical :: success
        type(coverage_data_t) :: coverage_data
        type(coverage_line_t), allocatable :: test_lines(:)
        
        success = .false.
        
        ! Create test data with executable lines
        call coverage_data%init()
        allocate(coverage_data%files(1))
        allocate(test_lines(2))
        
        ! Line 5: executed 2 times (covered)
        call test_lines(1)%init("test.f90", 5, 2, .true.)
        ! Line 6: not executed (uncovered but executable) 
        call test_lines(2)%init("test.f90", 6, 0, .true.)
        
        call coverage_data%files(1)%init("test.f90", test_lines)
        call coverage_data%files(1)%calculate_coverage()
        
        ! Should have 2 executable lines, 1 covered (50%)
        if (coverage_data%files(1)%total_lines == 2 .and. &
            coverage_data%files(1)%covered_lines == 1 .and. &
            abs(coverage_data%files(1)%line_coverage - 50.0) < 0.01) then
            success = .true.
        end if
        
    end function test_coverage_with_executable_lines
    
    function test_mixed_line_types() result(success)
        logical :: success
        type(coverage_data_t) :: coverage_data
        type(coverage_line_t), allocatable :: mixed_lines(:)
        
        success = .false.
        
        call coverage_data%init()
        allocate(coverage_data%files(1))
        allocate(mixed_lines(3))
        
        ! Mix of executable and non-executable lines
        call mixed_lines(1)%init("mixed.f90", 1, -1, .false.)  ! Non-executable (comment)
        call mixed_lines(2)%init("mixed.f90", 2, 1, .true.)    ! Executable, covered
        call mixed_lines(3)%init("mixed.f90", 3, 0, .true.)    ! Executable, not covered
        
        call coverage_data%files(1)%init("mixed.f90", mixed_lines)
        call coverage_data%files(1)%calculate_coverage()
        
        ! Should have 2 executable lines, 1 covered (50%)
        if (coverage_data%files(1)%total_lines == 2 .and. &
            coverage_data%files(1)%covered_lines == 1 .and. &
            abs(coverage_data%files(1)%line_coverage - 50.0) < 0.01) then
            success = .true.
        end if
        
    end function test_mixed_line_types
    

end program test_issue_202_coverage_calculation_fix