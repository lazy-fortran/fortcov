program test_coverage_model_comprehensive
    use coverage_model
    use coverage_diff
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing Coverage Model - Comprehensive Behavioral Tests..."
    
    ! Comprehensive threshold-based diff analysis tests
    all_tests_passed = all_tests_passed .and. test_diff_threshold_classification()
    all_tests_passed = all_tests_passed .and. test_statistical_confidence_calculation()
    all_tests_passed = all_tests_passed .and. test_coverage_delta_edge_cases()
    all_tests_passed = all_tests_passed .and. test_filter_by_threshold_behavior()
    
    ! Edge case tests for complex calculations
    all_tests_passed = all_tests_passed .and. test_file_coverage_zero_executable_lines()
    all_tests_passed = all_tests_passed .and. test_serialization_security_limits()
    all_tests_passed = all_tests_passed .and. test_branch_coverage_logic_edge_cases()
    
    ! Error condition and boundary tests
    all_tests_passed = all_tests_passed .and. test_negative_execution_counts()
    all_tests_passed = all_tests_passed .and. test_empty_file_arrays()
    all_tests_passed = all_tests_passed .and. test_diff_calculation_precision()

    if (all_tests_passed) then
        print *, "All comprehensive coverage model tests PASSED"
        call exit(0)
    else
        print *, "Some comprehensive coverage model tests FAILED"
        call exit(1)
    end if

contains

    function test_diff_threshold_classification() result(passed)
        logical :: passed
        type(diff_thresholds_t) :: thresholds
        integer :: class_critical_improve, class_major_degrade, class_unchanged, class_new
        logical :: test1, test2, test3, test4
        
        print *, "  Test: Threshold-based classification behavior"
        print *, "    Given: Different coverage percentage changes with threshold configuration"
        print *, "    When: Classifying changes using threshold algorithm"
        print *, "    Then: Should correctly classify based on magnitude and direction"
        
        ! Initialize thresholds with known values
        call thresholds%init(critical=5.0, major=2.0, minor=0.5, significance=0.1)
        
        ! Test 1: Critical improvement (baseline 50% -> current 60%)
        class_critical_improve = thresholds%classify_change(50.0, 60.0)
        test1 = (class_critical_improve == CRITICAL_IMPROVEMENT)
        
        ! Test 2: Major degradation (baseline 80% -> current 77%)
        class_major_degrade = thresholds%classify_change(80.0, 77.0)
        test2 = (class_major_degrade == MAJOR_DEGRADATION)
        
        ! Test 3: Unchanged within significance threshold (baseline 70% -> current 70.05%)
        class_unchanged = thresholds%classify_change(70.0, 70.05)
        test3 = (class_unchanged == UNCHANGED_COVERAGE)
        
        ! Test 4: New coverage (baseline 0% -> current 25%)
        class_new = thresholds%classify_change(0.0, 25.0)
        test4 = (class_new == NEW_COVERAGE)
        
        passed = test1 .and. test2 .and. test3 .and. test4
        
        if (.not. passed) then
            print *, "    FAILED: Threshold classification not working correctly"
            print *, "      Critical improvement:", test1, " Got:", class_critical_improve
            print *, "      Major degradation:", test2, " Got:", class_major_degrade
            print *, "      Unchanged:", test3, " Got:", class_unchanged
            print *, "      New coverage:", test4, " Got:", class_new
        else
            print *, "    PASSED: Threshold classification working correctly"
        end if
    end function test_diff_threshold_classification

    function test_statistical_confidence_calculation() result(passed)
        logical :: passed
        type(file_diff_t) :: file_diff
        type(line_diff_t), allocatable :: line_diffs(:)
        type(coverage_line_t) :: baseline_line, current_line
        type(diff_thresholds_t) :: thresholds
        real :: confidence
        logical :: test1, test2
        integer :: i
        
        print *, "  Test: Statistical confidence calculation"
        print *, "    Given: Files with different numbers of lines and coverage changes"
        print *, "    When: Calculating statistical confidence for file diffs"
        print *, "    Then: Confidence should increase with sample size and change magnitude"
        
        ! Create test line diffs
        allocate(line_diffs(100))  ! Large sample size
        
        ! Initialize baseline and current lines
        call baseline_line%init("test.f90", 1, 0, .true.)
        call current_line%init("test.f90", 1, 5, .true.)
        
        ! Fill with identical line diffs
        do i = 1, 100
            line_diffs(i) = line_diff_t(baseline_line, current_line, DIFF_CHANGED)
        end do
        
        ! Initialize file diff
        call file_diff%init("test.f90")
        file_diff%lines = line_diffs
        file_diff%baseline_coverage_percentage = 0.0
        file_diff%current_coverage_percentage = 50.0
        file_diff%coverage_percentage_delta = 50.0
        
        ! Apply threshold analysis
        call thresholds%init()
        call file_diff%apply_threshold_analysis(thresholds)
        
        confidence = file_diff%statistical_confidence
        
        ! Test 1: Confidence should be reasonable (between 0 and 1)
        test1 = (confidence >= 0.0 .and. confidence <= 1.0)
        
        ! Test 2: Large sample size with significant change should have high confidence
        test2 = (confidence > 0.5)  ! Should be reasonably confident
        
        passed = test1 .and. test2
        
        if (.not. passed) then
            print *, "    FAILED: Statistical confidence calculation incorrect"
            print *, "      Confidence in range:", test1, " Got:", confidence
            print *, "      High confidence for large change:", test2
        else
            print *, "    PASSED: Statistical confidence calculation working correctly"
        end if
    end function test_statistical_confidence_calculation

    function test_coverage_delta_edge_cases() result(passed)
        logical :: passed
        type(line_diff_t) :: diff1, diff2, diff3
        type(coverage_line_t) :: baseline, current1, current2, current3
        logical :: test1, test2, test3
        
        print *, "  Test: Coverage delta calculation edge cases"
        print *, "    Given: Various edge cases like zero to zero, negative counts"
        print *, "    When: Calculating coverage deltas"
        print *, "    Then: Should handle edge cases correctly without division errors"
        
        ! Create baseline line
        baseline = coverage_line_t(0, 1, "test.f90", .true.)
        
        ! Test 1: Zero to zero (should not be newly covered/uncovered)
        current1 = coverage_line_t(0, 1, "test.f90", .true.)
        diff1 = line_diff_t(baseline, current1, DIFF_UNCHANGED)
        test1 = (.not. diff1%is_newly_covered .and. .not. diff1%is_newly_uncovered)
        
        ! Test 2: Zero to positive (should be newly covered)
        current2 = coverage_line_t(5, 1, "test.f90", .true.)
        diff2 = line_diff_t(baseline, current2, DIFF_CHANGED)
        test2 = (diff2%is_newly_covered .and. .not. diff2%is_newly_uncovered)
        
        ! Test 3: Positive to zero (should be newly uncovered)
        baseline = coverage_line_t(10, 1, "test.f90", .true.)
        current3 = coverage_line_t(0, 1, "test.f90", .true.)
        diff3 = line_diff_t(baseline, current3, DIFF_CHANGED)
        test3 = (.not. diff3%is_newly_covered .and. diff3%is_newly_uncovered)
        
        passed = test1 .and. test2 .and. test3
        
        if (.not. passed) then
            print *, "    FAILED: Coverage delta edge cases not handled correctly"
            print *, "      Zero to zero:", test1
            print *, "      Zero to positive (newly covered):", test2
            print *, "      Positive to zero (newly uncovered):", test3
        else
            print *, "    PASSED: Coverage delta edge cases handled correctly"
        end if
    end function test_coverage_delta_edge_cases

    function test_filter_by_threshold_behavior() result(passed)
        logical :: passed
        type(coverage_diff_t) :: diff
        type(file_diff_t), allocatable :: file_diffs(:)
        type(line_diff_t), allocatable :: line_diffs(:)
        type(coverage_line_t) :: baseline, current
        integer :: original_size, filtered_size
        logical :: test1, test2
        
        print *, "  Test: Threshold filtering behavior"
        print *, "    Given: Coverage diff with files having various change magnitudes"
        print *, "    When: Filtering by significance threshold"
        print *, "    Then: Only files exceeding threshold should remain"
        
        ! Create file diffs with different change magnitudes
        allocate(file_diffs(3))
        allocate(line_diffs(1))
        
        baseline = coverage_line_t(5, 1, "test.f90", .true.)
        current = coverage_line_t(5, 1, "test.f90", .true.)
        line_diffs(1) = line_diff_t(baseline, current, DIFF_UNCHANGED)
        
        ! File 1: No change (should be filtered out)
        call file_diffs(1)%init("file1.f90", line_diffs)
        file_diffs(1)%baseline_coverage_percentage = 50.0
        file_diffs(1)%current_coverage_percentage = 50.0
        file_diffs(1)%coverage_percentage_delta = 0.0
        
        ! File 2: Small change (should be filtered out with high threshold)
        call file_diffs(2)%init("file2.f90", line_diffs)
        file_diffs(2)%baseline_coverage_percentage = 50.0
        file_diffs(2)%current_coverage_percentage = 50.5
        file_diffs(2)%coverage_percentage_delta = 0.5
        
        ! File 3: Large change (should remain)
        call file_diffs(3)%init("file3.f90", line_diffs)
        file_diffs(3)%baseline_coverage_percentage = 50.0
        file_diffs(3)%current_coverage_percentage = 60.0
        file_diffs(3)%coverage_percentage_delta = 10.0
        
        call diff%init(file_diffs, threshold=2.0)
        original_size = size(diff%file_diffs)
        
        call diff%filter_by_threshold()
        filtered_size = size(diff%file_diffs)
        
        ! Test 1: Original size should be 3
        test1 = (original_size == 3)
        
        ! Test 2: After filtering with threshold=2.0, only 1 file should remain
        test2 = (filtered_size == 1 .and. diff%file_diffs(1)%filename == "file3.f90")
        
        passed = test1 .and. test2
        
        if (.not. passed) then
            print *, "    FAILED: Threshold filtering not working correctly"
            print *, "      Original size:", test1, " Got:", original_size
            print *, "      Filtered correctly:", test2, " Size:", filtered_size
        else
            print *, "    PASSED: Threshold filtering working correctly"
        end if
    end function test_filter_by_threshold_behavior

    function test_file_coverage_zero_executable_lines() result(passed)
        logical :: passed
        type(coverage_file_t) :: file_cov
        type(coverage_line_t), allocatable :: lines(:)
        real :: percentage
        integer :: executable_count, covered_count
        logical :: test1, test2, test3
        
        print *, "  Test: File coverage with zero executable lines"
        print *, "    Given: File with no executable lines (comments only)"
        print *, "    When: Calculating coverage statistics"
        print *, "    Then: Should return 0% coverage without division by zero"
        
        ! Create file with non-executable lines only
        allocate(lines(3))
        lines(1) = coverage_line_t(0, 1, "test.f90", .false.)  ! Non-executable
        lines(2) = coverage_line_t(0, 2, "test.f90", .false.)  ! Non-executable  
        lines(3) = coverage_line_t(0, 3, "test.f90", .false.)  ! Non-executable
        
        file_cov = coverage_file_t(filename="test.f90", lines=lines)
        
        percentage = file_cov%get_line_coverage_percentage()
        executable_count = file_cov%get_executable_line_count()
        covered_count = file_cov%get_covered_line_count()
        
        ! Test 1: Coverage percentage should be 0.0
        test1 = (abs(percentage - 0.0) < 0.001)
        
        ! Test 2: Executable line count should be 0
        test2 = (executable_count == 0)
        
        ! Test 3: Covered line count should be 0
        test3 = (covered_count == 0)
        
        passed = test1 .and. test2 .and. test3
        
        if (.not. passed) then
            print *, "    FAILED: Zero executable lines not handled correctly"
            print *, "      Zero percentage:", test1, " Got:", percentage
            print *, "      Zero executable count:", test2, " Got:", executable_count
            print *, "      Zero covered count:", test3, " Got:", covered_count
        else
            print *, "    PASSED: Zero executable lines handled correctly"
        end if
    end function test_file_coverage_zero_executable_lines

    function test_serialization_security_limits() result(passed)
        logical :: passed
        type(coverage_data_t) :: data
        character(len=:), allocatable :: serialized
        type(coverage_file_t), allocatable :: files(:)
        type(coverage_line_t), allocatable :: lines(:)
        character(len=5000) :: very_long_filename
        logical :: test1, test2
        
        print *, "  Test: Serialization security limits"
        print *, "    Given: Coverage data with extremely long filename"
        print *, "    When: Serializing data"
        print *, "    Then: Should enforce security limits and not crash"
        
        ! Create extremely long filename (> 4096 chars)
        very_long_filename = repeat("a", 5000)
        
        allocate(lines(1))
        lines(1) = coverage_line_t(1, 1, very_long_filename, .true.)
        
        allocate(files(1))
        files(1) = coverage_file_t(filename=very_long_filename, lines=lines)
        
        data = coverage_data_t(files=files)
        serialized = data%serialize()
        
        ! Test 1: Should not crash (length > 0)
        test1 = (len(serialized) > 0)
        
        ! Test 2: Should contain security error message
        test2 = (index(serialized, "ERROR: Filename too long for security") > 0)
        
        passed = test1 .and. test2
        
        if (.not. passed) then
            print *, "    FAILED: Serialization security limits not enforced"
            print *, "      No crash:", test1, " Length:", len(serialized)
            print *, "      Security error:", test2
        else
            print *, "    PASSED: Serialization security limits enforced correctly"
        end if
    end function test_serialization_security_limits

    function test_branch_coverage_logic_edge_cases() result(passed)
        logical :: passed
        type(coverage_branch_t) :: branch1, branch2, branch3
        logical :: test1, test2, test3, test4, test5, test6
        
        print *, "  Test: Branch coverage logic edge cases"
        print *, "    Given: Branches with edge case taken/not_taken counts"
        print *, "    When: Checking partial and full coverage"
        print *, "    Then: Logic should handle all edge cases correctly"
        
        ! Test case 1: taken=0, not_taken=0 (no execution)
        branch1 = coverage_branch_t(0, 0, 1, 10, "test.f90")
        test1 = (.not. branch1%is_partially_covered())
        test2 = (.not. branch1%is_fully_covered())
        
        ! Test case 2: taken>0, not_taken=0 (partial coverage)
        branch2 = coverage_branch_t(5, 0, 2, 20, "test.f90")
        test3 = (branch2%is_partially_covered())
        test4 = (.not. branch2%is_fully_covered())
        
        ! Test case 3: taken=0, not_taken>0 (not standard, but should handle)
        branch3 = coverage_branch_t(0, 3, 3, 30, "test.f90")
        test5 = (.not. branch3%is_partially_covered())
        test6 = (.not. branch3%is_fully_covered())
        
        passed = test1 .and. test2 .and. test3 .and. test4 .and. test5 .and. test6
        
        if (.not. passed) then
            print *, "    FAILED: Branch coverage logic edge cases incorrect"
            print *, "      No execution - not partial:", test1
            print *, "      No execution - not full:", test2
            print *, "      Taken only - is partial:", test3
            print *, "      Taken only - not full:", test4
            print *, "      Not taken only - not partial:", test5
            print *, "      Not taken only - not full:", test6
        else
            print *, "    PASSED: Branch coverage logic edge cases correct"
        end if
    end function test_branch_coverage_logic_edge_cases

    function test_negative_execution_counts() result(passed)
        logical :: passed
        type(coverage_line_t) :: line_cov
        type(coverage_branch_t) :: branch_cov
        logical :: test1, test2
        
        print *, "  Test: Negative execution counts handling"
        print *, "    Given: Potentially negative execution counts from corrupted data"
        print *, "    When: Creating coverage objects"
        print *, "    Then: Should handle gracefully (current implementation allows negatives)"
        
        ! Test 1: Negative line execution count
        line_cov = coverage_line_t(-5, 1, "test.f90", .true.)
        test1 = (line_cov%execution_count == -5)  ! Current implementation allows this
        
        ! Test 2: Negative branch counts
        branch_cov = coverage_branch_t(-2, -3, 1, 10, "test.f90")
        test2 = (branch_cov%taken_count == -2 .and. branch_cov%not_taken_count == -3)
        
        passed = test1 .and. test2
        
        if (.not. passed) then
            print *, "    FAILED: Negative execution counts not handled as expected"
        else
            print *, "    PASSED: Negative execution counts handled (note: validation may be needed)"
        end if
    end function test_negative_execution_counts

    function test_empty_file_arrays() result(passed)
        logical :: passed
        type(coverage_data_t) :: data1, data2
        type(coverage_file_t), allocatable :: empty_files(:)
        logical :: test1, test2
        
        print *, "  Test: Empty file arrays handling"
        print *, "    Given: Coverage data with empty file arrays"
        print *, "    When: Initializing and accessing coverage data"
        print *, "    Then: Should handle empty arrays without crashing"
        
        ! Test 1: Initialize with empty array
        allocate(empty_files(0))
        data1 = coverage_data_t(files=empty_files)
        test1 = (allocated(data1%files) .and. size(data1%files) == 0)
        
        ! Test 2: Default initialization should create empty array
        data2 = coverage_data_t()
        test2 = (allocated(data2%files) .and. size(data2%files) == 0)
        
        passed = test1 .and. test2
        
        if (.not. passed) then
            print *, "    FAILED: Empty file arrays not handled correctly"
            print *, "      Empty array init:", test1
            print *, "      Default init:", test2
        else
            print *, "    PASSED: Empty file arrays handled correctly"
        end if
    end function test_empty_file_arrays

    function test_diff_calculation_precision() result(passed)
        logical :: passed
        type(line_diff_t) :: diff
        type(coverage_line_t) :: baseline, current
        real, parameter :: tolerance = 1.0e-6
        logical :: test1, test2
        
        print *, "  Test: Diff calculation precision"
        print *, "    Given: Lines with large execution counts"
        print *, "    When: Calculating execution count deltas"
        print *, "    Then: Should maintain precision for large numbers"
        
        ! Test with large execution counts
        baseline = coverage_line_t(2147483647, 1, "test.f90", .true.)  ! Near max int
        current = coverage_line_t(2147483640, 1, "test.f90", .true.)
        
        diff = line_diff_t(baseline, current, DIFF_CHANGED)
        
        ! Test 1: Delta calculation should be precise
        test1 = (diff%execution_count_delta == -7)
        
        ! Test 2: Should NOT be newly uncovered (still >0 execution count)
        test2 = (.not. diff%is_newly_covered .and. .not. diff%is_newly_uncovered)
        
        passed = test1 .and. test2
        
        if (.not. passed) then
            print *, "    FAILED: Diff calculation precision issues"
            print *, "      Delta precision:", test1, " Got:", diff%execution_count_delta
            print *, "      Newly uncovered:", test2
        else
            print *, "    PASSED: Diff calculation precision maintained"
        end if
    end function test_diff_calculation_precision

end program test_coverage_model_comprehensive