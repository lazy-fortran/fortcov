program test_issue_470_verification
    !! Verification test for Issue #470 fix
    !! 
    !! Tests that coverage parsing now shows correct percentages instead of 0.00%
    use coverage_file_processor, only: parse_coverage_files
    use coverage_statistics, only: calculate_line_coverage, coverage_stats_t
    use coverage_data_model, only: coverage_data_t
    use fortcov_config, only: config_t
    implicit none
    
    type(config_t) :: config
    character(len=1024) :: files(1)
    type(coverage_data_t) :: merged_coverage
    type(coverage_stats_t) :: stats
    logical :: parse_error
    
    ! Initialize config
    config%quiet = .true.  ! Suppress output for clean test
    
    ! Test file
    files(1) = "examples/build_systems/fpm/basic_example/demo_calculator.f90.gcov"
    
    print *, "Testing Issue #470 fix..."
    
    ! Parse coverage files using the fixed functionality
    call parse_coverage_files(files, config, merged_coverage, parse_error)
    
    if (parse_error) then
        print *, "❌ ERROR: Failed to parse coverage files"
        stop 1
    end if
    
    ! Calculate coverage statistics
    stats = calculate_line_coverage(merged_coverage)
    
    ! Verify results
    if (stats%percentage <= 0.01 .and. stats%total_count == 0) then
        print *, "❌ BUG NOT FIXED: Still shows 0.00% coverage with 0 total lines"
        print *, "   Percentage:", stats%percentage, "%"
        print *, "   Covered:", stats%covered_count
        print *, "   Total:", stats%total_count
        stop 1
    else if (stats%percentage > 50.0 .and. stats%total_count > 0) then
        print *, "✅ BUG FIXED: Shows correct coverage percentage"
        print *, "   Percentage:", stats%percentage, "%"
        print *, "   Covered:", stats%covered_count, "of", stats%total_count, "lines"
        stop 0
    else
        print *, "⚠️  Unexpected result:"
        print *, "   Percentage:", stats%percentage, "%"
        print *, "   Covered:", stats%covered_count, "of", stats%total_count, "lines"
        stop 1
    end if

end program test_issue_470_verification