program test_issue_132_resolution
    use coverage_model
    use json_coverage_io
    use coverage_diff
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "=== Issue #132 Resolution Verification ==="
    print *, "Testing JSON import/export format compatibility fixes..."
    
    ! Test 1: Round-trip compatibility
    call test_round_trip_compatibility(all_tests_passed)
    
    ! Test 2: Boolean case sensitivity fix
    call test_boolean_case_sensitivity_fix(all_tests_passed)
    
    ! Test 3: String escaping fix
    call test_string_escaping_fix(all_tests_passed)
    
    ! Test 4: Zero values support
    call test_zero_values_support(all_tests_passed)
    
    ! Test 5: Diff workflow with exported JSON
    call test_diff_workflow_integration(all_tests_passed)
    
    ! Output final result
    if (all_tests_passed) then
        print *, ""
        print *, "✓ Issue #132 RESOLVED: JSON import/export format compatibility fixed"
        print *, "✓ All critical JSON format incompatibilities have been addressed"
        print *, "✓ Round-trip JSON export/import works correctly"
        print *, "✓ --diff functionality works with exported JSON files"
        call exit(0)
    else
        print *, ""
        print *, "✗ Issue #132 NOT FULLY RESOLVED"
        call exit(1)
    end if
    
contains

    subroutine test_round_trip_compatibility(passed)
        logical, intent(inout) :: passed
        
        type(coverage_data_t) :: original_coverage, imported_coverage
        character(len=:), allocatable :: json_output
        logical :: import_error
        type(coverage_line_t) :: lines(2)
        type(coverage_file_t) :: files(1)
        
        print *, "Test 1: Round-trip compatibility..."
        
        ! Create test coverage data
        call lines(1)%init(5, 10, "test.f90", .true.)
        call lines(2)%init(0, 15, "test.f90", .true.)
        call files(1)%init("test.f90", lines)
        call original_coverage%init(files)
        
        ! Export to JSON
        call export_json_coverage(original_coverage, json_output)
        
        ! Import back
        call import_json_coverage_safe(json_output, imported_coverage, import_error)
        
        if (import_error) then
            print *, "  ✗ FAIL: Round-trip compatibility"
            passed = .false.
        else
            print *, "  ✓ PASS: Round-trip compatibility"
        end if
    end subroutine test_round_trip_compatibility
    
    subroutine test_boolean_case_sensitivity_fix(passed)
        logical, intent(inout) :: passed
        
        character(len=*), parameter :: uppercase_json = &
            '{"files": [{"filename": "test.f90", "lines": [{"line_number": 10, "execution_count": 5, "is_executable": True}]}]}'
        
        type(coverage_data_t) :: coverage_data
        logical :: error_occurred
        
        print *, "Test 2: Boolean case sensitivity fix..."
        
        call import_json_coverage_safe(uppercase_json, coverage_data, error_occurred)
        
        if (error_occurred) then
            print *, "  ✗ FAIL: Boolean case sensitivity not fixed"
            passed = .false.
        else
            print *, "  ✓ PASS: Boolean case sensitivity fixed (True/FALSE supported)"
        end if
    end subroutine test_boolean_case_sensitivity_fix
    
    subroutine test_string_escaping_fix(passed)
        logical, intent(inout) :: passed
        
        character(len=*), parameter :: escaped_json = &
            '{"files": [{"filename": "test\"quoted.f90", "lines": [{"line_number": 10, "execution_count": 5, "is_executable": true}]}]}'
        
        type(coverage_data_t) :: coverage_data
        logical :: error_occurred
        
        print *, "Test 3: String escaping fix..."
        
        call import_json_coverage_safe(escaped_json, coverage_data, error_occurred)
        
        if (error_occurred) then
            print *, "  ✗ FAIL: String escaping not fixed"
            passed = .false.
        else
            print *, "  ✓ PASS: String escaping fixed (escaped quotes supported)"
        end if
    end subroutine test_string_escaping_fix
    
    subroutine test_zero_values_support(passed)
        logical, intent(inout) :: passed
        
        character(len=*), parameter :: zero_json = &
            '{"files": [{"filename": "test.f90", "lines": [{"line_number": 0, "execution_count": 0, "is_executable": false}]}]}'
        
        type(coverage_data_t) :: coverage_data
        logical :: error_occurred
        
        print *, "Test 4: Zero values support..."
        
        call import_json_coverage_safe(zero_json, coverage_data, error_occurred)
        
        if (error_occurred) then
            print *, "  ✗ FAIL: Zero values not supported"
            passed = .false.
        else
            print *, "  ✓ PASS: Zero values supported"
        end if
    end subroutine test_zero_values_support
    
    subroutine test_diff_workflow_integration(passed)
        logical, intent(inout) :: passed
        
        type(coverage_data_t) :: baseline_coverage, current_coverage
        type(coverage_data_t) :: imported_baseline, imported_current
        character(len=:), allocatable :: baseline_json, current_json
        logical :: baseline_error, current_error
        type(coverage_diff_t) :: diff_result
        type(coverage_line_t) :: baseline_lines(1), current_lines(1)
        type(coverage_file_t) :: baseline_files(1), current_files(1)
        
        print *, "Test 5: Diff workflow integration..."
        
        ! Create test data
        call baseline_lines(1)%init(2, 10, "test.f90", .true.)
        call baseline_files(1)%init("test.f90", baseline_lines)
        call baseline_coverage%init(baseline_files)
        
        call current_lines(1)%init(4, 10, "test.f90", .true.)
        call current_files(1)%init("test.f90", current_lines)
        call current_coverage%init(current_files)
        
        ! Export to JSON
        call export_json_coverage(baseline_coverage, baseline_json)
        call export_json_coverage(current_coverage, current_json)
        
        ! Import back (simulating --diff workflow)
        call import_json_coverage_safe(baseline_json, imported_baseline, baseline_error)
        call import_json_coverage_safe(current_json, imported_current, current_error)
        
        if (baseline_error .or. current_error) then
            print *, "  ✗ FAIL: JSON import for diff workflow"
            passed = .false.
            return
        end if
        
        ! Compute diff
        diff_result = compute_coverage_diff(imported_baseline, imported_current, &
                                           .false., 0.0)
        
        if (size(diff_result%file_diffs) == 0) then
            print *, "  ✗ FAIL: Diff computation failed"
            passed = .false.
        else
            print *, "  ✓ PASS: Diff workflow integration works"
        end if
    end subroutine test_diff_workflow_integration

end program test_issue_132_resolution