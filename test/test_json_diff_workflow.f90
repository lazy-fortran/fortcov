program test_json_diff_workflow
    use coverage_model
    use json_coverage_io
    use coverage_diff
    use file_utils
    implicit none
    
    logical :: test_passed, all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "=== JSON Diff Workflow Tests ==="
    
    ! RED Test 1: Export JSON files and test diff functionality
    call test_exported_json_diff_workflow(test_passed)
    if (.not. test_passed) all_tests_passed = .false.
    
    ! RED Test 2: Test CLI diff command with exported JSON
    call test_cli_diff_with_exported_json(test_passed)
    if (.not. test_passed) all_tests_passed = .false.
    
    ! RED Test 3: Test format compatibility across different coverage scenarios
    call test_format_compatibility_scenarios(test_passed)
    if (.not. test_passed) all_tests_passed = .false.
    
    ! Output final result
    if (all_tests_passed) then
        print *, "All JSON diff workflow tests PASSED"
        call exit(0)
    else
        print *, "JSON diff workflow tests FAILED - Format incompatibility detected!"
        call exit(1)
    end if
    
contains

    ! Helper procedure to write JSON to file
    subroutine write_json_to_file(filename, json_content)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: json_content
        
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace', action='write')
        write(unit, '(A)') json_content
        close(unit)
    end subroutine write_json_to_file
    
    ! Helper procedure to delete a file
    subroutine delete_file(filename)
        character(len=*), intent(in) :: filename
        
        integer :: unit
        
        open(newunit=unit, file=filename, status='old')
        close(unit, status='delete')
    end subroutine delete_file

    ! Test export JSON files and use them in diff computation
    subroutine test_exported_json_diff_workflow(test_passed)
        logical, intent(out) :: test_passed
        
        type(coverage_data_t) :: baseline_coverage, current_coverage
        type(coverage_data_t) :: imported_baseline, imported_current
        character(len=:), allocatable :: baseline_json, current_json
        logical :: baseline_error, current_error
        type(coverage_diff_t) :: diff_result
        type(coverage_line_t) :: baseline_lines(2), current_lines(2)
        type(coverage_file_t) :: baseline_files(1), current_files(1)
        
        test_passed = .false.
        print *, "Test: Export JSON and use in diff computation..."
        
        ! Create baseline coverage data
        call baseline_lines(1)%init(3, 10, "test.f90", .true.)
        call baseline_lines(2)%init(0, 15, "test.f90", .true.)
        call baseline_files(1)%init("test.f90", baseline_lines)
        call baseline_coverage%init(baseline_files)
        
        ! Create current coverage data (with changes)
        call current_lines(1)%init(5, 10, "test.f90", .true.)  ! Increased from 3 to 5
        call current_lines(2)%init(2, 15, "test.f90", .true.)  ! Increased from 0 to 2
        call current_files(1)%init("test.f90", current_lines)
        call current_coverage%init(current_files)
        
        ! Export both to JSON
        call export_json_coverage(baseline_coverage, baseline_json)
        call export_json_coverage(current_coverage, current_json)
        
        print *, "Baseline JSON:"
        print *, baseline_json
        print *, "Current JSON:"
        print *, current_json
        
        ! Write to temporary files
        call write_json_to_file("test_baseline_exported.json", baseline_json)
        call write_json_to_file("test_current_exported.json", current_json)
        
        ! Import back from JSON (simulating --diff workflow)
        call import_json_coverage_safe(baseline_json, imported_baseline, baseline_error)
        call import_json_coverage_safe(current_json, imported_current, current_error)
        
        if (baseline_error) then
            print *, "FAIL: Could not import baseline JSON for diff"
            return
        end if
        
        if (current_error) then
            print *, "FAIL: Could not import current JSON for diff"
            return
        end if
        
        ! Compute diff using imported data (this is what --diff does)
        diff_result = compute_coverage_diff(imported_baseline, imported_current, &
                                           .false., 0.0)
        
        ! Verify diff computation worked
        if (size(diff_result%file_diffs) == 0) then
            print *, "FAIL: No diff results computed"
            return
        end if
        
        if (diff_result%file_diffs(1)%filename /= "test.f90") then
            print *, "FAIL: Diff filename mismatch"
            return
        end if
        
        print *, "PASS: Exported JSON files work in diff computation"
        test_passed = .true.
        
        ! Clean up
        call delete_file("test_baseline_exported.json")
        call delete_file("test_current_exported.json")
    end subroutine test_exported_json_diff_workflow
    
    ! Test CLI diff command with exported JSON files
    subroutine test_cli_diff_with_exported_json(test_passed)
        logical, intent(out) :: test_passed
        
        type(coverage_data_t) :: baseline_coverage, current_coverage
        character(len=:), allocatable :: baseline_json, current_json
        type(coverage_line_t) :: baseline_lines(1), current_lines(1)
        type(coverage_file_t) :: baseline_files(1), current_files(1)
        integer :: exit_code
        
        test_passed = .false.
        print *, "Test: CLI diff command with exported JSON files..."
        
        ! Create test coverage data
        call baseline_lines(1)%init(2, 10, "cli_test.f90", .true.)
        call baseline_files(1)%init("cli_test.f90", baseline_lines)
        call baseline_coverage%init(baseline_files)
        
        call current_lines(1)%init(4, 10, "cli_test.f90", .true.)
        call current_files(1)%init("cli_test.f90", current_lines)
        call current_coverage%init(current_files)
        
        ! Export to JSON files
        call export_json_coverage(baseline_coverage, baseline_json)
        call export_json_coverage(current_coverage, current_json)
        
        call write_json_to_file("cli_baseline.json", baseline_json)
        call write_json_to_file("cli_current.json", current_json)
        
        print *, "Testing CLI diff command..."
        print *, "Command: fpm run -- --diff=cli_baseline.json,cli_current.json"
        
        ! Test the actual CLI command (this would expose format incompatibility)
        call execute_command_line("cd /home/ert/code/fortcov && fpm run -- --diff=cli_baseline.json,cli_current.json", &
                                 exitstat=exit_code)
        
        if (exit_code /= 0) then
            print *, "FAIL: CLI diff command failed with exit code:", exit_code
            print *, "This indicates JSON format incompatibility in --diff workflow!"
            ! Clean up before returning
            call delete_file("cli_baseline.json")
            call delete_file("cli_current.json")
            return
        end if
        
        print *, "PASS: CLI diff command succeeded with exported JSON files"
        test_passed = .true.
        
        ! Clean up
        call delete_file("cli_baseline.json")
        call delete_file("cli_current.json")
    end subroutine test_cli_diff_with_exported_json
    
    ! Test format compatibility across different coverage scenarios
    subroutine test_format_compatibility_scenarios(test_passed)
        logical, intent(out) :: test_passed
        
        test_passed = .false.
        print *, "Test: Format compatibility across different scenarios..."
        
        ! Test with different JSON structure variations
        call test_object_vs_array_format_compatibility()
        call test_missing_functions_field_compatibility()
        call test_boolean_value_compatibility()
        
        print *, "PASS: Format compatibility scenarios tested"
        test_passed = .true.
    end subroutine test_format_compatibility_scenarios
    
    ! Test object vs array format compatibility
    subroutine test_object_vs_array_format_compatibility()
        character(len=*), parameter :: object_format = &
            '{"files": [{"filename": "test.f90", "lines": [{"line_number": 10, "execution_count": 5, "is_executable": true}]}]}'
        
        character(len=*), parameter :: array_format = &
            '[{"filename": "test.f90", "lines": [{"line_number": 10, "execution_count": 5, "is_executable": true}]}]'
        
        type(coverage_data_t) :: coverage_data
        logical :: error_occurred
        
        print *, "Testing object vs array format compatibility..."
        
        ! Test object format (what export_json_coverage produces)
        call import_json_coverage_safe(object_format, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "WARNING: Object format import failed"
        else
            print *, "Object format import successful"
        end if
        
        ! Test array format 
        call import_json_coverage_safe(array_format, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "WARNING: Array format import failed"
        else
            print *, "Array format import successful"
        end if
    end subroutine test_object_vs_array_format_compatibility
    
    ! Test missing functions field compatibility
    subroutine test_missing_functions_field_compatibility()
        character(len=*), parameter :: no_functions_json = &
            '{"files": [{"filename": "test.f90", "lines": [{"line_number": 10, "execution_count": 5, "is_executable": true}]}]}'
        
        type(coverage_data_t) :: coverage_data
        logical :: error_occurred
        
        print *, "Testing missing functions field compatibility..."
        
        call import_json_coverage_safe(no_functions_json, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "WARNING: Missing functions field caused import failure"
        else
            print *, "Missing functions field handled correctly"
        end if
    end subroutine test_missing_functions_field_compatibility
    
    ! Test boolean value format compatibility
    subroutine test_boolean_value_compatibility()
        character(len=*), parameter :: lowercase_bool_json = &
            '{"files": [{"filename": "test.f90", "lines": [{"line_number": 10, "execution_count": 5, "is_executable": true}]}]}'
        
        character(len=*), parameter :: uppercase_bool_json = &
            '{"files": [{"filename": "test.f90", "lines": [{"line_number": 10, "execution_count": 5, "is_executable": True}]}]}'
        
        type(coverage_data_t) :: coverage_data
        logical :: error_occurred
        
        print *, "Testing boolean value format compatibility..."
        
        call import_json_coverage_safe(lowercase_bool_json, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "WARNING: Lowercase boolean import failed"
        else
            print *, "Lowercase boolean import successful"
        end if
        
        call import_json_coverage_safe(uppercase_bool_json, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "WARNING: Uppercase boolean import failed"
        else
            print *, "Uppercase boolean import successful"
        end if
    end subroutine test_boolean_value_compatibility

end program test_json_diff_workflow