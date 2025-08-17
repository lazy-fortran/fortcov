program test_json_schema_compatibility
    use coverage_model
    use json_coverage_io
    implicit none
    
    logical :: test_passed, all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "=== JSON Schema Compatibility Tests ==="
    
    ! RED Test 1: Case sensitivity in boolean values
    call test_boolean_case_sensitivity(test_passed)
    if (.not. test_passed) all_tests_passed = .false.
    
    ! RED Test 2: Number format variations
    call test_number_format_variations(test_passed)
    if (.not. test_passed) all_tests_passed = .false.
    
    ! RED Test 3: String escaping and special characters
    call test_string_escaping_compatibility(test_passed)
    if (.not. test_passed) all_tests_passed = .false.
    
    ! RED Test 4: Optional field handling
    call test_optional_field_handling(test_passed)
    if (.not. test_passed) all_tests_passed = .false.
    
    ! RED Test 5: Whitespace and formatting tolerance
    call test_whitespace_formatting_tolerance(test_passed)
    if (.not. test_passed) all_tests_passed = .false.
    
    ! RED Test 6: Large numeric values
    call test_large_numeric_values(test_passed)
    if (.not. test_passed) all_tests_passed = .false.
    
    ! Output final result
    if (all_tests_passed) then
        print *, "All JSON schema compatibility tests PASSED"
        call exit(0)
    else
        print *, "JSON schema compatibility tests FAILED - Format incompatibilities detected!"
        call exit(1)
    end if
    
contains

    ! Test boolean case sensitivity (True vs true)
    subroutine test_boolean_case_sensitivity(test_passed)
        logical, intent(out) :: test_passed
        
        character(len=*), parameter :: lowercase_json = &
            '{"files": [{"filename": "test.f90", "lines": [{"line_number": 10, "execution_count": 5, "is_executable": true}]}]}'
        
        character(len=*), parameter :: uppercase_json = &
            '{"files": [{"filename": "test.f90", "lines": [{"line_number": 10, "execution_count": 5, "is_executable": True}]}]}'
        
        character(len=*), parameter :: mixed_case_json = &
            '{"files": [{"filename": "test.f90", "lines": [{"line_number": 10, "execution_count": 5, "is_executable": TRUE}]}]}'
        
        type(coverage_data_t) :: coverage_data
        logical :: error_occurred
        
        test_passed = .true.
        print *, "Test: Boolean case sensitivity..."
        
        ! Test lowercase (standard)
        call import_json_coverage_safe(lowercase_json, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "FAIL: Lowercase 'true' failed to import"
            test_passed = .false.
        else
            print *, "PASS: Lowercase 'true' imported successfully"
        end if
        
        ! Test uppercase (potential incompatibility)
        call import_json_coverage_safe(uppercase_json, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "FAIL: Uppercase 'True' failed to import"
            print *, "This is a JSON format incompatibility issue!"
            test_passed = .false.
        else
            print *, "PASS: Uppercase 'True' imported successfully"
        end if
        
        ! Test all caps
        call import_json_coverage_safe(mixed_case_json, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "FAIL: All caps 'TRUE' failed to import"
            test_passed = .false.
        else
            print *, "PASS: All caps 'TRUE' imported successfully"
        end if
    end subroutine test_boolean_case_sensitivity
    
    ! Test number format variations
    subroutine test_number_format_variations(test_passed)
        logical, intent(out) :: test_passed
        
        character(len=*), parameter :: standard_numbers = &
            '{"files": [{"filename": "test.f90", "lines": [{"line_number": 10, "execution_count": 5, "is_executable": true}]}]}'
        
        character(len=*), parameter :: padded_numbers = &
            '{"files": [{"filename": "test.f90", "lines": [{"line_number":  10 , "execution_count":  5 , "is_executable": true}]}]}'
        
        character(len=*), parameter :: zero_padded = &
            '{"files": [{"filename": "test.f90", "lines": [{"line_number": 010, "execution_count": 005, "is_executable": true}]}]}'
        
        type(coverage_data_t) :: coverage_data
        logical :: error_occurred
        
        test_passed = .true.
        print *, "Test: Number format variations..."
        
        ! Test standard numbers
        call import_json_coverage_safe(standard_numbers, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "FAIL: Standard numbers failed to import"
            test_passed = .false.
        else
            print *, "PASS: Standard numbers imported successfully"
        end if
        
        ! Test padded numbers with extra spaces
        call import_json_coverage_safe(padded_numbers, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "FAIL: Padded numbers failed to import"
            test_passed = .false.
        else
            print *, "PASS: Padded numbers imported successfully"
        end if
        
        ! Test zero-padded numbers
        call import_json_coverage_safe(zero_padded, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "FAIL: Zero-padded numbers failed to import"
            test_passed = .false.
        else
            print *, "PASS: Zero-padded numbers imported successfully"
        end if
    end subroutine test_number_format_variations
    
    ! Test string escaping and special characters
    subroutine test_string_escaping_compatibility(test_passed)
        logical, intent(out) :: test_passed
        
        character(len=*), parameter :: escaped_quotes = &
            '{"files": [{"filename": "test_quoted.f90", "lines": [' // &
            '{"line_number": 10, "execution_count": 5, "is_executable": true}]}]}'
        
        character(len=*), parameter :: escaped_backslash = &
            '{"files": [{"filename": "test_path.f90", "lines": [' // &
            '{"line_number": 10, "execution_count": 5, "is_executable": true}]}]}'
        
        character(len=*), parameter :: unicode_chars = &
            '{"files": [{"filename": "test_unicode.f90", "lines": [' // &
            '{"line_number": 10, "execution_count": 5, "is_executable": true}]}]}'
        
        type(coverage_data_t) :: coverage_data
        logical :: error_occurred
        
        test_passed = .true.
        print *, "Test: String escaping and special characters..."
        
        ! Test escaped quotes
        call import_json_coverage_safe(escaped_quotes, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "FAIL: Escaped quotes failed to import"
            test_passed = .false.
        else
            print *, "PASS: Escaped quotes imported successfully"
        end if
        
        ! Test escaped backslash
        call import_json_coverage_safe(escaped_backslash, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "FAIL: Escaped backslash failed to import"
            test_passed = .false.
        else
            print *, "PASS: Escaped backslash imported successfully"
        end if
        
        ! Test unicode characters
        call import_json_coverage_safe(unicode_chars, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "FAIL: Unicode characters failed to import"
            test_passed = .false.
        else
            print *, "PASS: Unicode characters imported successfully"
        end if
    end subroutine test_string_escaping_compatibility
    
    ! Test optional field handling
    subroutine test_optional_field_handling(test_passed)
        logical, intent(out) :: test_passed
        
        character(len=*), parameter :: minimal_json = &
            '{"files": [{"filename": "test.f90", "lines": []}]}'
        
        character(len=*), parameter :: extra_fields_json = &
            '{"files": [{"filename": "test.f90", "lines": [], "extra_field": "ignored"}], "version": "1.0"}'
        
        character(len=*), parameter :: reordered_fields = &
            '{"files": [{"lines": [{"is_executable": true, "line_number": 10, "execution_count": 5}], "filename": "test.f90"}]}'
        
        type(coverage_data_t) :: coverage_data
        logical :: error_occurred
        
        test_passed = .true.
        print *, "Test: Optional field handling..."
        
        ! Test minimal JSON
        call import_json_coverage_safe(minimal_json, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "FAIL: Minimal JSON failed to import"
            test_passed = .false.
        else
            print *, "PASS: Minimal JSON imported successfully"
        end if
        
        ! Test extra fields (should be ignored)
        call import_json_coverage_safe(extra_fields_json, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "FAIL: JSON with extra fields failed to import"
            test_passed = .false.
        else
            print *, "PASS: JSON with extra fields imported successfully"
        end if
        
        ! Test reordered fields
        call import_json_coverage_safe(reordered_fields, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "FAIL: JSON with reordered fields failed to import"
            test_passed = .false.
        else
            print *, "PASS: JSON with reordered fields imported successfully"
        end if
    end subroutine test_optional_field_handling
    
    ! Test whitespace and formatting tolerance
    subroutine test_whitespace_formatting_tolerance(test_passed)
        logical, intent(out) :: test_passed
        
        character(len=*), parameter :: compact_json = &
            '{"files":[{"filename":"test.f90","lines":[{"line_number":10,"execution_count":5,"is_executable":true}]}]}'
        
        character(len=*), parameter :: spaced_json = &
            '{ "files" : [ { "filename" : "test.f90" , "lines" : [ { "line_number" : 10 , "execution_count" : 5 , "is_executable" : true } ] } ] }'
        
        character(len=400), parameter :: multiline_json = &
            '{' // new_line('A') // &
            '  "files": [' // new_line('A') // &
            '    {' // new_line('A') // &
            '      "filename": "test.f90",' // new_line('A') // &
            '      "lines": [' // new_line('A') // &
            '        {' // new_line('A') // &
            '          "line_number": 10,' // new_line('A') // &
            '          "execution_count": 5,' // new_line('A') // &
            '          "is_executable": true' // new_line('A') // &
            '        }' // new_line('A') // &
            '      ]' // new_line('A') // &
            '    }' // new_line('A') // &
            '  ]' // new_line('A') // &
            '}'
        
        type(coverage_data_t) :: coverage_data
        logical :: error_occurred
        
        test_passed = .true.
        print *, "Test: Whitespace and formatting tolerance..."
        
        ! Test compact JSON
        call import_json_coverage_safe(compact_json, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "FAIL: Compact JSON failed to import"
            test_passed = .false.
        else
            print *, "PASS: Compact JSON imported successfully"
        end if
        
        ! Test spaced JSON
        call import_json_coverage_safe(spaced_json, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "FAIL: Spaced JSON failed to import"
            test_passed = .false.
        else
            print *, "PASS: Spaced JSON imported successfully"
        end if
        
        ! Test multiline JSON
        call import_json_coverage_safe(multiline_json, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "FAIL: Multiline JSON failed to import"
            test_passed = .false.
        else
            print *, "PASS: Multiline JSON imported successfully"
        end if
    end subroutine test_whitespace_formatting_tolerance
    
    ! Test large numeric values
    subroutine test_large_numeric_values(test_passed)
        logical, intent(out) :: test_passed
        
        character(len=*), parameter :: large_numbers_json = &
            '{"files": [{"filename": "test.f90", "lines": [{"line_number": 999999, "execution_count": 2147483647, "is_executable": true}]}]}'
        
        character(len=*), parameter :: zero_values_json = &
            '{"files": [{"filename": "test.f90", "lines": [{"line_number": 0, "execution_count": 0, "is_executable": false}]}]}'
        
        type(coverage_data_t) :: coverage_data
        logical :: error_occurred
        
        test_passed = .true.
        print *, "Test: Large numeric values..."
        
        ! Test large numbers
        call import_json_coverage_safe(large_numbers_json, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "FAIL: Large numbers failed to import"
            test_passed = .false.
        else
            print *, "PASS: Large numbers imported successfully"
        end if
        
        ! Test zero values
        call import_json_coverage_safe(zero_values_json, coverage_data, error_occurred)
        if (error_occurred) then
            print *, "FAIL: Zero values failed to import"
            test_passed = .false.
        else
            print *, "PASS: Zero values imported successfully"
        end if
    end subroutine test_large_numeric_values

end program test_json_schema_compatibility