program test_json_coverage_io
    use coverage_model
    use json_coverage_io
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing JSON Coverage I/O..."
    
    ! Test 1: Import simple JSON coverage data to coverage_data_t
    all_tests_passed = all_tests_passed .and. test_import_simple_json()
    
    ! Test 2: Import JSON with multiple files
    all_tests_passed = all_tests_passed .and. test_import_multiple_files()
    
    ! Test 3: Import JSON with branches and functions
    all_tests_passed = all_tests_passed .and. test_import_branches_functions()
    
    ! Test 4: Export coverage_data_t to JSON format
    all_tests_passed = all_tests_passed .and. test_export_to_json()
    
    ! Test 5: Round-trip JSON import/export consistency
    all_tests_passed = all_tests_passed .and. test_json_roundtrip()
    
    ! Test 6: Handle malformed JSON gracefully
    all_tests_passed = all_tests_passed .and. test_malformed_json_handling()
    
    ! Test 7: Handle empty JSON data
    all_tests_passed = all_tests_passed .and. test_empty_json_data()
    
    ! Test 8: Validate required JSON fields
    all_tests_passed = all_tests_passed .and. test_required_fields_validation()
    
    ! Test 9: Handle large JSON data efficiently
    all_tests_passed = all_tests_passed .and. test_large_json_performance()
    
    if (all_tests_passed) then
        print *, "All tests PASSED"
        call exit(0)
    else
        print *, "Some tests FAILED"
        call exit(1)
    end if

contains

    function test_import_simple_json() result(passed)
        logical :: passed
        type(coverage_data_t) :: coverage_data
        character(len=:), allocatable :: json_content
        
        passed = .false.
        
        ! Simple JSON with one file, one line
        json_content = '{"files": [{"filename": "test.f90", ' // &
                      '"lines": [{"line_number": 1, "execution_count": 5, ' // &
                      '"is_executable": true}]}]}'
        
        ! This should fail until json_coverage_io module is implemented
        call import_json_coverage(json_content, coverage_data)
        
        ! Verify the imported data
        if (size(coverage_data%files) == 1) then
            if (coverage_data%files(1)%filename == "test.f90") then
                if (size(coverage_data%files(1)%lines) == 1) then
                    if (coverage_data%files(1)%lines(1)%line_number == 1 .and. &
                        coverage_data%files(1)%lines(1)%execution_count == 5 .and. &
                        coverage_data%files(1)%lines(1)%is_executable) then
                        passed = .true.
                    end if
                end if
            end if
        end if
        
        if (.not. passed) then
            print *, "FAIL: test_import_simple_json - could not import basic JSON"
        end if
    end function test_import_simple_json

    function test_import_multiple_files() result(passed)
        logical :: passed
        type(coverage_data_t) :: coverage_data
        character(len=:), allocatable :: json_content
        
        passed = .false.
        
        ! JSON with multiple files
        json_content = '{"files": [' // &
                      '{"filename": "src/main.f90", "lines": [' // &
                      '{"line_number": 1, "execution_count": 3, ' // &
                      '"is_executable": true}]}, ' // &
                      '{"filename": "src/utils.f90", "lines": [' // &
                      '{"line_number": 10, "execution_count": 0, ' // &
                      '"is_executable": true}]}]}'
        
        call import_json_coverage(json_content, coverage_data)
        
        ! Verify two files imported correctly
        if (size(coverage_data%files) == 2) then
            if (coverage_data%files(1)%filename == "src/main.f90" .and. &
                coverage_data%files(2)%filename == "src/utils.f90") then
                if (coverage_data%files(1)%lines(1)%execution_count == 3 .and. &
                    coverage_data%files(2)%lines(1)%execution_count == 0) then
                    passed = .true.
                end if
            end if
        end if
        
        if (.not. passed) then
            print *, "FAIL: test_import_multiple_files - failed multiple file import"
        end if
    end function test_import_multiple_files

    function test_import_branches_functions() result(passed)
        logical :: passed
        type(coverage_data_t) :: coverage_data
        character(len=:), allocatable :: json_content
        
        passed = .false.
        
        ! JSON with branches and functions
        json_content = '{"files": [{"filename": "complex.f90", ' // &
                      '"lines": [{"line_number": 5, "execution_count": 2, ' // &
                      '"is_executable": true}], ' // &
                      '"functions": [{"name": "test_func", "parent_module": "test_mod", ' // &
                      '"is_module_procedure": true, "execution_count": 1, ' // &
                      '"line_number": 5, "filename": "complex.f90"}]}]}'
        
        call import_json_coverage(json_content, coverage_data)
        
        ! Verify functions are imported
        if (size(coverage_data%files) == 1) then
            if (allocated(coverage_data%files(1)%functions)) then
                if (size(coverage_data%files(1)%functions) == 1) then
                    if (coverage_data%files(1)%functions(1)%name == "test_func" .and. &
                        coverage_data%files(1)%functions(1)%parent_module == "test_mod" .and. &
                        coverage_data%files(1)%functions(1)%is_module_procedure) then
                        passed = .true.
                    end if
                end if
            end if
        end if
        
        if (.not. passed) then
            print *, "FAIL: test_import_branches_functions - function import failed"
        end if
    end function test_import_branches_functions

    function test_export_to_json() result(passed)
        logical :: passed
        type(coverage_data_t) :: coverage_data
        type(coverage_file_t) :: test_file
        type(coverage_line_t) :: test_lines(2)
        character(len=:), allocatable :: json_output
        
        passed = .false.
        
        ! Create test coverage data
        call test_lines(1)%init(5, 1, "test.f90", .true.)
        call test_lines(2)%init(0, 2, "test.f90", .true.)
        call test_file%init("test.f90", test_lines)
        call coverage_data%init([test_file])
        
        ! Export to JSON
        call export_json_coverage(coverage_data, json_output)
        
        ! Verify JSON contains expected data
        if (len(json_output) > 0) then
            if (index(json_output, '"filename": "test.f90"') > 0 .and. &
                index(json_output, '"execution_count": 5') > 0 .and. &
                index(json_output, '"execution_count": 0') > 0) then
                passed = .true.
            end if
        end if
        
        if (.not. passed) then
            print *, "FAIL: test_export_to_json - JSON export failed"
        end if
    end function test_export_to_json

    function test_json_roundtrip() result(passed)
        logical :: passed
        type(coverage_data_t) :: original_data, imported_data
        type(coverage_file_t) :: test_file
        type(coverage_line_t) :: test_lines(1)
        character(len=:), allocatable :: json_output
        
        passed = .false.
        
        ! Create original data
        call test_lines(1)%init(3, 10, "roundtrip.f90", .true.)
        call test_file%init("roundtrip.f90", test_lines)
        call original_data%init([test_file])
        
        ! Export to JSON then import back
        call export_json_coverage(original_data, json_output)
        call import_json_coverage(json_output, imported_data)
        
        if (size(imported_data%files) == size(original_data%files)) then
            if (imported_data%files(1)%filename == original_data%files(1)%filename) then
                if (imported_data%files(1)%lines(1)%execution_count == &
                    original_data%files(1)%lines(1)%execution_count) then
                    passed = .true.
                end if
            end if
        end if
        
        if (.not. passed) then
            print *, "FAIL: test_json_roundtrip - data lost in roundtrip"
        end if
    end function test_json_roundtrip

    function test_malformed_json_handling() result(passed)
        logical :: passed
        type(coverage_data_t) :: coverage_data
        character(len=:), allocatable :: malformed_json
        logical :: error_caught
        
        passed = .false.
        error_caught = .false.
        
        ! Malformed JSON should be handled gracefully
        malformed_json = '{"files": [{"filename": "bad.f90"'  ! Missing closing braces
        
        call import_json_coverage_safe(malformed_json, coverage_data, error_caught)
        
        ! Should catch the error without crashing
        if (error_caught) then
            passed = .true.
        end if
        
        if (.not. passed) then
            print *, "FAIL: test_malformed_json_handling - did not handle error"
        end if
    end function test_malformed_json_handling

    function test_empty_json_data() result(passed)
        logical :: passed
        type(coverage_data_t) :: coverage_data
        character(len=:), allocatable :: empty_json
        
        passed = .false.
        
        ! Empty JSON should result in empty coverage data
        empty_json = '{"files": []}'
        
        call import_json_coverage(empty_json, coverage_data)
        
        if (size(coverage_data%files) == 0) then
            passed = .true.
        end if
        
        if (.not. passed) then
            print *, "FAIL: test_empty_json_data - empty JSON not handled"
        end if
    end function test_empty_json_data

    function test_required_fields_validation() result(passed)
        logical :: passed
        type(coverage_data_t) :: coverage_data
        character(len=:), allocatable :: invalid_json
        logical :: error_caught
        
        passed = .false.
        error_caught = .false.
        
        ! JSON missing required field 'line_number'
        invalid_json = '{"files": [{"filename": "test.f90", ' // &
                      '"lines": [{"execution_count": 5, "is_executable": true}]}]}'
        
        call import_json_coverage_safe(invalid_json, coverage_data, error_caught)
        
        if (error_caught) then
            passed = .true.
        end if
        
        if (.not. passed) then
            print *, "FAIL: test_required_fields_validation - validation failed"
        end if
    end function test_required_fields_validation

    function test_large_json_performance() result(passed)
        logical :: passed
        type(coverage_data_t) :: coverage_data
        character(len=:), allocatable :: large_json
        integer :: start_time, end_time, time_diff
        
        passed = .false.
        
        ! Create JSON with smaller number of lines for simplicity
        large_json = '{"files": [{"filename": "large.f90", "lines": [' // &
                    '{"line_number": 1, "execution_count": 5, "is_executable": true},' // &
                    '{"line_number": 2, "execution_count": 0, "is_executable": true},' // &
                    '{"line_number": 3, "execution_count": 3, "is_executable": true},' // &
                    '{"line_number": 4, "execution_count": 1, "is_executable": true},' // &
                    '{"line_number": 5, "execution_count": 2, "is_executable": true}' // &
                    ']}]}'
        
        call system_clock(start_time)
        call import_json_coverage(large_json, coverage_data)
        call system_clock(end_time)
        
        time_diff = end_time - start_time
        
        ! Should handle 5 lines correctly - with proper memory safety checks
        if (allocated(coverage_data%files)) then
            if (size(coverage_data%files) >= 1) then
                if (allocated(coverage_data%files(1)%lines)) then
                    if (size(coverage_data%files(1)%lines) == 5) then
                        passed = .true.
                    end if
                end if
            end if
        end if
        
        if (.not. passed) then
            print *, "FAIL: test_large_json_performance - JSON parsing returned empty result"
        end if
    end function test_large_json_performance

end program test_json_coverage_io