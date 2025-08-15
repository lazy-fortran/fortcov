program test_import_integration
    ! Integration tests for actual JSON file import functionality
    ! RED PHASE: These tests will fail until import is fixed
    use coverage_model
    use json_coverage_io, only: import_json_coverage_safe
    use file_utils, only: read_file_content, file_exists
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing Import Integration..."
    
    ! RED PHASE - These tests should fail initially
    all_tests_passed = all_tests_passed .and. test_import_small_test_json()
    all_tests_passed = all_tests_passed .and. test_import_object_wrapped_json()
    all_tests_passed = all_tests_passed .and. test_import_error_diagnostics()
    
    if (all_tests_passed) then
        print *, "All integration tests PASSED"
        call exit(0)
    else
        print *, "Some integration tests FAILED"
        call exit(1)
    end if

contains

    function test_import_small_test_json() result(passed)
        ! RED TEST: Import existing small_test.json file (array format)
        logical :: passed
        type(coverage_data_t) :: coverage_data
        character(len=:), allocatable :: json_content
        logical :: error_occurred, file_error
        
        print *, "  Test 1: Import small_test.json (array format)"
        
        passed = .false.
        
        ! Check if test file exists
        if (.not. file_exists("small_test.json")) then
            print *, "    FAILED: small_test.json not found"
            return
        end if
        
        ! Read the JSON file
        call read_file_content("small_test.json", json_content, file_error)
        if (file_error) then
            print *, "    FAILED: Could not read small_test.json"
            return
        end if
        
        ! Try to import it (RED PHASE - this should fail)
        call import_json_coverage_safe(json_content, coverage_data, &
                                       error_occurred)
        
        if (.not. error_occurred) then
            ! Verify we got some data
            if (size(coverage_data%files) > 0) then
                if (len_trim(coverage_data%files(1)%filename) > 0) then
                    passed = .true.
                    print *, "    SUCCESS: Imported", size(coverage_data%files), &
                             "files"
                else
                    print *, "    FAILED: Empty filename after import"
                end if
            else
                print *, "    FAILED: No files imported"
            end if
        else
            print *, "    FAILED: JSON import failed with error"
        end if
    end function test_import_small_test_json

    function test_import_object_wrapped_json() result(passed)
        ! RED TEST: Import object-wrapped JSON format
        logical :: passed
        type(coverage_data_t) :: coverage_data
        character(len=:), allocatable :: json_content
        logical :: error_occurred
        
        print *, "  Test 2: Import object-wrapped JSON format"
        
        passed = .false.
        
        ! Create object-wrapped JSON content
        json_content = '{"files": [' // &
                      '{"filename": "test.f90", ' // &
                      '"lines": [' // &
                      '{"line_number": 1, "execution_count": 3, ' // &
                      '"is_executable": true}]}]}'
        
        ! Try to import it (RED PHASE - this should work with proper parser)
        call import_json_coverage_safe(json_content, coverage_data, &
                                       error_occurred)
        
        if (.not. error_occurred) then
            if (size(coverage_data%files) == 1) then
                if (coverage_data%files(1)%filename == "test.f90") then
                    if (size(coverage_data%files(1)%lines) == 1) then
                        if (coverage_data%files(1)%lines(1)%execution_count &
                            == 3) then
                            passed = .true.
                            print *, "    SUCCESS: Object-wrapped format works"
                        else
                            print *, "    FAILED: Wrong execution count"
                        end if
                    else
                        print *, "    FAILED: Wrong line count"
                    end if
                else
                    print *, "    FAILED: Wrong filename: ", &
                             coverage_data%files(1)%filename
                end if
            else
                print *, "    FAILED: Wrong file count:", &
                         size(coverage_data%files)
            end if
        else
            print *, "    FAILED: Object-wrapped JSON import failed"
        end if
    end function test_import_object_wrapped_json

    function test_import_error_diagnostics() result(passed)
        ! RED TEST: Error diagnostics for malformed JSON
        logical :: passed
        type(coverage_data_t) :: coverage_data
        character(len=:), allocatable :: malformed_json
        logical :: error_occurred
        
        print *, "  Test 3: Error diagnostics for malformed JSON"
        
        passed = .false.
        
        ! Create malformed JSON (missing closing brace)
        malformed_json = '{"files": [{"filename": "test.f90"'
        
        ! This should fail with proper error reporting
        call import_json_coverage_safe(malformed_json, coverage_data, &
                                       error_occurred)
        
        if (error_occurred) then
            passed = .true.
            print *, "    SUCCESS: Error correctly detected for malformed JSON"
        else
            print *, "    FAILED: Malformed JSON should have failed import"
        end if
    end function test_import_error_diagnostics

end program test_import_integration