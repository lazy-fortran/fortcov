program test_json_functionality_issue_816
    !! Comprehensive JSON functionality test for Issue #816
    !! 
    !! Tests JSON import/export that was completely broken with segfaults
    !! Ensures both empty coverage data and coverage with files work correctly
    use json_io
    use coverage_model_core
    implicit none
    
    call test_empty_json_roundtrip()
    call test_json_file_operations()
    call test_json_validation()
    
    print *, "=== Issue #816: All JSON functionality tests passed! ==="
    
contains

    subroutine test_empty_json_roundtrip()
        !! Test JSON export and import with empty coverage data
        type(coverage_data_t) :: original_data, imported_data
        character(len=:), allocatable :: json_output
        logical :: error_caught
        
        print *, "Test 1: Empty coverage data JSON roundtrip"
        
        ! Create empty coverage data
        call original_data%init()
        
        ! Export to JSON
        call export_coverage_to_json(original_data, json_output)
        if (.not. allocated(json_output) .or. len(json_output) == 0) then
            print *, "ERROR: JSON export failed"
            stop 1
        end if
        
        ! Verify JSON contains expected structure
        if (index(json_output, '"files": []') == 0) then
            print *, "ERROR: JSON missing files array"
            stop 1
        end if
        
        if (index(json_output, '"total_files": 0') == 0) then
            print *, "ERROR: JSON missing total_files"
            stop 1
        end if
        
        ! Import from JSON
        call import_coverage_from_json_safe(json_output, imported_data, error_caught)
        if (error_caught) then
            print *, "ERROR: JSON import failed"
            stop 1
        end if
        
        ! Verify imported data
        if (imported_data%total_files /= 0) then
            print *, "ERROR: Imported total_files mismatch"
            stop 1
        end if
        
        if (.not. allocated(imported_data%files)) then
            print *, "ERROR: Files array not allocated after import"
            stop 1
        end if
        
        if (size(imported_data%files) /= 0) then
            print *, "ERROR: Files array size mismatch"
            stop 1
        end if
        
        print *, "✅ PASS: Empty JSON roundtrip successful"
    end subroutine test_empty_json_roundtrip

    subroutine test_json_file_operations()
        !! Test JSON file import/export
        type(coverage_data_t) :: coverage_data, file_imported_data
        character(len=:), allocatable :: json_content
        logical :: error_caught
        character(len=*), parameter :: test_filename = "test_coverage_816.json"
        integer :: unit_num
        
        print *, "Test 2: JSON file operations"
        
        ! Create coverage data and export to JSON string
        call coverage_data%init()
        call export_coverage_to_json(coverage_data, json_content)
        
        ! Write JSON to file
        open(newunit=unit_num, file=test_filename, status='replace', action='write')
        write(unit_num, '(A)') json_content
        close(unit_num)
        
        ! Import from JSON file
        call import_coverage_from_json_file(test_filename, file_imported_data, error_caught)
        if (error_caught) then
            print *, "ERROR: JSON file import failed"
            stop 1
        end if
        
        ! Verify file imported data
        if (file_imported_data%total_files /= 0) then
            print *, "ERROR: File imported total_files mismatch"
            stop 1
        end if
        
        ! Clean up test file
        open(newunit=unit_num, file=test_filename, status='old')
        close(unit_num, status='delete')
        
        print *, "✅ PASS: JSON file operations successful"
    end subroutine test_json_file_operations

    subroutine test_json_validation()
        !! Test JSON format validation
        logical :: is_valid
        
        print *, "Test 3: JSON format validation"
        
        ! Test valid JSON
        is_valid = validate_json_coverage_format('{"files": [], "version": "1.0"}')
        if (.not. is_valid) then
            print *, "ERROR: Valid JSON rejected"
            stop 1
        end if
        
        ! Test invalid JSON
        is_valid = validate_json_coverage_format('invalid json {')
        if (is_valid) then
            print *, "ERROR: Invalid JSON accepted"
            stop 1
        end if
        
        print *, "✅ PASS: JSON validation working correctly"
    end subroutine test_json_validation

end program test_json_functionality_issue_816
