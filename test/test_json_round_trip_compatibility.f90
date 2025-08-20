program test_json_round_trip_compatibility
    use coverage_model
    use json_coverage_io
    use file_utils
    implicit none
    
    logical :: test_passed, all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "=== JSON Round-Trip Compatibility Tests ==="
    
    ! RED Test 1: Export JSON and attempt round-trip import
    call test_json_export_import_round_trip(test_passed)
    if (.not. test_passed) all_tests_passed = .false.
    
    ! RED Test 2: Test format compatibility with existing JSON files  
    call test_existing_json_format_compatibility(test_passed)
    if (.not. test_passed) all_tests_passed = .false.
    
    ! RED Test 3: Test with complex coverage data (functions + lines)
    call test_complex_coverage_round_trip(test_passed)
    if (.not. test_passed) all_tests_passed = .false.
    
    ! RED Test 4: Test empty coverage data
    call test_empty_coverage_round_trip(test_passed)
    if (.not. test_passed) all_tests_passed = .false.
    
    ! Output final result
    if (all_tests_passed) then
        print *, "All JSON round-trip tests PASSED"
        call exit(0)
    else
        print *, "JSON round-trip tests FAILED - Format incompatibility detected!"
        call exit(1)
    end if
    
contains

    ! Test basic JSON export and import round-trip
    subroutine test_json_export_import_round_trip(test_passed)
        logical, intent(out) :: test_passed
        
        type(coverage_data_t) :: original_coverage, imported_coverage
        character(len=:), allocatable :: json_output
        logical :: import_error
        type(coverage_line_t) :: lines(3)
        type(coverage_file_t) :: files(1)
        integer :: i
        
        test_passed = .false.
        print *, "Test: Basic JSON export/import round-trip..."
        
        ! Create test coverage data
        call lines(1)%init("test.f90", 10, 5, .true.)
        call lines(2)%init("test.f90", 15, 0, .true.)  
        call lines(3)%init("test.f90", 20, 3, .true.)
        
        call files(1)%init("test.f90", lines)
        call original_coverage%init(files)
        
        ! Export to JSON
        call export_json_coverage(original_coverage, json_output)
        
        print *, "Exported JSON:"
        print *, json_output
        
        ! Attempt to import back
        call import_json_coverage_safe(json_output, imported_coverage, import_error)
        
        if (import_error) then
            print *, "FAIL: Import failed after export"
            print *, "This indicates JSON format incompatibility!"
            return
        end if
        
        ! Verify data integrity
        if (size(imported_coverage%files) /= size(original_coverage%files)) then
            print *, "FAIL: File count mismatch"
            print *, "Original:", size(original_coverage%files)
            print *, "Imported:", size(imported_coverage%files)
            return
        end if
        
        if (imported_coverage%files(1)%filename /= original_coverage%files(1)%filename) then
            print *, "FAIL: Filename mismatch"
            print *, "Original:", original_coverage%files(1)%filename
            print *, "Imported:", imported_coverage%files(1)%filename
            return
        end if
        
        if (size(imported_coverage%files(1)%lines) /= size(original_coverage%files(1)%lines)) then
            print *, "FAIL: Line count mismatch"
            print *, "Original:", size(original_coverage%files(1)%lines)
            print *, "Imported:", size(imported_coverage%files(1)%lines)
            return
        end if
        
        ! Check line data integrity
        do i = 1, size(original_coverage%files(1)%lines)
            if (imported_coverage%files(1)%lines(i)%line_number /= &
                original_coverage%files(1)%lines(i)%line_number) then
                print *, "FAIL: Line number mismatch at line", i
                return
            end if
            
            if (imported_coverage%files(1)%lines(i)%execution_count /= &
                original_coverage%files(1)%lines(i)%execution_count) then
                print *, "FAIL: Execution count mismatch at line", i
                return
            end if
            
            if (imported_coverage%files(1)%lines(i)%is_executable .neqv. &
                original_coverage%files(1)%lines(i)%is_executable) then
                print *, "FAIL: Executable flag mismatch at line", i
                return
            end if
        end do
        
        print *, "PASS: Basic round-trip successful"
        test_passed = .true.
    end subroutine test_json_export_import_round_trip
    
    ! Test compatibility with existing JSON files (from repo)
    subroutine test_existing_json_format_compatibility(test_passed)
        logical, intent(out) :: test_passed
        
        character(len=:), allocatable :: existing_json_files(:)
        character(len=:), allocatable :: json_content
        type(coverage_data_t) :: imported_coverage
        logical :: import_error, file_error
        integer :: i
        
        test_passed = .false.
        print *, "Test: Compatibility with existing JSON files..."
        
        ! Find existing JSON files in repository
        existing_json_files = [character(len=256) :: &
            "baseline_1.json", "current_1.json", "test_baseline.json"]
        
        do i = 1, size(existing_json_files)
            if (file_exists(existing_json_files(i))) then
                print *, "Testing file:", trim(existing_json_files(i))
                
                call read_file_content(existing_json_files(i), json_content, file_error)
                if (file_error) then
                    print *, "FAIL: Could not read file:", trim(existing_json_files(i))
                    return
                end if
                
                call import_json_coverage_safe(json_content, imported_coverage, import_error)
                if (import_error) then
                    print *, "FAIL: Could not import existing JSON file:", trim(existing_json_files(i))
                    print *, "This indicates format incompatibility with existing files!"
                    return
                end if
                
                print *, "Successfully imported:", trim(existing_json_files(i))
            else
                print *, "File not found (skipping):", trim(existing_json_files(i))
            end if
        end do
        
        print *, "PASS: Existing JSON files are compatible"
        test_passed = .true.
    end subroutine test_existing_json_format_compatibility
    
    ! Test with complex coverage data including functions
    subroutine test_complex_coverage_round_trip(test_passed)
        logical, intent(out) :: test_passed
        
        type(coverage_data_t) :: original_coverage, imported_coverage
        character(len=:), allocatable :: json_output
        logical :: import_error
        type(coverage_line_t) :: lines(2)
        type(coverage_function_t) :: functions(1)
        type(coverage_file_t) :: files(1)
        
        test_passed = .false.
        print *, "Test: Complex coverage data with functions..."
        
        ! Create test coverage with functions
        call lines(1)%init("module.f90", 10, 3, .true.)
        call lines(2)%init("module.f90", 20, 1, .true.)
        
        call functions(1)%init("test_function", "module.f90", 10, 2, "test_module", .true.)
        
        call files(1)%init("module.f90", lines)
        allocate(files(1)%functions, source=functions)
        
        call original_coverage%init(files)
        
        ! Export to JSON
        call export_json_coverage(original_coverage, json_output)
        
        print *, "Exported complex JSON:"
        print *, json_output
        
        ! Attempt to import back
        call import_json_coverage_safe(json_output, imported_coverage, import_error)
        
        if (import_error) then
            print *, "FAIL: Import failed for complex coverage data"
            return
        end if
        
        ! Verify function data
        if (.not. allocated(imported_coverage%files(1)%functions)) then
            print *, "FAIL: Functions not preserved during round-trip"
            return
        end if
        
        if (size(imported_coverage%files(1)%functions) /= size(original_coverage%files(1)%functions)) then
            print *, "FAIL: Function count mismatch"
            return
        end if
        
        if (imported_coverage%files(1)%functions(1)%name /= &
            original_coverage%files(1)%functions(1)%name) then
            print *, "FAIL: Function name mismatch"
            return
        end if
        
        print *, "PASS: Complex coverage round-trip successful"
        test_passed = .true.
    end subroutine test_complex_coverage_round_trip
    
    ! Test with empty coverage data
    subroutine test_empty_coverage_round_trip(test_passed)
        logical, intent(out) :: test_passed
        
        type(coverage_data_t) :: original_coverage, imported_coverage
        character(len=:), allocatable :: json_output
        logical :: import_error
        type(coverage_file_t) :: files(0)
        
        test_passed = .false.
        print *, "Test: Empty coverage data round-trip..."
        
        ! Create empty coverage data
        call original_coverage%init(files)
        
        ! Export to JSON
        call export_json_coverage(original_coverage, json_output)
        
        print *, "Exported empty JSON:"
        print *, json_output
        
        ! Attempt to import back
        call import_json_coverage_safe(json_output, imported_coverage, import_error)
        
        if (import_error) then
            print *, "FAIL: Import failed for empty coverage data"
            return
        end if
        
        if (size(imported_coverage%files) /= 0) then
            print *, "FAIL: Empty data not preserved"
            return
        end if
        
        print *, "PASS: Empty coverage round-trip successful"
        test_passed = .true.
    end subroutine test_empty_coverage_round_trip

end program test_json_round_trip_compatibility