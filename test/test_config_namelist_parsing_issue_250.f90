program test_config_namelist_parsing_issue_250
    !! Test suite for issue #250: Configuration file parsing fails with iostat=5010
    !! Tests proper Fortran namelist parsing for configuration files
    
    use fortcov_config, only: config_t, parse_config_file
    use config_file_handler, only: parse_namelist_config_file
    implicit none
    
    integer :: test_count = 0
    integer :: failed_count = 0
    character(len=256) :: temp_file
    integer :: unit, iostat
    logical :: file_exists
    
    ! Start test suite
    print *, "Testing configuration namelist parsing (Issue #250)..."
    print *, "=================================================="
    
    ! Run tests
    call test_simple_namelist_config()
    call test_namelist_with_arrays()
    call test_namelist_with_comments()
    call test_mixed_format_detection()
    call test_iostat_5010_handling()
    call test_empty_namelist()
    call test_invalid_namelist_format()
    call test_namelist_with_logical_values()
    
    ! Summary
    print *, ""
    print *, "Test Summary:"
    print *, "============="
    print *, "Total tests: ", test_count
    print *, "Failed tests: ", failed_count
    
    if (failed_count == 0) then
        print *, "✓ All tests passed!"
        stop 0
    else
        print *, "✗ Some tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_simple_namelist_config()
        !! Test parsing simple namelist configuration
        type(config_t) :: config
        logical :: success
        character(len=1024) :: error_message
        
        test_count = test_count + 1
        print *, "Test: Simple namelist configuration..."
        
        ! Create test config file
        temp_file = "test_simple.nml"
        open(newunit=unit, file=temp_file, status='replace')
        write(unit, '(A)') "! Simple namelist config"
        write(unit, '(A)') "&fortcov_config"
        write(unit, '(A)') "    output_format = 'json'"
        write(unit, '(A)') "    output_path = 'coverage.json'"
        write(unit, '(A)') "    fail_under_threshold = 80.0"
        write(unit, '(A)') "    verbose = .true."
        write(unit, '(A)') "/"
        close(unit)
        
        ! Parse the config file
        config%config_file = temp_file
        call parse_namelist_config_file(config, success, error_message)
        
        if (.not. success) then
            print *, "  ✗ FAIL: Failed to parse namelist config"
            print *, "    Error: ", trim(error_message)
            failed_count = failed_count + 1
        else if (config%output_format /= 'json') then
            print *, "  ✗ FAIL: output_format not parsed correctly"
            print *, "    Expected: 'json', Got: '", config%output_format, "'"
            failed_count = failed_count + 1
        else if (config%output_path /= 'coverage.json') then
            print *, "  ✗ FAIL: output_path not parsed correctly"
            failed_count = failed_count + 1
        else if (abs(config%fail_under_threshold - 80.0) > 0.001) then
            print *, "  ✗ FAIL: fail_under_threshold not parsed correctly"
            failed_count = failed_count + 1
        else if (.not. config%verbose) then
            print *, "  ✗ FAIL: verbose not parsed correctly"
            failed_count = failed_count + 1
        else
            print *, "  ✓ PASS"
        end if
        
        ! Cleanup
        call delete_file(temp_file)
        
    end subroutine test_simple_namelist_config
    
    subroutine test_namelist_with_arrays()
        !! Test parsing namelist with array values
        type(config_t) :: config
        logical :: success
        character(len=1024) :: error_message
        
        test_count = test_count + 1
        print *, "Test: Namelist with array values..."
        
        ! Create test config file
        temp_file = "test_arrays.nml"
        open(newunit=unit, file=temp_file, status='replace')
        write(unit, '(A)') "&fortcov_config"
        write(unit, '(A)') "    source_paths = 'src/', 'lib/', 'app/'"
        write(unit, '(A)') "    exclude_patterns = '*.mod', '*.o', 'test/*'"
        write(unit, '(A)') "/"
        close(unit)
        
        ! Parse the config file
        config%config_file = temp_file
        call parse_namelist_config_file(config, success, error_message)
        
        if (.not. success) then
            print *, "  ✗ FAIL: Failed to parse namelist with arrays"
            print *, "    Error: ", trim(error_message)
            failed_count = failed_count + 1
        else if (.not. allocated(config%source_paths)) then
            print *, "  ✗ FAIL: source_paths not allocated"
            failed_count = failed_count + 1
        else if (size(config%source_paths) /= 3) then
            print *, "  ✗ FAIL: Wrong number of source_paths"
            print *, "    Expected: 3, Got: ", size(config%source_paths)
            failed_count = failed_count + 1
        else if (config%source_paths(1) /= 'src/') then
            print *, "  ✗ FAIL: First source_path incorrect"
            failed_count = failed_count + 1
        else
            print *, "  ✓ PASS"
        end if
        
        ! Cleanup
        call delete_file(temp_file)
        
    end subroutine test_namelist_with_arrays
    
    subroutine test_namelist_with_comments()
        !! Test that comments outside namelist block are handled correctly
        type(config_t) :: config
        logical :: success
        character(len=1024) :: error_message
        
        test_count = test_count + 1
        print *, "Test: Namelist with external comments..."
        
        ! Create test config file
        temp_file = "test_comments.nml"
        open(newunit=unit, file=temp_file, status='replace')
        write(unit, '(A)') "! This is a comment before the namelist"
        write(unit, '(A)') "! Another comment line"
        write(unit, '(A)') "&fortcov_config"
        write(unit, '(A)') "    output_format = 'markdown'"
        write(unit, '(A)') "/"
        write(unit, '(A)') "! Comment after the namelist"
        close(unit)
        
        ! Parse the config file
        config%config_file = temp_file
        call parse_namelist_config_file(config, success, error_message)
        
        if (.not. success) then
            print *, "  ✗ FAIL: Failed to parse namelist with comments"
            print *, "    Error: ", trim(error_message)
            failed_count = failed_count + 1
        else if (config%output_format /= 'markdown') then
            print *, "  ✗ FAIL: output_format not parsed correctly"
            failed_count = failed_count + 1
        else
            print *, "  ✓ PASS"
        end if
        
        ! Cleanup
        call delete_file(temp_file)
        
    end subroutine test_namelist_with_comments
    
    subroutine test_mixed_format_detection()
        !! Test detection and parsing of different config file formats
        type(config_t) :: config
        logical :: success
        character(len=1024) :: error_message
        
        test_count = test_count + 1
        print *, "Test: Format detection (namelist vs key=value)..."
        
        ! Test 1: Create namelist format file
        temp_file = "test_format.nml"
        open(newunit=unit, file=temp_file, status='replace')
        write(unit, '(A)') "&fortcov_config"
        write(unit, '(A)') "    verbose = .true."
        write(unit, '(A)') "/"
        close(unit)
        
        config%config_file = temp_file
        call parse_config_file(config, success, error_message)
        
        if (.not. success) then
            print *, "  ✗ FAIL: Failed to detect and parse namelist format"
            print *, "    Error: ", trim(error_message)
            failed_count = failed_count + 1
            call delete_file(temp_file)
            return
        end if
        
        ! Test 2: Create key=value format file
        temp_file = "test_format.cfg"
        open(newunit=unit, file=temp_file, status='replace')
        write(unit, '(A)') "# Simple key=value config"
        write(unit, '(A)') "verbose = true"
        write(unit, '(A)') "output_format = json"
        close(unit)
        
        config%config_file = temp_file
        call parse_config_file(config, success, error_message)
        
        if (.not. success) then
            print *, "  ✗ FAIL: Failed to detect and parse key=value format"
            print *, "    Error: ", trim(error_message)
            failed_count = failed_count + 1
        else
            print *, "  ✓ PASS"
        end if
        
        ! Cleanup
        call delete_file(temp_file)
        
    end subroutine test_mixed_format_detection
    
    subroutine test_iostat_5010_handling()
        !! Test proper handling of iostat=5010 (end-of-record) errors
        type(config_t) :: config
        logical :: success
        character(len=1024) :: error_message
        
        test_count = test_count + 1
        print *, "Test: iostat=5010 error handling..."
        
        ! Create test config file with potential EOR issue
        temp_file = "test_eor.nml"
        open(newunit=unit, file=temp_file, status='replace')
        write(unit, '(A)') "&fortcov_config"
        write(unit, '(A)') "    source_paths = 'src/',"
        write(unit, '(A)') "                   'lib/',"
        write(unit, '(A)') "                   'app/'"
        write(unit, '(A)') "    output_format = 'json'"
        write(unit, '(A)') "/"
        close(unit)
        
        ! Parse the config file
        config%config_file = temp_file
        call parse_namelist_config_file(config, success, error_message)
        
        if (.not. success) then
            ! Check if error mentions iostat=5010
            if (index(error_message, "5010") > 0) then
                print *, "  ✗ FAIL: iostat=5010 error not handled properly"
                print *, "    Error: ", trim(error_message)
                failed_count = failed_count + 1
            else
                print *, "  ✗ FAIL: Unexpected error"
                print *, "    Error: ", trim(error_message)
                failed_count = failed_count + 1
            end if
        else
            print *, "  ✓ PASS"
        end if
        
        ! Cleanup
        call delete_file(temp_file)
        
    end subroutine test_iostat_5010_handling
    
    subroutine test_empty_namelist()
        !! Test parsing empty namelist
        type(config_t) :: config
        logical :: success
        character(len=1024) :: error_message
        
        test_count = test_count + 1
        print *, "Test: Empty namelist..."
        
        ! Create test config file
        temp_file = "test_empty.nml"
        open(newunit=unit, file=temp_file, status='replace')
        write(unit, '(A)') "&fortcov_config"
        write(unit, '(A)') "/"
        close(unit)
        
        ! Parse the config file
        config%config_file = temp_file
        call parse_namelist_config_file(config, success, error_message)
        
        if (.not. success) then
            print *, "  ✗ FAIL: Failed to parse empty namelist"
            print *, "    Error: ", trim(error_message)
            failed_count = failed_count + 1
        else
            print *, "  ✓ PASS"
        end if
        
        ! Cleanup
        call delete_file(temp_file)
        
    end subroutine test_empty_namelist
    
    subroutine test_invalid_namelist_format()
        !! Test error handling for invalid namelist format
        type(config_t) :: config
        logical :: success
        character(len=1024) :: error_message
        
        test_count = test_count + 1
        print *, "Test: Invalid namelist format..."
        
        ! Create test config file with invalid format
        temp_file = "test_invalid.nml"
        open(newunit=unit, file=temp_file, status='replace')
        write(unit, '(A)') "&fortcov_config"
        write(unit, '(A)') "    invalid syntax here"
        write(unit, '(A)') "/"
        close(unit)
        
        ! Parse the config file
        config%config_file = temp_file
        call parse_namelist_config_file(config, success, error_message)
        
        if (success) then
            print *, "  ✗ FAIL: Should have failed for invalid namelist format"
            failed_count = failed_count + 1
        else if (len_trim(error_message) == 0) then
            print *, "  ✗ FAIL: No error message provided"
            failed_count = failed_count + 1
        else
            print *, "  ✓ PASS (correctly detected invalid format)"
        end if
        
        ! Cleanup
        call delete_file(temp_file)
        
    end subroutine test_invalid_namelist_format
    
    subroutine test_namelist_with_logical_values()
        !! Test parsing logical values in namelist
        type(config_t) :: config
        logical :: success
        character(len=1024) :: error_message
        
        test_count = test_count + 1
        print *, "Test: Namelist with logical values..."
        
        ! Create test config file
        temp_file = "test_logical.nml"
        open(newunit=unit, file=temp_file, status='replace')
        write(unit, '(A)') "&fortcov_config"
        write(unit, '(A)') "    verbose = .true."
        write(unit, '(A)') "    quiet = .false."
        write(unit, '(A)') "    tui_mode = .true."
        write(unit, '(A)') "    strict_mode = .false."
        write(unit, '(A)') "/"
        close(unit)
        
        ! Parse the config file
        config%config_file = temp_file
        call parse_namelist_config_file(config, success, error_message)
        
        if (.not. success) then
            print *, "  ✗ FAIL: Failed to parse logical values"
            print *, "    Error: ", trim(error_message)
            failed_count = failed_count + 1
        else if (.not. config%verbose) then
            print *, "  ✗ FAIL: verbose not parsed correctly"
            failed_count = failed_count + 1
        else if (config%quiet) then
            print *, "  ✗ FAIL: quiet not parsed correctly"
            failed_count = failed_count + 1
        else if (.not. config%tui_mode) then
            print *, "  ✗ FAIL: tui_mode not parsed correctly"
            failed_count = failed_count + 1
        else if (config%strict_mode) then
            print *, "  ✗ FAIL: strict_mode not parsed correctly"
            failed_count = failed_count + 1
        else
            print *, "  ✓ PASS"
        end if
        
        ! Cleanup
        call delete_file(temp_file)
        
    end subroutine test_namelist_with_logical_values
    
    subroutine delete_file(filename)
        !! Helper to delete temporary test files
        character(len=*), intent(in) :: filename
        logical :: exists
        
        inquire(file=filename, exist=exists)
        if (exists) then
            open(newunit=unit, file=filename, status='old')
            close(unit, status='delete')
        end if
    end subroutine delete_file
    
end program test_config_namelist_parsing_issue_250