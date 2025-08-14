program test_error_handling
    use error_handling
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing Comprehensive Error Handling..."
    
    ! Test 1: Handle missing source files
    all_tests_passed = all_tests_passed .and. test_missing_source_files()
    
    ! Test 2: Handle permission denied
    all_tests_passed = all_tests_passed .and. test_permission_denied()
    
    ! Test 3: Handle out of memory
    all_tests_passed = all_tests_passed .and. test_out_of_memory()
    
    ! Test 4: Handle invalid configuration
    all_tests_passed = all_tests_passed .and. test_invalid_configuration()
    
    ! Test 5: Recover from single file error
    all_tests_passed = all_tests_passed .and. test_single_file_recovery()
    
    ! Test 6: Handle circular dependencies
    all_tests_passed = all_tests_passed .and. test_circular_dependencies()
    
    ! Test 7: Handle incomplete coverage data
    all_tests_passed = all_tests_passed .and. test_incomplete_coverage_data()
    
    ! Test 8: Stack trace on fatal errors
    all_tests_passed = all_tests_passed .and. test_stack_trace_fatal()
    
    ! Test 9: Bounds checking for safe writing functions
    all_tests_passed = all_tests_passed .and. test_safe_writing_bounds()
    
    if (all_tests_passed) then
        print *, "All tests PASSED"
        call exit(0)
    else
        print *, "Some tests FAILED"
        call exit(1)
    end if

contains


    function test_missing_source_files() result(passed)
        logical :: passed
        type(error_context_t) :: error_ctx
        character(len=*), parameter :: missing_source = "nonexistent.f90"
        
        print *, "  Test 1: Handle missing source files"
        
        ! Given: Coverage data referencing non-existent source
        ! When: Generating report
        call handle_missing_source(missing_source, error_ctx)
        
        ! Then: Should report missing file and show 0% coverage
        passed = (error_ctx%error_code == ERROR_MISSING_SOURCE_FILE) .and. &
                 (index(error_ctx%message, "missing") > 0) .and. &
                 (index(error_ctx%message, missing_source) > 0) .and. &
                 (error_ctx%recoverable .eqv. .true.)
        
        if (.not. passed) then
            print *, "    FAILED: Missing source files not handled properly"
        else
            print *, "    PASSED"
        end if
    end function test_missing_source_files

    function test_permission_denied() result(passed)
        logical :: passed
        type(error_context_t) :: error_ctx
        character(len=*), parameter :: readonly_path = "/root/readonly_report.md"
        
        print *, "  Test 2: Handle permission denied"
        
        ! Given: Output path without write permissions
        ! When: Writing report
        call handle_permission_denied(readonly_path, error_ctx)
        
        ! Then: Should report "Permission denied" with path
        passed = (error_ctx%error_code == ERROR_PERMISSION_DENIED) .and. &
                 (index(error_ctx%message, "Permission denied") > 0) .and. &
                 (index(error_ctx%message, readonly_path) > 0) .and. &
                 (error_ctx%recoverable .eqv. .false.)
        
        if (.not. passed) then
            print *, "    FAILED: Permission denied not handled properly"
        else
            print *, "    PASSED"
        end if
    end function test_permission_denied

    function test_out_of_memory() result(passed)
        logical :: passed
        type(error_context_t) :: error_ctx
        integer, parameter :: huge_size = 999999999
        
        print *, "  Test 3: Handle out of memory"
        
        ! Given: Extremely large coverage dataset
        ! When: Processing
        call handle_out_of_memory(huge_size, error_ctx)
        
        ! Then: Should report memory error and suggest solutions
        passed = (error_ctx%error_code == ERROR_OUT_OF_MEMORY) .and. &
                 (index(error_ctx%message, "memory") > 0) .and. &
                 (index(error_ctx%suggestion, "solutions") > 0) .and. &
                 (error_ctx%recoverable .eqv. .false.)
        
        if (.not. passed) then
            print *, "    FAILED: Out of memory not handled properly"
        else
            print *, "    PASSED"
        end if
    end function test_out_of_memory

    function test_invalid_configuration() result(passed)
        logical :: passed
        type(error_context_t) :: error_ctx
        character(len=*), parameter :: config_file = "temp_invalid.cfg"
        integer, parameter :: error_line = 5
        
        print *, "  Test 4: Handle invalid configuration"
        
        ! Given: Config file with syntax errors
        call create_invalid_config_file(config_file)
        
        ! When: Loading configuration
        call handle_invalid_config(config_file, error_line, error_ctx)
        
        ! Then: Should report line number and error details
        passed = (error_ctx%error_code == ERROR_INVALID_CONFIG) .and. &
                 (index(error_ctx%message, "line") > 0) .and. &
                 (index(error_ctx%message, "5") > 0) .and. &
                 (index(error_ctx%message, config_file) > 0)
        
        call cleanup_test_file(config_file)
        
        if (.not. passed) then
            print *, "    FAILED: Invalid configuration not handled properly"
        else
            print *, "    PASSED"
        end if
    end function test_invalid_configuration

    function test_single_file_recovery() result(passed)
        logical :: passed
        type(error_context_t) :: error_ctx
        character(len=256) :: test_files(3)
        integer :: processed_count
        
        print *, "  Test 5: Recover from single file error"
        
        ! Given: Multiple files, one corrupted
        test_files(1) = "valid1.gcda"
        test_files(2) = "corrupt_file.gcda"
        test_files(3) = "valid2.gcda"
        
        ! Create valid test files  
        call create_simple_text_file(test_files(1))
        call create_simple_text_file(test_files(3))
        ! test_files(2) intentionally left non-existent to simulate corruption
        
        ! When: Processing all files
        call handle_file_batch_processing(test_files, processed_count, error_ctx)
        
        ! Then: Should skip corrupted file and process others
        passed = (processed_count == 2) .and. &
                 (error_ctx%error_code == ERROR_PARTIAL_PROCESSING) .and. &
                 (error_ctx%recoverable .eqv. .true.)
        
        call cleanup_test_file(test_files(1))
        call cleanup_test_file(test_files(2))
        call cleanup_test_file(test_files(3))
        
        if (.not. passed) then
            print *, "    FAILED: Single file recovery not working properly"
            print *, "    Expected processed count: 2, got:", processed_count
        else
            print *, "    PASSED"
        end if
    end function test_single_file_recovery

    function test_circular_dependencies() result(passed)
        logical :: passed
        type(error_context_t) :: error_ctx
        character(len=256) :: modules(3)
        
        print *, "  Test 6: Handle circular dependencies"
        
        ! Given: Modules with circular use statements
        modules(1) = "module_a"
        modules(2) = "module_b"
        modules(3) = "module_c"
        
        ! When: Building hierarchy
        call handle_circular_dependency_detection(modules, error_ctx)
        
        ! Then: Should detect and report cycle
        passed = (error_ctx%error_code == ERROR_CIRCULAR_DEPENDENCY) .and. &
                 (index(error_ctx%message, "Circular") > 0) .and. &
                 (index(error_ctx%message, "cycle") > 0)
        
        if (.not. passed) then
            print *, "    FAILED: Circular dependencies not detected properly"
        else
            print *, "    PASSED"
        end if
    end function test_circular_dependencies

    function test_incomplete_coverage_data() result(passed)
        logical :: passed
        type(error_context_t) :: error_ctx
        character(len=*), parameter :: gcno_only = "test_incomplete.gcno"
        
        print *, "  Test 7: Handle incomplete coverage data"
        
        ! Given: Coverage file with incomplete data
        
        ! When: Processing coverage
        call handle_incomplete_coverage(gcno_only, error_ctx)
        
        ! Then: Should report as 0% coverage with warning
        passed = (error_ctx%error_code == ERROR_INCOMPLETE_COVERAGE) .and. &
                 (index(error_ctx%message, "incomplete") > 0) .and. &
                 (index(error_ctx%message, "0%") > 0) .and. &
                 (error_ctx%recoverable .eqv. .true.)
        
        
        if (.not. passed) then
            print *, "    FAILED: Incomplete coverage data not handled properly"
        else
            print *, "    PASSED"
        end if
    end function test_incomplete_coverage_data

    function test_stack_trace_fatal() result(passed)
        logical :: passed
        type(error_context_t) :: error_ctx
        logical, parameter :: verbose_mode = .true.
        
        print *, "  Test 8: Stack trace on fatal errors"
        
        ! Given: Unrecoverable error condition
        ! When: Error occurs
        call handle_fatal_error_with_trace(verbose_mode, error_ctx)
        
        ! Then: Should provide stack trace if verbose mode
        passed = (error_ctx%error_code == ERROR_FATAL) .and. &
                 (len_trim(error_ctx%stack_trace) > 0) .and. &
                 (index(error_ctx%stack_trace, "handle_fatal_error") > 0)
        
        if (.not. passed) then
            print *, "    FAILED: Stack trace not provided properly"
        else
            print *, "    PASSED"
        end if
    end function test_stack_trace_fatal

    function test_safe_writing_bounds() result(passed)
        logical :: passed
        type(error_context_t) :: error_ctx
        character(len=1000) :: long_text
        
        print *, "  Test 9: Bounds checking for safe writing functions"
        
        ! Given: A very long text that exceeds buffer limits
        long_text = repeat("This is a very long error message that should be truncated. ", 20)
        
        ! When: Using safe writing functions
        call clear_error_context(error_ctx)
        call safe_write_message(error_ctx, long_text)
        call safe_write_suggestion(error_ctx, long_text)
        call safe_write_context(error_ctx, long_text)
        
        ! Then: Should truncate with ellipsis and not overflow
        passed = (len_trim(error_ctx%message) <= 512) .and. &
                 (len_trim(error_ctx%suggestion) <= 512) .and. &
                 (len_trim(error_ctx%context) <= 256) .and. &
                 (index(error_ctx%message, "...") > 0)
        
        if (.not. passed) then
            print *, "    FAILED: Safe writing bounds checking not working"
            print *, "    Message length:", len_trim(error_ctx%message)
            print *, "    Suggestion length:", len_trim(error_ctx%suggestion)
            print *, "    Context length:", len_trim(error_ctx%context)
        else
            print *, "    PASSED"
        end if
    end function test_safe_writing_bounds

    ! Helper subroutines for test setup

    subroutine create_simple_text_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(A)') "Simple text file for testing"
        close(unit)
    end subroutine create_simple_text_file

    subroutine create_invalid_config_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(A)') "# Valid config"
        write(unit, '(A)') "source_paths = ['src']"
        write(unit, '(A)') "output_format = 'markdown'"
        write(unit, '(A)') "# Line 4: valid"
        write(unit, '(A)') "invalid syntax here @#$%"  ! Line 5: invalid
        close(unit)
    end subroutine create_invalid_config_file

    subroutine cleanup_test_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit, stat
        logical :: file_exists
        
        ! Check if file exists before attempting to open
        inquire(file=filename, exist=file_exists)
        if (.not. file_exists) return
        
        open(newunit=unit, file=filename, status='old', iostat=stat)
        if (stat == 0) then
            close(unit, status='delete')
        else
            ! If we somehow fail to open but file exists, 
            ! ensure unit is not leaked by closing if opened
            if (unit > 0) close(unit)
        end if
    end subroutine cleanup_test_file

end program test_error_handling