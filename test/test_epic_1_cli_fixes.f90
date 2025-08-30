program test_epic_1_cli_fixes
    !! Test program for EPIC 1 CLI exit code fixes
    !! Tests for Issues #951, #952, #862 - CLI exit code corrections
    
    use config_core, only: parse_config, validate_config_with_context
    use config_types, only: config_t
    use error_handling_core, only: error_context_t, ERROR_SUCCESS
    use coverage_stats_reporter, only: apply_threshold_validation, line_coverage_stats_t
    use constants_core, only: EXIT_THRESHOLD_NOT_MET, EXIT_NO_COVERAGE_DATA, EXIT_SUCCESS
    implicit none
    
    character(len=256) :: error_message
    type(config_t) :: config
    type(error_context_t) :: error_ctx
    logical :: success
    integer :: test_count = 0, passed_count = 0
    
    print *, "========================================"
    print *, "EPIC 1: CLI Exit Code Corrections Tests"
    print *, "Issues #951, #952, #862"
    print *, "========================================"
    print *, ""
    
    ! Test Issue #951: --fail-under exit codes
    call test_fail_under_exit_codes()
    
    ! Test Issue #952: nonexistent source path exit codes
    call test_source_path_validation()
    
    ! Test threshold validation functionality
    call test_threshold_validation_logic()
    
    ! Final summary
    print *, "========================================"
    print '(A,I0,A,I0,A)', "EPIC 1 Summary: ", passed_count, "/", test_count, " tests passed"
    if (passed_count == test_count) then
        print *, "✅ ALL CLI EXIT CODE FIXES WORKING"
    else
        print *, "❌ SOME CLI EXIT CODE ISSUES REMAIN"
        call exit(1)
    end if
    print *, "========================================"

contains

    subroutine test_fail_under_exit_codes()
        print *, "=== Testing Issue #951: --fail-under exit codes ==="
        
        ! Test fail-under threshold validation logic
        call run_exit_code_test("fail-under below threshold", test_fail_under_below_threshold)
        call run_exit_code_test("fail-under meets threshold", test_fail_under_meets_threshold)
        
        print *, ""
    end subroutine test_fail_under_exit_codes
    
    subroutine test_source_path_validation()
        print *, "=== Testing Issue #952: nonexistent source path exit codes ==="
        
        ! Test nonexistent source path validation
        call run_validation_test("nonexistent source path", test_nonexistent_source_path)
        
        print *, ""
    end subroutine test_source_path_validation
    
    subroutine test_threshold_validation_logic()
        print *, "=== Testing threshold validation logic ==="
        
        ! Test threshold validation function directly
        call run_threshold_test("threshold validation below", test_threshold_below)
        call run_threshold_test("threshold validation above", test_threshold_above)
        
        print *, ""
    end subroutine test_threshold_validation_logic
    
    subroutine run_exit_code_test(test_name, test_proc)
        character(len=*), intent(in) :: test_name
        interface
            subroutine test_proc(expected_exit_code, success, error_message)
                integer, intent(out) :: expected_exit_code
                logical, intent(out) :: success
                character(len=*), intent(out) :: error_message
            end subroutine test_proc
        end interface
        
        integer :: expected_exit_code
        logical :: success
        character(len=256) :: error_message
        
        test_count = test_count + 1
        call test_proc(expected_exit_code, success, error_message)
        
        if (success) then
            passed_count = passed_count + 1
            print '(A,A,A)', "  ✅ PASS: ", trim(test_name)
        else
            print '(A,A,A)', "  ❌ FAIL: ", trim(test_name)
            print '(A,A)', "     Error: ", trim(error_message)
        end if
    end subroutine run_exit_code_test
    
    subroutine run_validation_test(test_name, test_proc)
        character(len=*), intent(in) :: test_name
        interface
            subroutine test_proc(expected_has_error, success, error_message)
                logical, intent(out) :: expected_has_error
                logical, intent(out) :: success
                character(len=*), intent(out) :: error_message
            end subroutine test_proc
        end interface
        
        logical :: expected_has_error, success
        character(len=256) :: error_message
        
        test_count = test_count + 1
        call test_proc(expected_has_error, success, error_message)
        
        if (success) then
            passed_count = passed_count + 1
            print '(A,A,A)', "  ✅ PASS: ", trim(test_name)
        else
            print '(A,A,A)', "  ❌ FAIL: ", trim(test_name)
            print '(A,A)', "     Error: ", trim(error_message)
        end if
    end subroutine run_validation_test
    
    subroutine run_threshold_test(test_name, test_proc)
        character(len=*), intent(in) :: test_name
        interface
            subroutine test_proc(expected_exit_code, success, error_message)
                integer, intent(out) :: expected_exit_code
                logical, intent(out) :: success
                character(len=*), intent(out) :: error_message
            end subroutine test_proc
        end interface
        
        integer :: expected_exit_code
        logical :: success
        character(len=256) :: error_message
        
        test_count = test_count + 1
        call test_proc(expected_exit_code, success, error_message)
        
        if (success) then
            passed_count = passed_count + 1
            print '(A,A,A)', "  ✅ PASS: ", trim(test_name)
        else
            print '(A,A,A)', "  ❌ FAIL: ", trim(test_name)
            print '(A,A)', "     Error: ", trim(error_message)
        end if
    end subroutine run_threshold_test
    
    ! Test procedure implementations for EPIC 1 exit code fixes
    subroutine test_fail_under_below_threshold(expected_exit_code, success, error_message)
        integer, intent(out) :: expected_exit_code
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        type(line_coverage_stats_t) :: stats
        type(config_t) :: config
        integer :: actual_exit_code
        
        ! Setup: coverage below threshold
        stats%percentage = 50.0  ! Below threshold
        config%fail_under_threshold = 80.0  ! Set threshold
        config%quiet = .true.  ! Suppress output during test
        
        actual_exit_code = apply_threshold_validation(stats, config)
        expected_exit_code = EXIT_THRESHOLD_NOT_MET
        
        if (actual_exit_code == expected_exit_code) then
            success = .true.
            error_message = ""
        else
            success = .false.
            write(error_message, '(A,I0,A,I0)') &
                "Expected exit code ", expected_exit_code, ", got ", actual_exit_code
        end if
    end subroutine test_fail_under_below_threshold
    
    subroutine test_fail_under_meets_threshold(expected_exit_code, success, error_message)
        integer, intent(out) :: expected_exit_code
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        type(line_coverage_stats_t) :: stats
        type(config_t) :: config
        integer :: actual_exit_code
        
        ! Setup: coverage meets threshold
        stats%percentage = 90.0  ! Above threshold
        config%fail_under_threshold = 80.0  ! Set threshold
        config%quiet = .true.  ! Suppress output during test
        
        actual_exit_code = apply_threshold_validation(stats, config)
        expected_exit_code = EXIT_SUCCESS
        
        if (actual_exit_code == expected_exit_code) then
            success = .true.
            error_message = ""
        else
            success = .false.
            write(error_message, '(A,I0,A,I0)') &
                "Expected exit code ", expected_exit_code, ", got ", actual_exit_code
        end if
    end subroutine test_fail_under_meets_threshold
    
    subroutine test_nonexistent_source_path(expected_has_error, success, error_message)
        logical, intent(out) :: expected_has_error
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        type(config_t) :: config
        character(len=256) :: args(2)
        logical :: parse_success
        
        ! Setup: nonexistent source path
        args(1) = "--source=nonexistent_directory"
        args(2) = "dummy.gcov"
        
        call parse_config(args, config, parse_success, error_message)
        
        ! Validation
        call validate_config_with_context(config, error_ctx)
        
        expected_has_error = .true.
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (index(error_ctx%message, "Source path not found") > 0) then
                success = .true.
                error_message = ""
            else
                success = .false.
                error_message = "Expected 'Source path not found' error, got: " // trim(error_ctx%message)
            end if
        else
            success = .false.
            error_message = "Expected validation error for nonexistent source path"
        end if
    end subroutine test_nonexistent_source_path
    
    subroutine test_threshold_below(expected_exit_code, success, error_message)
        integer, intent(out) :: expected_exit_code
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        type(line_coverage_stats_t) :: stats
        type(config_t) :: config
        integer :: actual_exit_code
        
        ! Test minimum coverage threshold (warning, not failure)
        stats%percentage = 70.0
        config%minimum_coverage = 80.0
        config%fail_under_threshold = 0.0  ! No fail-under
        config%quiet = .true.
        
        actual_exit_code = apply_threshold_validation(stats, config)
        expected_exit_code = EXIT_SUCCESS  ! Warning only, not failure
        
        if (actual_exit_code == expected_exit_code) then
            success = .true.
            error_message = ""
        else
            success = .false.
            write(error_message, '(A,I0,A,I0)') &
                "Expected exit code ", expected_exit_code, ", got ", actual_exit_code
        end if
    end subroutine test_threshold_below
    
    subroutine test_threshold_above(expected_exit_code, success, error_message)
        integer, intent(out) :: expected_exit_code
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        type(line_coverage_stats_t) :: stats
        type(config_t) :: config
        integer :: actual_exit_code
        
        ! Test coverage above all thresholds
        stats%percentage = 95.0
        config%minimum_coverage = 80.0
        config%fail_under_threshold = 85.0
        config%quiet = .true.
        
        actual_exit_code = apply_threshold_validation(stats, config)
        expected_exit_code = EXIT_SUCCESS
        
        if (actual_exit_code == expected_exit_code) then
            success = .true.
            error_message = ""
        else
            success = .false.
            write(error_message, '(A,I0,A,I0)') &
                "Expected exit code ", expected_exit_code, ", got ", actual_exit_code
        end if
    end subroutine test_threshold_above

end program test_epic_1_cli_fixes