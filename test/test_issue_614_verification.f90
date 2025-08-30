program test_issue_614_verification
    !! Comprehensive test for Issue #614: Empty output file path fix
    !! Verifies that default output paths are correctly applied for all formats
    
    use config_core, only: config_t, parse_config
    use config_types, only: MAX_ARRAY_SIZE
    implicit none
    
    logical :: all_tests_passed
    
    print '(A)', "=== Issue #614 Verification Test ==="
    print '(A)', "Testing default output path generation for all formats"
    print '(A)', ""
    
    all_tests_passed = .true.
    
    ! Test 1: Markdown format (default)
    call test_format("markdown", "coverage.md", all_tests_passed)
    
    ! Test 2: JSON format
    call test_format("json", "coverage.json", all_tests_passed)
    
    ! Test 3: XML format  
    call test_format("xml", "coverage.xml", all_tests_passed)
    
    ! Test 4: HTML format
    call test_format("html", "coverage.html", all_tests_passed)
    
    ! Test 5: Text format
    call test_format("text", "coverage.txt", all_tests_passed)
    
    print '(A)', ""
    if (all_tests_passed) then
        print '(A)', "✅ ALL TESTS PASSED - Issue #614 is FIXED"
        print '(A)', "   Default output paths are correctly applied for all formats"
    else
        print '(A)', "❌ SOME TESTS FAILED - Issue #614 still exists"
        stop 1
    end if
    
contains

    subroutine test_format(format_name, expected_output, test_status)
        character(len=*), intent(in) :: format_name
        character(len=*), intent(in) :: expected_output
        logical, intent(inout) :: test_status
        
        character(len=256) :: test_args(3)
        type(config_t) :: config
        logical :: parse_success
        character(len=512) :: error_msg
        
        ! Build test arguments dynamically
        test_args(1) = "--source=src"
        test_args(2) = "--format=" // trim(format_name)
        test_args(3) = "test.gcov"
        
        ! Parse configuration with no --output flag
        call parse_config(test_args, config, parse_success, error_msg)
        
        if (.not. parse_success) then
            print '(A,A,A)', "failed: " // trim(error_msg)
            test_status = .false.
            return
        end if
        
        ! Check if output path was set to expected default
        if (len_trim(config%output_path) == 0) then
            print '(A,A,A)', "❌ FAIL: ", format_name, " - Output path is empty"
            test_status = .false.
        else if (trim(config%output_path) == trim(expected_output)) then
            print '(A,A,A,A,A)', "✅ PASS: ", format_name, " -> ", trim(config%output_path), &
                                   " (correct default)"
        else
            print '(A,A,A,A,A,A,A)', "❌ FAIL: ", format_name, " -> ", trim(config%output_path), &
                                      " (expected: ", trim(expected_output), ")"
            test_status = .false.
        end if
        
    end subroutine test_format

end program test_issue_614_verification