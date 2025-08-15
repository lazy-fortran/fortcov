program test_html_default_file
    use fortcov_config
    use file_utils
    implicit none
    
    ! Test results tracking
    integer :: test_count = 0
    integer :: pass_count = 0
    
    write(*,*) "Running HTML default file tests..."
    
    ! Issue #104: HTML output format produces no output
    ! The problem is HTML content goes to stdout mixed with status messages
    ! Solution: Auto-generate default filename when HTML format without output file
    
    ! Test 1: HTML format without output file should set default filename
    call test_html_format_without_output_sets_default()
    
    ! Test 2: HTML format with explicit output file should preserve behavior
    call test_html_format_with_output_preserves_behavior()
    
    ! Test 3: Other formats without output should continue using stdout
    call test_other_formats_without_output_use_stdout()
    
    ! Report results
    write(*,*) ""
    write(*,'(A,I0,A,I0,A,I0,A)') "Tests: ", test_count, ", Passed: ", &
               pass_count, " (", (pass_count * 100) / test_count, "%)"
    
    if (pass_count /= test_count) then
        stop 1  ! Exit with error code
    end if
    
contains
    
    subroutine assert(condition, test_name, expected, actual)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name
        character(len=*), intent(in) :: expected, actual
        
        test_count = test_count + 1
        if (condition) then
            write(*,'(A,A)') "PASS: ", test_name
            pass_count = pass_count + 1
        else
            write(*,'(A,A)') "FAIL: ", test_name
            write(*,'(A,A)') "  Expected: ", expected
            write(*,'(A,A)') "  Actual:   ", actual
        end if
    end subroutine assert
    
    ! Test HTML format without output file sets default filename
    ! Given: config with output_format = "html" and output_path = "-"
    ! When: parse_config processes arguments like "--output-format=html test.gcov"
    ! Then: output_path should be set to "coverage_report.html"
    subroutine test_html_format_without_output_sets_default()
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message
        character(len=*), parameter :: arg1 = "--output-format=html"
        character(len=*), parameter :: arg2 = "test.gcov"
        character(len=100) :: args(2)
        
        ! Initialize args array
        args(1) = arg1
        args(2) = arg2
        
        ! When: Parse config with HTML format but no output file
        call parse_config(args, config, success, error_message)
        
        ! Then: Should succeed and set default HTML filename
        call assert(success, &
                   "parse_config succeeds with HTML format", "success", &
                   merge("success", "failure", success))
        
        ! This is the failing test - currently output_path remains "-"
        ! After implementation, this should pass
        call assert(config%output_path == "coverage_report.html", &
                   "HTML format without output sets default filename", &
                   "coverage_report.html", trim(config%output_path))
    end subroutine test_html_format_without_output_sets_default
    
    ! Test HTML format with explicit output file preserves behavior
    ! Given: config with output_format = "html" and explicit output file
    ! When: parse_config processes "--output-format=html --output=custom.html test.gcov"
    ! Then: output_path should remain "custom.html" (preserve existing behavior)
    subroutine test_html_format_with_output_preserves_behavior()
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message
        character(len=*), parameter :: arg1 = "--output-format=html"
        character(len=*), parameter :: arg2 = "--output=custom.html"
        character(len=*), parameter :: arg3 = "test.gcov"
        character(len=100) :: args(3)
        
        ! Initialize args array
        args(1) = arg1
        args(2) = arg2
        args(3) = arg3
        
        ! When: Parse config with HTML format and explicit output file
        call parse_config(args, config, success, error_message)
        
        ! Then: Should preserve explicit output filename
        call assert(success, &
                   "parse_config succeeds with explicit HTML output", "success", &
                   merge("success", "failure", success))
        
        call assert(config%output_path == "custom.html", &
                   "HTML format with explicit output preserves filename", &
                   "custom.html", trim(config%output_path))
    end subroutine test_html_format_with_output_preserves_behavior
    
    ! Test other formats without output should continue using stdout
    ! Given: config with output_format = "markdown" and no output file
    ! When: parse_config processes "--output-format=markdown test.gcov"
    ! Then: output_path should remain "-" (stdout)
    subroutine test_other_formats_without_output_use_stdout()
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message
        character(len=*), parameter :: arg1 = "--output-format=markdown"
        character(len=*), parameter :: arg2 = "test.gcov"
        character(len=100) :: args(2)
        
        ! Initialize args array
        args(1) = arg1
        args(2) = arg2
        
        ! When: Parse config with markdown format but no output file
        call parse_config(args, config, success, error_message)
        
        ! Then: Should preserve stdout behavior for non-HTML formats
        call assert(success, &
                   "parse_config succeeds with markdown format", "success", &
                   merge("success", "failure", success))
        
        call assert(config%output_path == "-", &
                   "Markdown format without output uses stdout", &
                   "-", trim(config%output_path))
    end subroutine test_other_formats_without_output_use_stdout
    
end program test_html_default_file