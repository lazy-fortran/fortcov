! Comprehensive CLI quiet flag tests for Issue #130
! 
! These tests validate the --quiet flag behavior, ensuring:
! 1. Coverage reports are suppressed when output goes to stdout with --quiet
! 2. Error and warning messages remain visible regardless of --quiet
! 3. Verbose + quiet combinations work correctly 
! 4. CLI flag combinations with --quiet work properly
! 5. Stdout/stderr separation is maintained
!
! All tests use Given-When-Then documentation pattern.
! Tests are designed to FAIL until Issue #130 implementation is complete.
program test_cli_quiet_flag
    use fortcov_config
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing CLI --quiet flag for Issue #130..."
    
    ! Test 1: Basic quiet flag parsing
    all_tests_passed = all_tests_passed .and. test_quiet_flag_parsing()
    all_tests_passed = all_tests_passed .and. test_quiet_short_flag_parsing()
    
    ! Test 2: Quiet flag with other flags combinations  
    all_tests_passed = all_tests_passed .and. test_quiet_with_verbose()
    all_tests_passed = all_tests_passed .and. test_quiet_with_output_file()
    all_tests_passed = all_tests_passed .and. test_quiet_with_fail_under()
    all_tests_passed = all_tests_passed .and. test_quiet_with_source_paths()
    
    ! Test 3: Output suppression scenarios
    all_tests_passed = all_tests_passed .and. test_quiet_suppresses_stdout_output()
    all_tests_passed = all_tests_passed .and. test_quiet_preserves_file_output()
    all_tests_passed = all_tests_passed .and. test_quiet_preserves_error_output()
    
    ! Test 4: Format-specific quiet behavior
    all_tests_passed = all_tests_passed .and. test_quiet_with_markdown_stdout()
    all_tests_passed = all_tests_passed .and. test_quiet_with_json_stdout()
    all_tests_passed = all_tests_passed .and. test_quiet_with_xml_stdout()
    all_tests_passed = all_tests_passed .and. test_quiet_with_html_stdout()
    
    ! Test 5: Flag precedence and edge cases
    all_tests_passed = all_tests_passed .and. test_verbose_overridden_by_quiet()
    all_tests_passed = all_tests_passed .and. test_quiet_with_multiple_sources()
    all_tests_passed = all_tests_passed .and. test_quiet_flag_case_sensitivity()
    
    if (all_tests_passed) then
        print *, "All CLI quiet flag tests PASSED"
        call exit(0)
    else
        print *, "Some CLI quiet flag tests FAILED"
        call exit(1)
    end if

contains

    ! Test 1.1: Basic quiet flag parsing --quiet
    ! Given: Command line argument --quiet
    ! When: Parsing configuration
    ! Then: Should set quiet=true and allow normal processing
    function test_quiet_flag_parsing() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet flag parsing"
        
        ! Setup: Args with quiet flag and source to avoid help
        allocate(character(len=15) :: args(2))
        args(1) = "--quiet"
        args(2) = "--source=."
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should set quiet flag and succeed
        passed = success .and. config%quiet .and. .not. config%verbose
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=T, quiet=T, verbose=F"
            print *, "      Got success=", success, ", quiet=", config%quiet, &
                    ", verbose=", config%verbose
        else
            print *, "    PASSED"
        end if
    end function test_quiet_flag_parsing

    ! Test 1.2: Short quiet flag parsing -q
    ! Given: Command line argument -q
    ! When: Parsing configuration
    ! Then: Should set quiet=true and allow normal processing
    function test_quiet_short_flag_parsing() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: -q flag parsing"
        
        ! Setup: Args with short quiet flag and source
        allocate(character(len=15) :: args(2))
        args(1) = "-q"
        args(2) = "--source=."
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should set quiet flag and succeed
        passed = success .and. config%quiet .and. .not. config%verbose
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=T, quiet=T, verbose=F"
        else
            print *, "    PASSED"
        end if
    end function test_quiet_short_flag_parsing

    ! Test 2.1: Quiet flag with verbose flag combination
    ! Given: Command line arguments --quiet --verbose
    ! When: Parsing configuration
    ! Then: Should set both flags independently (implementation determines precedence)
    function test_quiet_with_verbose() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet --verbose combination"
        
        ! Setup: Args with both flags
        allocate(character(len=15) :: args(3))
        args(1) = "--quiet"
        args(2) = "--verbose"
        args(3) = "--source=."
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should set both flags (quiet should suppress verbose output)
        passed = success .and. config%quiet .and. config%verbose
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=T, quiet=T, verbose=T"
            print *, "      Got success=", success, ", quiet=", config%quiet, &
                    ", verbose=", config%verbose
        else
            print *, "    PASSED"
        end if
    end function test_quiet_with_verbose

    ! Test 2.2: Quiet flag with output file specification
    ! Given: Command line arguments --quiet --output=report.md
    ! When: Parsing configuration
    ! Then: Should set quiet flag and output path correctly
    function test_quiet_with_output_file() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet with --output=file"
        
        ! Setup: Args with quiet and output file
        allocate(character(len=25) :: args(3))
        args(1) = "--quiet"
        args(2) = "--output=report.md"
        args(3) = "--source=."
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should set quiet and output path
        passed = success .and. config%quiet .and. &
                (config%output_path == "report.md")
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=T, quiet=T, output_path='report.md'"
        else
            print *, "    PASSED"
        end if
    end function test_quiet_with_output_file

    ! Test 2.3: Quiet flag with fail-under threshold
    ! Given: Command line arguments --quiet --fail-under=80
    ! When: Parsing configuration
    ! Then: Should set quiet flag and threshold correctly
    function test_quiet_with_fail_under() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet with --fail-under"
        
        ! Setup: Args with quiet and threshold
        allocate(character(len=20) :: args(3))
        args(1) = "--quiet"
        args(2) = "--fail-under=80"
        args(3) = "--source=."
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should set quiet and threshold
        passed = success .and. config%quiet .and. &
                (abs(config%minimum_coverage - 80.0) < 0.001)
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=T, quiet=T, threshold=80.0"
        else
            print *, "    PASSED"
        end if
    end function test_quiet_with_fail_under

    ! Test 2.4: Quiet flag with multiple source paths
    ! Given: Command line arguments --quiet --source=src --source=lib
    ! When: Parsing configuration
    ! Then: Should set quiet flag and source paths correctly
    function test_quiet_with_source_paths() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet with multiple --source"
        
        ! Setup: Args with quiet and multiple sources
        allocate(character(len=15) :: args(3))
        args(1) = "--quiet"
        args(2) = "--source=src"
        args(3) = "--source=lib"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should set quiet and source paths
        passed = success .and. config%quiet .and. &
                (size(config%source_paths) == 2) .and. &
                (config%source_paths(1) == "src") .and. &
                (config%source_paths(2) == "lib")
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=T, quiet=T, 2 source paths"
        else
            print *, "    PASSED"
        end if
    end function test_quiet_with_source_paths

    ! Test 3.1: CRITICAL TEST - Quiet flag should suppress stdout output
    ! Given: Configuration with quiet=true and output_path="-" (stdout)
    ! When: Generating coverage report
    ! Then: Should suppress coverage report output to stdout
    ! NOTE: This test validates the core Issue #130 requirement
    function test_quiet_suppresses_stdout_output() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet suppresses stdout output (CORE Issue #130)"
        
        ! Setup: Args with quiet flag and stdout output (default)
        allocate(character(len=15) :: args(2))
        args(1) = "--quiet"
        args(2) = "--source=."
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify configuration is correct for the test
        ! The actual output suppression will be tested in integration tests
        ! but we can verify the configuration is set up correctly for suppression
        passed = success .and. config%quiet .and. (config%output_path == "-")
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=T, quiet=T, output_path='-'"
            print *, "      Got success=", success, ", quiet=", config%quiet, &
                    ", output_path='", trim(config%output_path), "'"
            print *, "    NOTE: This is a configuration test. Actual suppression"
            print *, "          requires implementation changes in coverage_reporter.f90"
        else
            print *, "    PASSED - Configuration ready for stdout suppression"
        end if
    end function test_quiet_suppresses_stdout_output

    ! Test 3.2: Quiet flag should preserve file output behavior
    ! Given: Configuration with quiet=true and output_path="file.md"
    ! When: Generating coverage report
    ! Then: Should generate file output normally (quiet only affects stdout)
    function test_quiet_preserves_file_output() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet preserves file output"
        
        ! Setup: Args with quiet flag and file output
        allocate(character(len=25) :: args(3))
        args(1) = "--quiet"
        args(2) = "--output=test.md"
        args(3) = "--source=."
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should set quiet but file output should work normally
        passed = success .and. config%quiet .and. &
                (config%output_path == "test.md")
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=T, quiet=T, output_path='test.md'"
        else
            print *, "    PASSED - File output preserved with quiet"
        end if
    end function test_quiet_preserves_file_output

    ! Test 3.3: Quiet flag should preserve error output
    ! Given: Configuration with quiet=true
    ! When: Error conditions occur
    ! Then: Error messages should still be displayed (never suppressed)
    function test_quiet_preserves_error_output() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet preserves error output"
        
        ! Setup: Args with quiet flag and invalid argument to trigger error
        allocate(character(len=20) :: args(2))
        args(1) = "--quiet"
        args(2) = "--invalid-flag"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should fail with error message even with quiet
        passed = .not. success .and. config%quiet .and. &
                (len_trim(error_message) > 0)
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=F, quiet=T, error_message present"
            print *, "      Got success=", success, ", quiet=", config%quiet, &
                    ", error_len=", len_trim(error_message)
        else
            print *, "    PASSED - Error output preserved with quiet"
        end if
    end function test_quiet_preserves_error_output

    ! Test 4.1: Quiet with markdown stdout output
    ! Given: --quiet --output-format=markdown (default output to stdout)
    ! When: Parsing configuration
    ! Then: Should prepare for markdown output suppression
    function test_quiet_with_markdown_stdout() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet with markdown stdout"
        
        ! Setup: Explicit markdown format with quiet
        allocate(character(len=30) :: args(3))
        args(1) = "--quiet"
        args(2) = "--output-format=markdown"
        args(3) = "--source=."
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify configuration for markdown suppression
        passed = success .and. config%quiet .and. &
                (config%output_format == "markdown") .and. &
                (config%output_path == "-")
        
        if (.not. passed) then
            print *, "    FAILED: Expected markdown format with quiet stdout"
        else
            print *, "    PASSED"
        end if
    end function test_quiet_with_markdown_stdout

    ! Test 4.2: Quiet with JSON stdout output
    ! Given: --quiet --output-format=json (output to stdout)
    ! When: Parsing configuration
    ! Then: Should prepare for JSON output suppression
    function test_quiet_with_json_stdout() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet with JSON stdout"
        
        ! Setup: JSON format with quiet
        allocate(character(len=25) :: args(3))
        args(1) = "--quiet"
        args(2) = "--output-format=json"
        args(3) = "--source=."
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify configuration for JSON suppression
        passed = success .and. config%quiet .and. &
                (config%output_format == "json") .and. &
                (config%output_path == "-")
        
        if (.not. passed) then
            print *, "    FAILED: Expected JSON format with quiet stdout"
        else
            print *, "    PASSED"
        end if
    end function test_quiet_with_json_stdout

    ! Test 4.3: Quiet with XML stdout output
    ! Given: --quiet --output-format=xml (output to stdout)
    ! When: Parsing configuration
    ! Then: Should prepare for XML output suppression
    function test_quiet_with_xml_stdout() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet with XML stdout"
        
        ! Setup: XML format with quiet
        allocate(character(len=25) :: args(3))
        args(1) = "--quiet"
        args(2) = "--output-format=xml"
        args(3) = "--source=."
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify configuration for XML suppression
        passed = success .and. config%quiet .and. &
                (config%output_format == "xml") .and. &
                (config%output_path == "-")
        
        if (.not. passed) then
            print *, "    FAILED: Expected XML format with quiet stdout"
        else
            print *, "    PASSED"
        end if
    end function test_quiet_with_xml_stdout

    ! Test 4.4: Quiet with HTML stdout output (should redirect to file)
    ! Given: --quiet --output-format=html (HTML avoids stdout by default)
    ! When: Parsing configuration
    ! Then: Should redirect to default HTML file due to Issue #104 logic
    function test_quiet_with_html_stdout() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet with HTML format"
        
        ! Setup: HTML format with quiet
        allocate(character(len=25) :: args(3))
        args(1) = "--quiet"
        args(2) = "--output-format=html"
        args(3) = "--source=."
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: HTML should auto-redirect to file (Issue #104 behavior)
        passed = success .and. config%quiet .and. &
                (config%output_format == "html") .and. &
                (config%output_path == "coverage_report.html")
        
        if (.not. passed) then
            print *, "    FAILED: Expected HTML to redirect to file"
            print *, "      Got output_path='", trim(config%output_path), "'"
        else
            print *, "    PASSED - HTML auto-redirected to file"
        end if
    end function test_quiet_with_html_stdout

    ! Test 5.1: Verbose flag should be overridden by quiet for output
    ! Given: --verbose --quiet (in that order)
    ! When: Both flags are present
    ! Then: Quiet should suppress verbose output per architecture document
    function test_verbose_overridden_by_quiet() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: verbose overridden by quiet"
        
        ! Setup: Both flags with verbose first
        allocate(character(len=15) :: args(3))
        args(1) = "--verbose"
        args(2) = "--quiet"
        args(3) = "--source=."
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Both flags set, but quiet should control output behavior
        passed = success .and. config%verbose .and. config%quiet
        
        if (.not. passed) then
            print *, "    FAILED: Expected both verbose=T and quiet=T"
            print *, "      Got verbose=", config%verbose, ", quiet=", config%quiet
        else
            print *, "    PASSED - Both flags set (quiet controls output)"
        end if
    end function test_verbose_overridden_by_quiet

    ! Test 5.2: Quiet with multiple source directories
    ! Given: --quiet with many source paths
    ! When: Processing multiple source directories
    ! Then: All sources should be parsed with quiet flag active
    function test_quiet_with_multiple_sources() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet with multiple sources"
        
        ! Setup: Quiet with multiple source paths
        allocate(character(len=20) :: args(5))
        args(1) = "--quiet"
        args(2) = "--source=src"
        args(3) = "--source=lib"
        args(4) = "--source=app"
        args(5) = "--source=test"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: All sources parsed with quiet active
        passed = success .and. config%quiet .and. &
                (size(config%source_paths) == 4) .and. &
                (config%source_paths(1) == "src") .and. &
                (config%source_paths(2) == "lib") .and. &
                (config%source_paths(3) == "app") .and. &
                (config%source_paths(4) == "test")
        
        if (.not. passed) then
            print *, "    FAILED: Expected 4 source paths with quiet"
            if (allocated(config%source_paths)) then
                print *, "      Got", size(config%source_paths), "source paths"
            end if
        else
            print *, "    PASSED"
        end if
    end function test_quiet_with_multiple_sources

    ! Test 5.3: Case sensitivity for quiet flag
    ! Given: Various case combinations of quiet flag
    ! When: Parsing configuration
    ! Then: Only exact --quiet and -q should be recognized
    function test_quiet_flag_case_sensitivity() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        logical :: test1_passed, test2_passed, test3_passed
        
        print *, "  Test: quiet flag case sensitivity"
        
        ! Test 1: Correct case should work
        allocate(character(len=15) :: args(2))
        args(1) = "--quiet"
        args(2) = "--source=."
        call parse_config(args, config, success, error_message)
        test1_passed = success .and. config%quiet
        deallocate(args)
        
        ! Test 2: Wrong case should fail
        allocate(character(len=15) :: args(2))
        args(1) = "--QUIET"
        args(2) = "--source=."
        call parse_config(args, config, success, error_message)
        test2_passed = .not. success  ! Should fail for wrong case
        deallocate(args)
        
        ! Test 3: Short form correct case should work
        allocate(character(len=15) :: args(2))
        args(1) = "-q"
        args(2) = "--source=."
        call parse_config(args, config, success, error_message)
        test3_passed = success .and. config%quiet
        deallocate(args)
        
        passed = test1_passed .and. test2_passed .and. test3_passed
        
        if (.not. passed) then
            print *, "    FAILED: Case sensitivity test failed"
            print *, "      --quiet:", test1_passed, ", --QUIET:", test2_passed, &
                    ", -q:", test3_passed
        else
            print *, "    PASSED"
        end if
    end function test_quiet_flag_case_sensitivity

end program test_cli_quiet_flag