! Comprehensive CLI flag combination tests for --quiet flag
!
! These tests validate that --quiet flag works correctly in combination
! with all other CLI flags supported by fortcov.
!
! All tests follow Given-When-Then documentation pattern.
! Tests validate configuration parsing and flag interaction behavior.
program test_quiet_flag_combinations
    use fortcov_config
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing --quiet flag combinations..."
    
    ! Basic flag combinations
    all_tests_passed = all_tests_passed .and. test_quiet_verbose_combination()
    all_tests_passed = all_tests_passed .and. test_quiet_help_combination()
    all_tests_passed = all_tests_passed .and. test_quiet_version_combination()
    
    ! Format and output combinations
    all_tests_passed = all_tests_passed .and. test_quiet_output_format_combinations()
    all_tests_passed = all_tests_passed .and. test_quiet_input_format_combinations()
    all_tests_passed = all_tests_passed .and. test_quiet_output_path_combinations()
    
    ! Source and filtering combinations
    all_tests_passed = all_tests_passed .and. test_quiet_source_combinations()
    all_tests_passed = all_tests_passed .and. test_quiet_exclude_combinations()
    all_tests_passed = all_tests_passed .and. test_quiet_gcov_combinations()
    
    ! Threshold and validation combinations
    all_tests_passed = all_tests_passed .and. test_quiet_fail_under_combinations()
    all_tests_passed = all_tests_passed .and. test_quiet_strict_combinations()
    
    ! Advanced feature combinations
    all_tests_passed = all_tests_passed .and. test_quiet_diff_combinations()
    all_tests_passed = all_tests_passed .and. test_quiet_import_combinations()
    all_tests_passed = all_tests_passed .and. test_quiet_tui_combinations()
    
    ! Edge case combinations
    all_tests_passed = all_tests_passed .and. test_quiet_multiple_flags()
    all_tests_passed = all_tests_passed .and. test_quiet_flag_ordering()
    
    if (all_tests_passed) then
        print *, "All quiet flag combination tests PASSED"
        call exit(0)
    else
        print *, "Some quiet flag combination tests FAILED"
        call exit(1)
    end if

contains

    ! Test 1: Quiet + Verbose combination behavior
    ! Given: --quiet and --verbose flags together
    ! When: Parsing configuration
    ! Then: Both flags should be set (quiet suppresses verbose output per architecture)
    function test_quiet_verbose_combination() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet + --verbose combination"
        
        ! Setup: Both verbose and quiet flags
        allocate(character(len=15) :: args(3))
        args(1) = "--quiet"
        args(2) = "--verbose"
        args(3) = "--source=."
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Both flags set, quiet takes precedence for output control
        passed = success .and. config%quiet .and. config%verbose
        
        if (.not. passed) then
            print *, "    FAILED: Expected both quiet=T and verbose=T"
            print *, "      Got quiet=", config%quiet, ", verbose=", config%verbose
        else
            print *, "    PASSED"
        end if
    end function test_quiet_verbose_combination

    ! Test 2: Quiet + Help should show help regardless of quiet
    ! Given: --quiet and --help flags
    ! When: Parsing configuration
    ! Then: Should show help (help overrides quiet for help output)
    function test_quiet_help_combination() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet + --help combination"
        
        ! Setup: Quiet and help flags
        allocate(character(len=15) :: args(2))
        args(1) = "--quiet"
        args(2) = "--help"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Help takes precedence (early exit)
        passed = .not. success .and. config%show_help .and. config%quiet
        
        if (.not. passed) then
            print *, "    FAILED: Expected show_help=T, success=F, quiet=T"
            print *, "      Got show_help=", config%show_help, ", success=", success
        else
            print *, "    PASSED - Help overrides quiet"
        end if
    end function test_quiet_help_combination

    ! Test 3: Quiet + Version should show version regardless of quiet
    ! Given: --quiet and --version flags
    ! When: Parsing configuration
    ! Then: Should show version (version overrides quiet for version output)
    function test_quiet_version_combination() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet + --version combination"
        
        ! Setup: Quiet and version flags
        allocate(character(len=15) :: args(2))
        args(1) = "--quiet"
        args(2) = "--version"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Version takes precedence (early exit)
        passed = .not. success .and. config%show_version .and. config%quiet
        
        if (.not. passed) then
            print *, "    FAILED: Expected show_version=T, success=F, quiet=T"
        else
            print *, "    PASSED - Version overrides quiet"
        end if
    end function test_quiet_version_combination

    ! Test 4: Quiet with all output format combinations
    ! Given: --quiet with each supported output format
    ! When: Parsing configuration
    ! Then: All formats should work with quiet flag
    function test_quiet_output_format_combinations() result(passed)
        logical :: passed
        logical :: markdown_test, json_test, xml_test, html_test
        
        print *, "  Test: --quiet + output format combinations"
        
        markdown_test = test_format_combination("markdown")
        json_test = test_format_combination("json")
        xml_test = test_format_combination("xml")
        html_test = test_format_combination("html")
        
        passed = markdown_test .and. json_test .and. xml_test .and. html_test
        
        if (.not. passed) then
            print *, "    FAILED: Format tests - MD:", markdown_test, &
                    ", JSON:", json_test, ", XML:", xml_test, ", HTML:", html_test
        else
            print *, "    PASSED - All formats work with quiet"
        end if
    end function test_quiet_output_format_combinations

    ! Helper function for format testing
    function test_format_combination(format) result(passed)
        character(len=*), intent(in) :: format
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        ! Setup
        allocate(character(len=30) :: args(3))
        args(1) = "--quiet"
        args(2) = "--output-format=" // trim(format)
        args(3) = "--source=."
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify
        passed = success .and. config%quiet .and. &
                (config%output_format == trim(format))
    end function test_format_combination

    ! Test 5: Quiet with input format combinations
    ! Given: --quiet with each supported input format
    ! When: Parsing configuration
    ! Then: All input formats should work with quiet
    function test_quiet_input_format_combinations() result(passed)
        logical :: passed
        logical :: gcov_test, lcov_test, json_test
        
        print *, "  Test: --quiet + input format combinations"
        
        gcov_test = test_input_format_combination("gcov")
        lcov_test = test_input_format_combination("lcov")
        json_test = test_input_format_combination("json")
        
        passed = gcov_test .and. lcov_test .and. json_test
        
        if (.not. passed) then
            print *, "    FAILED: Input format tests - GCOV:", gcov_test, &
                    ", LCOV:", lcov_test, ", JSON:", json_test
        else
            print *, "    PASSED - All input formats work with quiet"
        end if
    end function test_quiet_input_format_combinations

    ! Helper function for input format testing
    function test_input_format_combination(format) result(passed)
        character(len=*), intent(in) :: format
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        ! Setup
        allocate(character(len=30) :: args(3))
        args(1) = "--quiet"
        args(2) = "--input-format=" // trim(format)
        args(3) = "--source=."
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify
        passed = success .and. config%quiet .and. &
                (config%input_format == trim(format))
    end function test_input_format_combination

    ! Test 6: Quiet with output path combinations
    ! Given: --quiet with various output paths
    ! When: Parsing configuration
    ! Then: Output paths should work correctly with quiet
    function test_quiet_output_path_combinations() result(passed)
        logical :: passed
        logical :: stdout_test, file_test, path_test
        
        print *, "  Test: --quiet + output path combinations"
        
        ! Test stdout (default)
        stdout_test = test_output_path_combination("-")
        
        ! Test file output
        file_test = test_output_path_combination("report.md")
        
        ! Test path with directory
        path_test = test_output_path_combination("./output/coverage.json")
        
        passed = stdout_test .and. file_test .and. path_test
        
        if (.not. passed) then
            print *, "    FAILED: Output path tests - stdout:", stdout_test, &
                    ", file:", file_test, ", path:", path_test
        else
            print *, "    PASSED - All output paths work with quiet"
        end if
    end function test_quiet_output_path_combinations

    ! Helper function for output path testing
    function test_output_path_combination(output_path) result(passed)
        character(len=*), intent(in) :: output_path
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        ! Setup
        allocate(character(len=50) :: args(3))
        args(1) = "--quiet"
        args(2) = "--output=" // trim(output_path)
        args(3) = "--source=."
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify
        passed = success .and. config%quiet .and. &
                (config%output_path == trim(output_path))
    end function test_output_path_combination

    ! Test 7: Quiet with source path combinations
    ! Given: --quiet with various source configurations
    ! When: Parsing configuration
    ! Then: Source paths should work correctly with quiet
    function test_quiet_source_combinations() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet + source path combinations"
        
        ! Setup: Multiple source paths with quiet
        allocate(character(len=20) :: args(4))
        args(1) = "--quiet"
        args(2) = "--source=src"
        args(3) = "--source=lib"
        args(4) = "--source=app"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: All sources parsed with quiet
        passed = success .and. config%quiet .and. &
                (size(config%source_paths) == 3) .and. &
                (config%source_paths(1) == "src") .and. &
                (config%source_paths(2) == "lib") .and. &
                (config%source_paths(3) == "app")
        
        if (.not. passed) then
            print *, "    FAILED: Source combination with quiet failed"
        else
            print *, "    PASSED"
        end if
    end function test_quiet_source_combinations

    ! Test 8: Quiet with exclude pattern combinations
    ! Given: --quiet with exclude patterns
    ! When: Parsing configuration
    ! Then: Exclude patterns should work correctly with quiet
    function test_quiet_exclude_combinations() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet + exclude pattern combinations"
        
        ! Setup: Multiple exclude patterns with quiet
        allocate(character(len=20) :: args(4))
        args(1) = "--quiet"
        args(2) = "--exclude=*.mod"
        args(3) = "--exclude=test/*"
        args(4) = "--source=."
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Exclude patterns parsed with quiet
        passed = success .and. config%quiet .and. &
                (size(config%exclude_patterns) == 2) .and. &
                (config%exclude_patterns(1) == "*.mod") .and. &
                (config%exclude_patterns(2) == "test/*")
        
        if (.not. passed) then
            print *, "    FAILED: Exclude combination with quiet failed"
        else
            print *, "    PASSED"
        end if
    end function test_quiet_exclude_combinations

    ! Test 9: Quiet with gcov-related combinations
    ! Given: --quiet with gcov configuration flags
    ! When: Parsing configuration
    ! Then: Gcov settings should work correctly with quiet
    function test_quiet_gcov_combinations() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet + gcov combinations"
        
        ! Setup: Gcov configuration with quiet
        allocate(character(len=50) :: args(5))
        args(1) = "--quiet"
        args(2) = "--gcov=/usr/bin/gcov-10"
        args(3) = "--gcov-args=--branch-probabilities"
        args(4) = "--keep-gcov-files"
        args(5) = "--source=."
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Gcov settings parsed with quiet
        passed = success .and. config%quiet .and. &
                (config%gcov_executable == "/usr/bin/gcov-10") .and. &
                (config%gcov_args == "--branch-probabilities") .and. &
                config%keep_gcov_files
        
        if (.not. passed) then
            print *, "    FAILED: Gcov combination with quiet failed"
        else
            print *, "    PASSED"
        end if
    end function test_quiet_gcov_combinations

    ! Test 10: Quiet with fail-under threshold
    ! Given: --quiet with coverage threshold
    ! When: Parsing configuration
    ! Then: Threshold should work correctly with quiet
    function test_quiet_fail_under_combinations() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet + fail-under combinations"
        
        ! Setup: Threshold with quiet
        allocate(character(len=20) :: args(3))
        args(1) = "--quiet"
        args(2) = "--fail-under=85"
        args(3) = "--source=."
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Threshold parsed with quiet
        passed = success .and. config%quiet .and. &
                (abs(config%minimum_coverage - 85.0) < 0.001)
        
        if (.not. passed) then
            print *, "    FAILED: Fail-under combination with quiet failed"
        else
            print *, "    PASSED"
        end if
    end function test_quiet_fail_under_combinations

    ! Test 11: Quiet with strict mode
    ! Given: --quiet with --strict flag
    ! When: Parsing configuration
    ! Then: Strict mode should work correctly with quiet
    function test_quiet_strict_combinations() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet + --strict combination"
        
        ! Setup: Strict mode with quiet
        allocate(character(len=15) :: args(3))
        args(1) = "--quiet"
        args(2) = "--strict"
        args(3) = "--source=."
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Strict mode parsed with quiet
        passed = success .and. config%quiet .and. config%strict_mode
        
        if (.not. passed) then
            print *, "    FAILED: Strict combination with quiet failed"
            print *, "      Got quiet=", config%quiet, ", strict=", config%strict_mode
        else
            print *, "    PASSED"
        end if
    end function test_quiet_strict_combinations

    ! Test 12: Quiet with diff mode
    ! Given: --quiet with diff functionality
    ! When: Parsing configuration
    ! Then: Diff mode should work correctly with quiet
    function test_quiet_diff_combinations() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet + diff combinations"
        
        ! Setup: Diff mode with quiet
        allocate(character(len=40) :: args(3))
        args(1) = "--quiet"
        args(2) = "--diff=baseline.json,current.json"
        args(3) = "--include-unchanged"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Diff mode parsed with quiet
        passed = success .and. config%quiet .and. config%enable_diff .and. &
                config%include_unchanged .and. &
                (config%diff_baseline_file == "baseline.json") .and. &
                (config%diff_current_file == "current.json")
        
        if (.not. passed) then
            print *, "    FAILED: Diff combination with quiet failed"
        else
            print *, "    PASSED"
        end if
    end function test_quiet_diff_combinations

    ! Test 13: Quiet with import functionality
    ! Given: --quiet with JSON import
    ! When: Parsing configuration
    ! Then: Import should work correctly with quiet
    function test_quiet_import_combinations() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet + import combinations"
        
        ! Setup: Import with quiet
        allocate(character(len=30) :: args(2))
        args(1) = "--quiet"
        args(2) = "--import=coverage.json"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Import configured with quiet
        passed = success .and. config%quiet .and. &
                (config%import_file == "coverage.json") .and. &
                (config%input_format == "json")  ! Auto-set by import
        
        if (.not. passed) then
            print *, "    FAILED: Import combination with quiet failed"
        else
            print *, "    PASSED"
        end if
    end function test_quiet_import_combinations

    ! Test 14: Quiet with TUI mode
    ! Given: --quiet with --tui flag
    ! When: Parsing configuration
    ! Then: TUI mode should work with quiet (quiet affects TUI output)
    function test_quiet_tui_combinations() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet + --tui combination"
        
        ! Setup: TUI mode with quiet
        allocate(character(len=15) :: args(3))
        args(1) = "--quiet"
        args(2) = "--tui"
        args(3) = "--source=."
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: TUI mode configured with quiet
        passed = success .and. config%quiet .and. config%tui_mode
        
        if (.not. passed) then
            print *, "    FAILED: TUI combination with quiet failed"
        else
            print *, "    PASSED"
        end if
    end function test_quiet_tui_combinations

    ! Test 15: Multiple flags with quiet
    ! Given: Many flags combined with --quiet
    ! When: Parsing configuration
    ! Then: All flags should be parsed correctly
    function test_quiet_multiple_flags() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --quiet with multiple flags"
        
        ! Setup: Many flags combined
        allocate(character(len=30) :: args(8))
        args(1) = "--quiet"
        args(2) = "--verbose"
        args(3) = "--output-format=json"
        args(4) = "--output=report.json"
        args(5) = "--source=src"
        args(6) = "--exclude=*.mod"
        args(7) = "--fail-under=90"
        args(8) = "--strict"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: All flags parsed correctly
        passed = success .and. config%quiet .and. config%verbose .and. &
                (config%output_format == "json") .and. &
                (config%output_path == "report.json") .and. &
                (size(config%source_paths) == 1) .and. &
                (config%source_paths(1) == "src") .and. &
                (size(config%exclude_patterns) == 1) .and. &
                (config%exclude_patterns(1) == "*.mod") .and. &
                (abs(config%minimum_coverage - 90.0) < 0.001) .and. &
                config%strict_mode
        
        if (.not. passed) then
            print *, "    FAILED: Multiple flags with quiet failed"
        else
            print *, "    PASSED"
        end if
    end function test_quiet_multiple_flags

    ! Test 16: Flag ordering with quiet
    ! Given: --quiet in different positions in argument list
    ! When: Parsing configuration
    ! Then: Position should not affect parsing
    function test_quiet_flag_ordering() result(passed)
        logical :: passed
        logical :: first_test, middle_test, last_test
        
        print *, "  Test: --quiet flag ordering"
        
        ! Test quiet first
        first_test = test_flag_order_combination([character(len=20) :: &
            "--quiet", "--verbose", "--source=."])
        
        ! Test quiet middle
        middle_test = test_flag_order_combination([character(len=20) :: &
            "--verbose", "--quiet", "--source=."])
        
        ! Test quiet last
        last_test = test_flag_order_combination([character(len=20) :: &
            "--verbose", "--source=.", "--quiet"])
        
        passed = first_test .and. middle_test .and. last_test
        
        if (.not. passed) then
            print *, "    FAILED: Flag ordering tests - first:", first_test, &
                    ", middle:", middle_test, ", last:", last_test
        else
            print *, "    PASSED - Flag ordering doesn't matter"
        end if
    end function test_quiet_flag_ordering

    ! Helper function for flag order testing
    function test_flag_order_combination(args) result(passed)
        character(len=*), intent(in) :: args(:)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Quiet and verbose both set regardless of order
        passed = success .and. config%quiet .and. config%verbose
    end function test_flag_order_combination

end program test_quiet_flag_combinations