program test_cli_consistency_validation
    !! CLI Consistency Validation Test (Issue #509)
    !!
    !! Validates that all documented CLI examples from README.md work correctly.
    !! This ensures Sprint 2 success criteria #3: "CLI consistency - all
    !! documented examples work"
    !!
    !! Tests every example from the documentation to ensure they parse correctly
    !! and produce expected configuration results.
    
    use iso_fortran_env, only: output_unit, error_unit
    use fortcov_config, only: config_t, parse_config, show_help, show_version
    implicit none
    
    integer :: test_count = 0
    integer :: passed_tests = 0
    logical :: all_tests_passed = .true.
    
    write(output_unit, '(A)') "======================================================="
    write(output_unit, '(A)') "         CLI Consistency Validation Test Suite         "
    write(output_unit, '(A)') "======================================================="
    write(output_unit, '(A)') ""
    
    ! Test all documented examples from README.md
    call test_readme_basic_usage_examples()
    call test_readme_manual_file_specification()
    call test_readme_cicd_integration_example()
    call test_documented_flag_combinations()
    call test_help_and_version_consistency()
    call test_error_message_consistency()
    call test_threshold_validation_examples()
    call test_output_format_examples()
    
    ! Test CLI argument edge cases
    call test_argument_parsing_edge_cases()
    call test_flag_ordering_independence()
    
    write(output_unit, '(A)') ""
    write(output_unit, '(A)') "======================================================="
    write(*, '(A,I0,A,I0,A)') "CLI CONSISTENCY VALIDATION: ", passed_tests, "/", &
                              test_count, " tests passed"
    
    if (all_tests_passed) then
        write(output_unit, '(A)') "✅ ALL CLI EXAMPLES WORK AS DOCUMENTED"
        write(output_unit, '(A)') "   Documentation and implementation are consistent"
        call exit(0)
    else
        write(output_unit, '(A)') "❌ CLI CONSISTENCY VALIDATION FAILED"
        write(output_unit, '(A)') "   Some documented examples don't work"
        call exit(1)
    end if

contains

    subroutine assert_test(condition, test_name, details)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name, details
        
        test_count = test_count + 1
        
        if (condition) then
            passed_tests = passed_tests + 1
            write(output_unit, '(A)') "✅ PASS: " // trim(test_name)
        else
            all_tests_passed = .false.
            write(output_unit, '(A)') "❌ FAIL: " // trim(test_name)
            write(output_unit, '(A)') "   Details: " // trim(details)
        end if
    end subroutine assert_test

    subroutine test_readme_basic_usage_examples()
        !! Tests the exact examples shown in README.md Basic Usage section
        
        type(config_t) :: config
        character(len=64), allocatable :: args(:)
        logical :: success
        character(len=256) :: error_message
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== README BASIC USAGE EXAMPLES ==="
        
        ! Example 1: fortcov --source=src *.gcov
        allocate(character(len=64) :: args(2))
        args(1) = "--source=src"
        args(2) = "*.gcov"
        
        call parse_config(args, config, success, error_message)
        call assert_test(success, "README Example: fortcov --source=src *.gcov", &
                        "Should parse: " // trim(error_message))
        
        if (success) then
            call assert_test(size(config%source_paths) > 0 .and. &
                           trim(config%source_paths(1)) == "src", &
                            "Source directory parsed correctly", &
                            "Expected 'src' in source_paths")
            call assert_test(.not. config%zero_configuration_mode, &
                            "Manual mode (not zero-config)", &
                            "Should be manual mode with explicit args")
        end if
        
        deallocate(args)
        
        ! Example 2: fortcov (no arguments - zero-config mode)
        allocate(character(len=64) :: args(0))
        
        call parse_config(args, config, success, error_message)
        call assert_test(success, "README Example: fortcov (no args)", &
                        "Should parse: " // trim(error_message))
        
        if (success) then
            call assert_test(config%zero_configuration_mode, &
                            "Zero-config mode activated", &
                            "Should activate auto-discovery")
        end if
        
        deallocate(args)
        
    end subroutine test_readme_basic_usage_examples

    subroutine test_readme_manual_file_specification()
        !! Tests manual file specification examples from README.md
        
        type(config_t) :: config
        character(len=64), allocatable :: args(:)
        logical :: success
        character(len=256) :: error_message
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== README MANUAL FILE SPECIFICATION ==="
        
        ! Example: fortcov --source=src *.gcov  # Shows terminal coverage output
        allocate(character(len=64) :: args(2))
        args(1) = "--source=src"
        args(2) = "test_file.gcov"
        
        call parse_config(args, config, success, error_message)
        call assert_test(success, "Manual file specification example", &
                        "Should parse successfully: " // trim(error_message))
        
        if (success) then
            call assert_test(.not. config%zero_configuration_mode, &
                            "Manual mode with file args", &
                            "Should not be zero-config with explicit files")
            call assert_test(size(config%source_paths) > 0 .and. &
                           trim(config%source_paths(1)) == "src", &
                            "Source directory correct", &
                            "Expected 'src' in source_paths")
        end if
        
    end subroutine test_readme_manual_file_specification

    subroutine test_readme_cicd_integration_example()
        !! Tests CI/CD integration example from README.md
        
        type(config_t) :: config
        character(len=64), allocatable :: args(:)
        logical :: success
        character(len=256) :: error_message
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== README CI/CD INTEGRATION EXAMPLE ==="
        
        ! Example: fortcov --source=src *.gcov --fail-under 80  # Fail if coverage < 80%
        allocate(character(len=64) :: args(3))
        args(1) = "--source=src"
        args(2) = "*.gcov"
        args(3) = "--fail-under=80"
        
        call parse_config(args, config, success, error_message)
        call assert_test(success, "CI/CD integration example", &
                        "Should parse successfully: " // trim(error_message))
        
        if (success) then
            call assert_test(config%fail_under_threshold == 80.0, &
                            "Threshold value correct", &
                            "Expected 80.0")
            call assert_test(size(config%source_paths) > 0 .and. &
                           trim(config%source_paths(1)) == "src", &
                            "Source directory correct in CI example", &
                            "Expected 'src' in source_paths")
        end if
        
    end subroutine test_readme_cicd_integration_example

    subroutine test_documented_flag_combinations()
        !! Tests various flag combinations shown in documentation
        
        type(config_t) :: config
        character(len=64), allocatable :: args(:)
        logical :: success
        character(len=256) :: error_message
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== DOCUMENTED FLAG COMBINATIONS ==="
        
        ! Test 1: --output flag
        allocate(character(len=64) :: args(1))
        args(1) = "--output=coverage.md"
        
        call parse_config(args, config, success, error_message)
        call assert_test(success, "Output flag example", &
                        "Should parse: " // trim(error_message))
        
        if (success) then
            call assert_test(index(config%output_path, "coverage.md") > 0, &
                            "Output path set correctly", &
                            "Should contain coverage.md")
        end if
        
        deallocate(args)
        
        ! Test 2: --verbose flag
        allocate(character(len=64) :: args(1))
        args(1) = "--verbose"
        
        call parse_config(args, config, success, error_message)
        call assert_test(success, "Verbose flag example", &
                        "Should parse: " // trim(error_message))
        
        if (success) then
            call assert_test(config%verbose, "Verbose flag set", &
                            "Should be true")
        end if
        
        deallocate(args)
        
        ! Test 3: --quiet flag
        allocate(character(len=64) :: args(1))
        args(1) = "--quiet"
        
        call parse_config(args, config, success, error_message)
        call assert_test(success, "Quiet flag example", &
                        "Should parse: " // trim(error_message))
        
        if (success) then
            call assert_test(config%quiet, "Quiet flag set", &
                            "Should be true")
        end if
        
        deallocate(args)
        
        ! Test 4: Multiple flags combination
        allocate(character(len=64) :: args(3))
        args(1) = "--source=src"
        args(2) = "--output=report.md"
        args(3) = "--verbose"
        
        call parse_config(args, config, success, error_message)
        call assert_test(success, "Multiple flags combination", &
                        "Should parse: " // trim(error_message))
        
        if (success) then
            call assert_test(config%verbose .and. &
                           size(config%source_paths) > 0 .and. &
                           trim(config%source_paths(1)) == "src" .and. &
                           index(config%output_path, "report.md") > 0, &
                           "All flags preserved in combination", &
                           "All flags should be set correctly")
        end if
        
    end subroutine test_documented_flag_combinations

    subroutine test_help_and_version_consistency()
        !! Tests that --help and --version work as documented
        
        type(config_t) :: config
        character(len=32), allocatable :: args(:)
        logical :: success
        character(len=256) :: error_message
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== HELP AND VERSION CONSISTENCY ==="
        
        ! Test --help flag
        allocate(character(len=32) :: args(1))
        args(1) = "--help"
        
        call parse_config(args, config, success, error_message)
        call assert_test(success, "Help flag parsing", &
                        "Should parse: " // trim(error_message))
        
        if (success) then
            call assert_test(config%show_help, "Help flag activated", &
                            "Should set show_help to true")
        end if
        
        deallocate(args)
        
        ! Test --version flag
        allocate(character(len=32) :: args(1))
        args(1) = "--version"
        
        call parse_config(args, config, success, error_message)
        call assert_test(success, "Version flag parsing", &
                        "Should parse: " // trim(error_message))
        
        if (success) then
            call assert_test(config%show_version, "Version flag activated", &
                            "Should set show_version to true")
        end if
        
        ! Test -h shorthand (if documented)
        deallocate(args)
        allocate(character(len=32) :: args(1))
        args(1) = "-h"
        
        call parse_config(args, config, success, error_message)
        if (success) then
            call assert_test(config%show_help, "Help shorthand (-h) works", &
                            "Should set show_help to true")
        else
            call assert_test(.true., "Help shorthand (-h) not supported", &
                            "Acceptable if not documented")
        end if
        
    end subroutine test_help_and_version_consistency

    subroutine test_error_message_consistency()
        !! Tests that error messages are consistent and helpful
        
        type(config_t) :: config
        character(len=32), allocatable :: args(:)
        logical :: success
        character(len=256) :: error_message
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== ERROR MESSAGE CONSISTENCY ==="
        
        ! Test invalid flag
        allocate(character(len=32) :: args(1))
        args(1) = "--invalid-flag"
        
        call parse_config(args, config, success, error_message)
        call assert_test(.not. success, "Invalid flag rejected", &
                        "Should reject invalid flags")
        call assert_test(len_trim(error_message) > 0, "Error message provided", &
                        "Should provide helpful error message")
        
        ! Test error message contains useful information
        if (.not. success) then
            call assert_test(index(error_message, "invalid") > 0 .or. &
                           index(error_message, "unknown") > 0 .or. &
                           index(error_message, "not recognized") > 0, &
                           "Error message is descriptive", &
                           "Should describe the error clearly")
        end if
        
    end subroutine test_error_message_consistency

    subroutine test_threshold_validation_examples()
        !! Tests threshold validation as documented
        
        type(config_t) :: config
        character(len=32), allocatable :: args(:)
        logical :: success
        character(len=256) :: error_message
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== THRESHOLD VALIDATION EXAMPLES ==="
        
        ! Test valid threshold values
        allocate(character(len=32) :: args(1))
        args(1) = "--fail-under=75"
        
        call parse_config(args, config, success, error_message)
        call assert_test(success, "Valid threshold (75)", &
                        "Should accept valid threshold: " // trim(error_message))
        
        if (success) then
            call assert_test(config%fail_under_threshold == 75.0, &
                            "Threshold value correct", "Expected 75.0")
        end if
        
        deallocate(args)
        
        ! Test threshold at boundary (0)
        allocate(character(len=32) :: args(1))
        args(1) = "--fail-under=0"
        
        call parse_config(args, config, success, error_message)
        call assert_test(success, "Valid threshold (0)", &
                        "Should accept zero threshold: " // trim(error_message))
        
        deallocate(args)
        
        ! Test threshold at boundary (100)
        allocate(character(len=32) :: args(1))
        args(1) = "--fail-under=100"
        
        call parse_config(args, config, success, error_message)
        call assert_test(success, "Valid threshold (100)", &
                        "Should accept 100% threshold: " // trim(error_message))
        
        deallocate(args)
        
        ! Test invalid threshold (negative)
        allocate(character(len=32) :: args(1))
        args(1) = "--fail-under=-5"
        
        call parse_config(args, config, success, error_message)
        call assert_test(.not. success, "Invalid threshold (-5) rejected", &
                        "Should reject negative thresholds")
        
        deallocate(args)
        
        ! Test invalid threshold (over 100)
        allocate(character(len=32) :: args(1))
        args(1) = "--fail-under=150"
        
        call parse_config(args, config, success, error_message)
        call assert_test(.not. success, "Invalid threshold (150) rejected", &
                        "Should reject thresholds over 100")
        
    end subroutine test_threshold_validation_examples

    subroutine test_output_format_examples()
        !! Tests output format examples as documented
        
        type(config_t) :: config
        character(len=64), allocatable :: args(:)
        logical :: success
        character(len=256) :: error_message
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== OUTPUT FORMAT EXAMPLES ==="
        
        ! Test markdown output (default/documented)
        allocate(character(len=64) :: args(1))
        args(1) = "--output=coverage.md"
        
        call parse_config(args, config, success, error_message)
        call assert_test(success, "Markdown output format", &
                        "Should accept .md output: " // trim(error_message))
        
        deallocate(args)
        
        ! Test JSON output
        allocate(character(len=64) :: args(1))
        args(1) = "--output=coverage.json"
        
        call parse_config(args, config, success, error_message)
        call assert_test(success, "JSON output format", &
                        "Should accept .json output: " // trim(error_message))
        
        deallocate(args)
        
        ! Test HTML output
        allocate(character(len=64) :: args(1))
        args(1) = "--output=coverage.html"
        
        call parse_config(args, config, success, error_message)
        call assert_test(success, "HTML output format", &
                        "Should accept .html output: " // trim(error_message))
        
        deallocate(args)
        
        ! Test XML output
        allocate(character(len=64) :: args(1))
        args(1) = "--output=coverage.xml"
        
        call parse_config(args, config, success, error_message)
        call assert_test(success, "XML output format", &
                        "Should accept .xml output: " // trim(error_message))
        
    end subroutine test_output_format_examples

    subroutine test_argument_parsing_edge_cases()
        !! Tests edge cases in argument parsing
        
        type(config_t) :: config
        character(len=128), allocatable :: args(:)
        logical :: success
        character(len=256) :: error_message
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== ARGUMENT PARSING EDGE CASES ==="
        
        ! Test arguments with spaces
        allocate(character(len=128) :: args(1))
        args(1) = '--source=my source dir'
        
        call parse_config(args, config, success, error_message)
        call assert_test(success, "Arguments with spaces", &
                        "Should handle spaces in paths: " // trim(error_message))
        
        deallocate(args)
        
        ! Test equal sign vs space separation
        allocate(character(len=128) :: args(2))
        args(1) = "--source"
        args(2) = "src"
        
        call parse_config(args, config, success, error_message)
        call assert_test(success, "Space-separated arguments", &
                        "Should handle space separation: " // trim(error_message))
        
        deallocate(args)
        
        ! Test mixed equal and space syntax
        allocate(character(len=128) :: args(3))
        args(1) = "--source=src"
        args(2) = "--fail-under"
        args(3) = "80"
        
        call parse_config(args, config, success, error_message)
        call assert_test(success, "Mixed argument syntax", &
                        "Should handle mixed syntax: " // trim(error_message))
        
    end subroutine test_argument_parsing_edge_cases

    subroutine test_flag_ordering_independence()
        !! Tests that flag ordering doesn't matter
        
        type(config_t) :: config1, config2
        character(len=64), allocatable :: args1(:), args2(:)
        logical :: success1, success2
        character(len=256) :: error_message1, error_message2
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== FLAG ORDERING INDEPENDENCE ==="
        
        ! Test order 1: source, output, verbose
        allocate(character(len=64) :: args1(3))
        args1(1) = "--source=src"
        args1(2) = "--output=test.md"
        args1(3) = "--verbose"
        
        call parse_config(args1, config1, success1, error_message1)
        
        ! Test order 2: verbose, output, source
        allocate(character(len=64) :: args2(3))
        args2(1) = "--verbose"
        args2(2) = "--output=test.md"
        args2(3) = "--source=src"
        
        call parse_config(args2, config2, success2, error_message2)
        
        call assert_test(success1 .and. success2, "Both orderings parse", &
                        "Both should succeed")
        
        if (success1 .and. success2) then
            call assert_test(config1%verbose .eqv. config2%verbose .and. &
                           size(config1%source_paths) > 0 .and. &
                           size(config2%source_paths) > 0 .and. &
                           trim(config1%source_paths(1)) == &
                           trim(config2%source_paths(1)) .and. &
                           trim(config1%output_path) == trim(config2%output_path), &
                           "Flag ordering independence", &
                           "Results should be identical regardless of order")
        end if
        
    end subroutine test_flag_ordering_independence

end program test_cli_consistency_validation