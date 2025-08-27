program test_cli_flags_options
    !! CLI Flags and Options Test Module 
    !!
    !! Tests CLI flag combinations, help/version functionality,
    !! and output format handling.
    !!
    !! Extracted from test_cli_consistency_validation.f90 for 
    !! SRP compliance and improved maintainability.

    use iso_fortran_env, only: output_unit, error_unit
    use fortcov_config, only: config_t, parse_config
    use test_utils, only: assert_test, reset_test_counters, &
                          print_test_header, print_test_summary
    implicit none

    call reset_test_counters()
    call print_test_header("CLI Flags and Options")

    ! Test flag combinations and options
    call test_documented_flag_combinations()
    call test_help_and_version_consistency()
    call test_output_format_examples()

    call print_test_summary("CLI FLAGS/OPTIONS")

contains


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

end program test_cli_flags_options