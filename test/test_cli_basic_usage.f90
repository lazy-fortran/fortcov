program test_cli_basic_usage
    !! CLI Basic Usage Test Module - README Examples Validation
    !!
    !! Tests basic CLI usage examples from README.md to ensure
    !! documentation and implementation consistency.
    !!
    !! Extracted from test_cli_consistency_validation.f90 for 
    !! SRP compliance and improved maintainability.

    use iso_fortran_env, only: output_unit, error_unit
    use fortcov_config, only: config_t, parse_config
    implicit none

    integer :: test_count = 0
    integer :: passed_tests = 0
    logical :: all_tests_passed = .true.

    write(output_unit, '(A)') "======================================================="
    write(output_unit, '(A)') "            CLI Basic Usage Test Suite                "
    write(output_unit, '(A)') "======================================================="
    write(output_unit, '(A)') ""

    ! Test basic README examples
    call test_readme_basic_usage_examples()
    call test_readme_manual_file_specification()
    call test_readme_cicd_integration_example()

    write(output_unit, '(A)') ""
    write(output_unit, '(A)') "======================================================="
    write(*, '(A,I0,A,I0,A)') "CLI BASIC USAGE: ", passed_tests, "/", &
                              test_count, " tests passed"

    if (all_tests_passed) then
        write(output_unit, '(A)') "✅ ALL BASIC CLI EXAMPLES WORK AS DOCUMENTED"
        call exit(0)
    else
        write(output_unit, '(A)') "❌ CLI BASIC USAGE TESTS FAILED"
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

end program test_cli_basic_usage