program test_cli_validation
    !! CLI Validation Test Module
    !!
    !! Tests error handling, threshold validation, and argument 
    !! parsing edge cases.
    !!
    !! Extracted from test_cli_consistency_validation.f90 for 
    !! SRP compliance and improved maintainability.

    use iso_fortran_env, only: output_unit, error_unit
    use config_core, only: config_t, parse_config
    use test_utils_core, only: assert_test, reset_test_counters, &
                          print_test_header, print_test_summary
    implicit none

    call reset_test_counters()
    call print_test_header("CLI Validation")

    ! Test validation and error handling
    call test_error_message_consistency()
    call test_threshold_validation_examples()
    call test_argument_parsing_edge_cases()

    call print_test_summary("CLI VALIDATION")

contains


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

end program test_cli_validation
