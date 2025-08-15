! Comprehensive CLI argument parsing tests (TDD RED phase)
! 
! These tests expose critical bugs in CLI argument parsing, particularly:
! 1. CRITICAL BUG: Executable path treated as unknown argument
!    - Test shows: "Unknown argument: /path/to/fortcov" error
!    - Root cause: main.f90 likely passing argv(0) to parse_config
!
! 2. CLI argument validation completeness
!    - Flag parsing: --help, -h, --version, -V
!    - Parameter parsing: --fail-under=N, --output=FILE
!    - Invalid argument rejection with proper error messages
!    - Default behavior when no arguments provided
!
! All tests use Given-When-Then documentation pattern.
! Test failure indicates bugs in current CLI implementation.
program test_cli_parsing
    use fortcov_config
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing CLI Argument Parsing..."
    
    ! Test 1: Basic flag parsing
    all_tests_passed = all_tests_passed .and. test_help_flag_parsing()
    all_tests_passed = all_tests_passed .and. test_help_short_flag_parsing()
    all_tests_passed = all_tests_passed .and. test_version_flag_parsing()
    all_tests_passed = all_tests_passed .and. test_version_short_flag_parsing()
    
    ! Test 2: Directory argument parsing simulation 
    all_tests_passed = all_tests_passed .and. test_directory_argument_parsing()
    all_tests_passed = all_tests_passed .and. test_executable_path_not_treated_as_argument()
    
    ! Test 3: Threshold parsing
    all_tests_passed = all_tests_passed .and. test_threshold_parsing_equals()
    all_tests_passed = all_tests_passed .and. test_threshold_parsing_fail_under()
    all_tests_passed = all_tests_passed .and. test_invalid_threshold_values()
    
    ! Test 4: Invalid argument rejection
    all_tests_passed = all_tests_passed .and. test_invalid_argument_rejection()
    all_tests_passed = all_tests_passed .and. test_unknown_flag_rejection()
    
    ! Test 5: Argument-less execution
    all_tests_passed = all_tests_passed .and. test_no_arguments_execution()
    all_tests_passed = all_tests_passed .and. test_empty_arguments_execution()
    
    ! Test 6: Help/version exit behavior
    all_tests_passed = all_tests_passed .and. test_help_exit_behavior()
    all_tests_passed = all_tests_passed .and. test_version_exit_behavior()
    
    ! Test 7: Complex argument combinations
    all_tests_passed = all_tests_passed .and. test_multiple_valid_arguments()
    all_tests_passed = all_tests_passed .and. test_mixed_valid_invalid_arguments()
    
    if (all_tests_passed) then
        print *, "All CLI parsing tests PASSED"
        call exit(0)
    else
        print *, "Some CLI parsing tests FAILED"
        call exit(1)
    end if

contains

    ! Test 1.1: Help flag parsing --help
    ! Given: Command line argument --help
    ! When: Parsing configuration
    ! Then: Should set show_help=true and return success=false for early exit
    function test_help_flag_parsing() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --help flag parsing"
        
        ! Setup: Args with help flag
        allocate(character(len=10) :: args(1))
        args(1) = "--help"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should set show_help and return false for early exit
        passed = .not. success .and. config%show_help .and. .not. config%show_version
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=F, show_help=T, show_version=F"
            if (success) print *, "      Got success=T (should be F for early exit)"
            if (.not. config%show_help) print *, "      Got show_help=F (should be T)"
            if (config%show_version) print *, "      Got show_version=T (should be F)"
        else
            print *, "    PASSED"
        end if
    end function test_help_flag_parsing

    ! Test 1.2: Help short flag parsing -h
    ! Given: Command line argument -h
    ! When: Parsing configuration
    ! Then: Should set show_help=true and return success=false for early exit
    function test_help_short_flag_parsing() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: -h flag parsing"
        
        ! Setup: Args with short help flag
        allocate(character(len=10) :: args(1))
        args(1) = "-h"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should set show_help and return false for early exit
        passed = .not. success .and. config%show_help .and. .not. config%show_version
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=F, show_help=T, show_version=F"
        else
            print *, "    PASSED"
        end if
    end function test_help_short_flag_parsing

    ! Test 1.3: Version flag parsing --version
    ! Given: Command line argument --version
    ! When: Parsing configuration
    ! Then: Should set show_version=true and return success=false for early exit
    function test_version_flag_parsing() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --version flag parsing"
        
        ! Setup: Args with version flag
        allocate(character(len=15) :: args(1))
        args(1) = "--version"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should set show_version and return false for early exit
        passed = .not. success .and. config%show_version .and. .not. config%show_help
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=F, show_version=T, show_help=F"
        else
            print *, "    PASSED"
        end if
    end function test_version_flag_parsing

    ! Test 1.4: Version short flag parsing -V
    ! Given: Command line argument -V
    ! When: Parsing configuration
    ! Then: Should set show_version=true and return success=false for early exit
    function test_version_short_flag_parsing() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: -V flag parsing"
        
        ! Setup: Args with short version flag
        allocate(character(len=10) :: args(1))
        args(1) = "-V"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should set show_version and return false for early exit
        passed = .not. success .and. config%show_version .and. .not. config%show_help
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=F, show_version=T, show_help=F"
        else
            print *, "    PASSED"
        end if
    end function test_version_short_flag_parsing

    ! Test 2.1: Directory argument parsing simulation
    ! Given: Arguments like directory paths (simulating what main.f90 would pass)
    ! When: Parsing configuration
    ! Then: Should reject non-flag arguments without equals sign
    function test_directory_argument_parsing() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: Directory argument parsing"
        
        ! Setup: Args with directory-like argument (no equals sign)
        allocate(character(len=15) :: args(1))
        args(1) = "src/"  ! This should be treated as unknown argument
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should fail with unknown argument error
        passed = .not. success .and. len_trim(error_message) > 0
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=F with error for non-flag argument"
            print *, "      Got success=", success, ", error='", trim(error_message), "'"
        else
            print *, "    PASSED: Correctly rejected non-flag argument"
        end if
    end function test_directory_argument_parsing

    ! Test 2.2: Executable path should not be treated as argument
    ! Given: Simulated executable path (what argv(0) would contain)
    ! When: This path is passed to parse_config (fixed implementation)
    ! Then: Should succeed by ignoring the executable path gracefully
    function test_executable_path_not_treated_as_argument() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: Executable path should be ignored gracefully"
        
        ! Setup: Test with executable path (should be ignored)
        allocate(character(len=70) :: args(1))
        args(1) = "/home/ert/code/fortcov/build/gfortran_8DB1C02C4AA3DC60/app/fortcov"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should succeed because executable paths should be ignored
        ! The fixed implementation should skip executable paths gracefully
        passed = success .and. len_trim(error_message) == 0
        
        if (.not. passed) then
            print *, "    FAILED: Executable path should be ignored gracefully"
            print *, "      Expected success=T with no error"
            print *, "      Got success=", success, ", error='", trim(error_message), "'"
        else
            print *, "    PASSED: Executable path correctly ignored"
        end if
    end function test_executable_path_not_treated_as_argument

    ! Test 3.1: Threshold parsing with equals sign
    ! Given: Command line argument --fail-under=80
    ! When: Parsing configuration
    ! Then: Should set minimum_coverage=80.0 and return success=true
    function test_threshold_parsing_equals() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: --fail-under=80 parsing"
        
        ! Setup: Args with threshold value
        allocate(character(len=20) :: args(1))
        args(1) = "--fail-under=80"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should parse threshold correctly
        passed = success .and. (abs(config%minimum_coverage - 80.0) < 0.001)
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=T, minimum_coverage=80.0"
            print *, "      Got success=", success, ", coverage=", config%minimum_coverage
        else
            print *, "    PASSED"
        end if
    end function test_threshold_parsing_equals

    ! Test 3.2: Alternative threshold parsing
    ! Given: Command line argument --threshold=90 (if supported)
    ! When: Parsing configuration
    ! Then: Should handle alternative threshold syntax
    function test_threshold_parsing_fail_under() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: Alternative threshold syntax"
        
        ! Setup: Args with alternative threshold syntax (should be rejected)
        allocate(character(len=20) :: args(1))
        args(1) = "--threshold=90"  ! This should be unknown option
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should fail as unsupported option
        passed = .not. success .and. index(error_message, "Unknown option") > 0
        
        if (.not. passed) then
            print *, "    FAILED: Expected rejection of --threshold syntax"
        else
            print *, "    PASSED: Correctly rejected unsupported syntax"
        end if
    end function test_threshold_parsing_fail_under

    ! Test 3.3: Invalid threshold values
    ! Given: Command line argument --fail-under=150
    ! When: Parsing configuration  
    ! Then: Should fail with range error
    function test_invalid_threshold_values() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: Invalid threshold values"
        
        ! Setup: Args with out-of-range threshold
        allocate(character(len=20) :: args(1))
        args(1) = "--fail-under=150"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should fail with range error
        passed = .not. success .and. len_trim(error_message) > 0
        
        if (.not. passed) then
            print *, "    FAILED: Expected failure for out-of-range threshold"
        else
            print *, "    PASSED: Correctly rejected invalid threshold"
        end if
    end function test_invalid_threshold_values

    ! Test 4.1: Invalid argument rejection
    ! Given: Command line argument --nonexistent-flag
    ! When: Parsing configuration
    ! Then: Should fail with unknown argument error (no equals sign)
    function test_invalid_argument_rejection() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: Invalid argument rejection"
        
        ! Setup: Args with invalid flag (no equals sign = "Unknown argument")
        allocate(character(len=25) :: args(1))
        args(1) = "--nonexistent-flag"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should fail with unknown argument error (no equals sign)
        passed = .not. success .and. index(error_message, "Unknown argument") > 0
        
        if (.not. passed) then
            print *, "    FAILED: Expected 'Unknown argument' error for invalid flag"
            print *, "      Got success=", success, ", error='", trim(error_message), "'"
        else
            print *, "    PASSED"
        end if
    end function test_invalid_argument_rejection

    ! Test 4.2: Unknown flag with equals sign rejection
    ! Given: Command line argument --unknown=value
    ! When: Parsing configuration
    ! Then: Should fail with unknown option error
    function test_unknown_flag_rejection() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: Unknown flag with value rejection"
        
        ! Setup: Args with unknown flag and value
        allocate(character(len=20) :: args(1))
        args(1) = "--unknown=value"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should fail with unknown option error
        passed = .not. success .and. index(error_message, "Unknown option") > 0
        
        if (.not. passed) then
            print *, "    FAILED: Expected 'Unknown option' error"
        else
            print *, "    PASSED"
        end if
    end function test_unknown_flag_rejection

    ! Test 5.1: No arguments execution (Issue #102 behavior)
    ! Given: Empty arguments array
    ! When: Parsing configuration
    ! Then: Should show help instead of using defaults (Issue #102 fix)
    function test_no_arguments_execution() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: No arguments execution"
        
        ! Setup: Empty args array
        allocate(character(len=10) :: args(0))
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should show help (Issue #102 fix)
        passed = (.not. success) .and. config%show_help
        
        if (.not. passed) then
            print *, "    FAILED: Expected show_help=T when no args (Issue #102)"
            print *, "      Got success=", success, ", show_help=", config%show_help
        else
            print *, "    PASSED"
        end if
    end function test_no_arguments_execution

    ! Test 5.2: Empty arguments execution (zero-length array) (Issue #102 behavior)
    ! Given: Zero-length arguments array
    ! When: Parsing configuration
    ! Then: Should show help instead of using defaults (Issue #102 fix)
    function test_empty_arguments_execution() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: Empty arguments array execution"
        
        ! Setup: Allocate zero-size args array
        allocate(character(len=1) :: args(0))
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should show help (Issue #102 fix)
        passed = (.not. success) .and. config%show_help
        
        if (.not. passed) then
            print *, "    FAILED: Expected show_help=T with empty args (Issue #102)"
            print *, "      Got success=", success, ", show_help=", config%show_help
        else
            print *, "    PASSED"
        end if
    end function test_empty_arguments_execution

    ! Test 6.1: Help exit behavior
    ! Given: --help flag
    ! When: Parsing configuration
    ! Then: Should return early exit status (success=false) but with show_help=true
    function test_help_exit_behavior() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: Help exit behavior"
        
        ! Setup: Args with help flag
        allocate(character(len=10) :: args(1))
        args(1) = "--help"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should signal early exit but not an error
        passed = .not. success .and. config%show_help .and. len_trim(error_message) == 0
        
        if (.not. passed) then
            print *, "    FAILED: Expected early exit for help without error"
        else
            print *, "    PASSED"
        end if
    end function test_help_exit_behavior

    ! Test 6.2: Version exit behavior
    ! Given: --version flag
    ! When: Parsing configuration
    ! Then: Should return early exit status (success=false) but with show_version=true
    function test_version_exit_behavior() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: Version exit behavior"
        
        ! Setup: Args with version flag
        allocate(character(len=15) :: args(1))
        args(1) = "--version"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should signal early exit but not an error
        passed = .not. success .and. config%show_version .and. len_trim(error_message) == 0
        
        if (.not. passed) then
            print *, "    FAILED: Expected early exit for version without error"
        else
            print *, "    PASSED"
        end if
    end function test_version_exit_behavior

    ! Test 7.1: Multiple valid arguments
    ! Given: Multiple valid command line arguments
    ! When: Parsing configuration
    ! Then: Should parse all correctly
    function test_multiple_valid_arguments() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: Multiple valid arguments"
        
        ! Setup: Args with multiple valid options
        allocate(character(len=30) :: args(3))
        args(1) = "--output=report.md"
        args(2) = "--verbose"
        args(3) = "--fail-under=85"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should parse all correctly
        passed = success .and. &
                 (config%output_path == "report.md") .and. &
                 config%verbose .and. &
                 (abs(config%minimum_coverage - 85.0) < 0.001)
        
        if (.not. passed) then
            print *, "    FAILED: Expected all arguments parsed correctly"
        else
            print *, "    PASSED"
        end if
    end function test_multiple_valid_arguments

    ! Test 7.2: Mixed valid and invalid arguments
    ! Given: Mix of valid and invalid arguments
    ! When: Parsing configuration
    ! Then: Should fail on first invalid argument
    function test_mixed_valid_invalid_arguments() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: Mixed valid/invalid arguments"
        
        ! Setup: Args with valid then invalid argument
        allocate(character(len=25) :: args(2))
        args(1) = "--verbose"        ! Valid
        args(2) = "--invalid-flag"   ! Invalid (no equals = "Unknown argument")
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should fail on invalid argument
        passed = .not. success .and. index(error_message, "Unknown argument") > 0
        
        if (.not. passed) then
            print *, "    FAILED: Expected failure on invalid argument"
            print *, "      Got success=", success, ", error='", trim(error_message), "'"
        else
            print *, "    PASSED"
        end if
    end function test_mixed_valid_invalid_arguments

end program test_cli_parsing