program test_cli
    use fortcov_config
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing CLI Application..."
    
    ! Test 1: Basic execution with gcov files
    all_tests_passed = all_tests_passed .and. test_basic_execution_gcov_files()
    
    ! Test 2: Specify output file
    all_tests_passed = all_tests_passed .and. test_specify_output_file()
    
    ! Test 3: Help message
    all_tests_passed = all_tests_passed .and. test_help_message()
    
    ! Test 4: Version information
    all_tests_passed = all_tests_passed .and. test_version_information()
    
    ! Test 5: Invalid arguments
    all_tests_passed = all_tests_passed .and. test_invalid_arguments()
    
    ! Test 6: No coverage files found
    all_tests_passed = all_tests_passed .and. test_no_coverage_files_found()
    
    ! Test 7: Coverage below threshold
    all_tests_passed = all_tests_passed .and. test_coverage_below_threshold()
    
    ! Test 8: Successful threshold check
    all_tests_passed = all_tests_passed .and. test_successful_threshold_check()
    
    ! Test 9: Multiple source directories
    all_tests_passed = all_tests_passed .and. test_multiple_source_directories()
    
    ! Test 10: Verbose output
    all_tests_passed = all_tests_passed .and. test_verbose_output()
    
    if (all_tests_passed) then
        print *, "All tests PASSED"
        call exit(0)
    else
        print *, "Some tests FAILED"
        call exit(1)
    end if

contains

    ! Test 1: Basic execution with gcov files
    ! Given: Directory with .gcda/.gcno files
    ! When: Running fortcov
    ! Then: Should generate markdown report to stdout
    function test_basic_execution_gcov_files() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test 1: Basic execution with gcov files"
        
        ! Setup: Empty args array (default behavior)
        allocate(character(len=10) :: args(0))
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should succeed with default configuration
        passed = success .and. &
                 (config%input_format == "gcov") .and. &
                 (config%output_format == "markdown") .and. &
                 (config%output_path == "-")
        
        if (.not. passed) then
            print *, "    FAILED: Expected defaults gcov/markdown/stdout"
        else
            print *, "    PASSED"
        end if
    end function test_basic_execution_gcov_files

    ! Test 2: Specify output file
    ! Given: Command fortcov --output=report.md
    ! When: Executing
    ! Then: Should write report to report.md file
    function test_specify_output_file() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test 2: Specify output file"
        
        ! Setup: Args with output parameter
        allocate(character(len=20) :: args(1))
        args(1) = "--output=report.md"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should set output path correctly
        passed = success .and. (config%output_path == "report.md")
        
        if (.not. passed) then
            print *, "    FAILED: Expected output_path='report.md', success=T"
        else
            print *, "    PASSED"
        end if
    end function test_specify_output_file

    ! Test 3: Help message
    ! Given: Command fortcov --help
    ! When: Executing
    ! Then: Should display usage and exit with code 0
    function test_help_message() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test 3: Help message"
        
        ! Setup: Args with help flag
        allocate(character(len=10) :: args(1))
        args(1) = "--help"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should trigger help display
        passed = .not. success .and. config%show_help
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=F, show_help=T"
        else
            print *, "    PASSED"
        end if
    end function test_help_message

    ! Test 4: Version information
    ! Given: Command fortcov --version
    ! When: Executing
    ! Then: Should display version and exit with code 0
    function test_version_information() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test 4: Version information"
        
        ! Setup: Args with version flag
        allocate(character(len=10) :: args(1))
        args(1) = "--version"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should trigger version display (this test will fail until 
        ! we implement version support)
        passed = .not. success .and. config%show_version
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=F, show_version=T"
        else
            print *, "    PASSED"
        end if
    end function test_version_information

    ! Test 5: Invalid arguments
    ! Given: Command fortcov --invalid-option
    ! When: Executing
    ! Then: Should display error and exit with code 1
    function test_invalid_arguments() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test 5: Invalid arguments"
        
        ! Setup: Args with invalid option
        allocate(character(len=20) :: args(1))
        args(1) = "--invalid-option"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should fail with error message
        passed = .not. success .and. (len_trim(error_message) > 0)
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=F with error message"
        else
            print *, "    PASSED"
        end if
    end function test_invalid_arguments

    ! Test 6: No coverage files found
    ! Given: Directory with no coverage files
    ! When: Running fortcov
    ! Then: Should display warning and generate empty report
    function test_no_coverage_files_found() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test 6: No coverage files found"
        
        ! Setup: Default configuration (this test verifies behavior at 
        ! engine level, config parsing should succeed)
        allocate(character(len=10) :: args(0))
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Config parsing should succeed
        passed = success
        
        if (.not. passed) then
            print *, "    FAILED: Config parsing should succeed"
        else
            print *, "    PASSED - Config parsing succeeds"
        end if
    end function test_no_coverage_files_found

    ! Test 7: Coverage below threshold
    ! Given: Command fortcov --fail-under=90 with 80% coverage
    ! When: Executing
    ! Then: Should exit with code 2
    function test_coverage_below_threshold() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test 7: Coverage below threshold"
        
        ! Setup: Args with fail-under threshold
        allocate(character(len=20) :: args(1))
        args(1) = "--fail-under=90"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should parse threshold correctly
        passed = success .and. (abs(config%minimum_coverage - 90.0) < 0.01)
        
        if (.not. passed) then
            print *, "    FAILED: Expected minimum_coverage=90.0, success=T"
        else
            print *, "    PASSED"
        end if
    end function test_coverage_below_threshold

    ! Test 8: Successful threshold check
    ! Given: Command fortcov --fail-under=70 with 80% coverage
    ! When: Executing
    ! Then: Should exit with code 0
    function test_successful_threshold_check() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test 8: Successful threshold check"
        
        ! Setup: Args with fail-under threshold
        allocate(character(len=20) :: args(1))
        args(1) = "--fail-under=70"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should parse threshold correctly
        passed = success .and. (abs(config%minimum_coverage - 70.0) < 0.01)
        
        if (.not. passed) then
            print *, "    FAILED: Expected minimum_coverage=70.0, success=T"
        else
            print *, "    PASSED"
        end if
    end function test_successful_threshold_check

    ! Test 9: Multiple source directories
    ! Given: Command fortcov --source=src --source=lib
    ! When: Executing
    ! Then: Should process files from both directories
    function test_multiple_source_directories() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test 9: Multiple source directories"
        
        ! Setup: Args with multiple source directories
        allocate(character(len=15) :: args(2))
        args(1) = "--source=src"
        args(2) = "--source=lib"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should parse both source directories
        passed = success .and. (size(config%source_paths) == 2) .and. &
                 (config%source_paths(1) == "src") .and. &
                 (config%source_paths(2) == "lib")
        
        if (.not. passed) then
            print *, "    FAILED: Expected source_paths=['src', 'lib'], success=T"
        else
            print *, "    PASSED"
        end if
    end function test_multiple_source_directories

    ! Test 10: Verbose output
    ! Given: Command fortcov --verbose
    ! When: Executing
    ! Then: Should display detailed processing information
    function test_verbose_output() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test 10: Verbose output"
        
        ! Setup: Args with verbose flag
        allocate(character(len=10) :: args(1))
        args(1) = "--verbose"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should set verbose flag
        passed = success .and. config%verbose
        
        if (.not. passed) then
            print *, "    FAILED: Expected verbose=T, success=T"
        else
            print *, "    PASSED"
        end if
    end function test_verbose_output

end program test_cli