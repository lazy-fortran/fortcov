program test_fortcov_config
    use fortcov_config
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing Configuration Module..."
    
    ! Test 1: Parse input format argument
    all_tests_passed = all_tests_passed .and. test_parse_input_format()
    
    ! Test 2: Parse output format argument
    all_tests_passed = all_tests_passed .and. test_parse_output_format()
    
    ! Test 3: Parse output file path
    all_tests_passed = all_tests_passed .and. test_parse_output_path()
    
    ! Test 4: Default to stdout
    all_tests_passed = all_tests_passed .and. test_default_output_stdout()
    
    ! Test 5: Parse source paths
    all_tests_passed = all_tests_passed .and. test_parse_source_paths()
    
    ! Test 6: Parse exclude patterns
    all_tests_passed = all_tests_passed .and. test_parse_exclude_patterns()
    
    ! Test 7: Parse minimum coverage threshold
    all_tests_passed = all_tests_passed .and. test_parse_coverage_threshold()
    
    ! Test 8: Handle invalid threshold
    all_tests_passed = all_tests_passed .and. test_invalid_threshold()
    
    ! Test 9: Parse verbose flag
    all_tests_passed = all_tests_passed .and. test_parse_verbose_flag()
    
    ! Test 10: Show help message
    all_tests_passed = all_tests_passed .and. test_show_help_message()
    
    ! Test 11: Handle unknown arguments
    all_tests_passed = all_tests_passed .and. test_unknown_arguments()
    
    ! Test 12: Load config from file
    all_tests_passed = all_tests_passed .and. test_load_config_file()
    
    if (all_tests_passed) then
        print *, "All tests PASSED"
        call exit(0)
    else
        print *, "Some tests FAILED"
        call exit(1)
    end if

contains

    function test_parse_input_format() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: args(:)
        character(len=256) :: error_message
        logical :: success
        
        print *, "  Test 1: Parse input format argument"
        
        allocate(character(len=20) :: args(1))
        args(1) = "--input-format=gcov"
        
        call parse_config(args, config, success, error_message)
        
        passed = success .and. (config%input_format == "gcov")
        
        if (.not. passed) then
            print *, "    FAILED: Expected input_format='gcov', success=T"
        else
            print *, "    PASSED"
        end if
    end function test_parse_input_format

    function test_parse_output_format() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: args(:)
        character(len=256) :: error_message
        logical :: success
        
        print *, "  Test 2: Parse output format argument"
        
        allocate(character(len=25) :: args(1))
        args(1) = "--output-format=markdown"
        
        call parse_config(args, config, success, error_message)
        
        passed = success .and. (config%output_format == "markdown")
        
        if (.not. passed) then
            print *, "    FAILED: Expected output_format='markdown', success=T"
        else
            print *, "    PASSED"
        end if
    end function test_parse_output_format

    function test_parse_output_path() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: args(:)
        character(len=256) :: error_message
        logical :: success
        
        print *, "  Test 3: Parse output file path"
        
        allocate(character(len=20) :: args(1))
        args(1) = "--output=coverage.md"
        
        call parse_config(args, config, success, error_message)
        
        passed = success .and. (config%output_path == "coverage.md")
        
        if (.not. passed) then
            print *, "    FAILED: Expected output_path='coverage.md', success=T"
        else
            print *, "    PASSED"
        end if
    end function test_parse_output_path

    function test_default_output_stdout() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: args(:)
        character(len=256) :: error_message
        logical :: success
        
        print *, "  Test 4: Default to stdout"
        
        allocate(character(len=10) :: args(0))
        
        call parse_config(args, config, success, error_message)
        
        passed = success .and. (config%output_path == "-")
        
        if (.not. passed) then
            print *, "    FAILED: Expected output_path='-', success=T"
        else
            print *, "    PASSED"
        end if
    end function test_default_output_stdout

    function test_parse_source_paths() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: args(:)
        character(len=256) :: error_message
        logical :: success
        
        print *, "  Test 5: Parse source paths"
        
        allocate(character(len=15) :: args(2))
        args(1) = "--source=src"
        args(2) = "--source=lib"
        
        call parse_config(args, config, success, error_message)
        
        passed = success .and. (size(config%source_paths) == 2) .and. &
                 (config%source_paths(1) == "src") .and. &
                 (config%source_paths(2) == "lib")
        
        if (.not. passed) then
            print *, "    FAILED: Expected source_paths=['src', 'lib'], success=T"
        else
            print *, "    PASSED"
        end if
    end function test_parse_source_paths

    function test_parse_exclude_patterns() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: args(:)
        character(len=256) :: error_message
        logical :: success
        
        print *, "  Test 6: Parse exclude patterns"
        
        allocate(character(len=20) :: args(2))
        args(1) = "--exclude=test/*"
        args(2) = "--exclude=*.mod"
        
        call parse_config(args, config, success, error_message)
        
        passed = success .and. (size(config%exclude_patterns) == 2) .and. &
                 (config%exclude_patterns(1) == "test/*") .and. &
                 (config%exclude_patterns(2) == "*.mod")
        
        if (.not. passed) then
            print *, "    FAILED: Expected exclude_patterns=['test/*', '*.mod']"
        else
            print *, "    PASSED"
        end if
    end function test_parse_exclude_patterns

    function test_parse_coverage_threshold() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: args(:)
        character(len=256) :: error_message
        logical :: success
        
        print *, "  Test 7: Parse minimum coverage threshold"
        
        allocate(character(len=20) :: args(1))
        args(1) = "--fail-under=80.0"
        
        call parse_config(args, config, success, error_message)
        
        passed = success .and. (abs(config%minimum_coverage - 80.0) < 0.01)
        
        if (.not. passed) then
            print *, "    FAILED: Expected minimum_coverage=80.0, success=T"
        else
            print *, "    PASSED"
        end if
    end function test_parse_coverage_threshold

    function test_invalid_threshold() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: args(:)
        character(len=256) :: error_message
        logical :: success
        
        print *, "  Test 8: Handle invalid threshold"
        
        allocate(character(len=18) :: args(1))
        args(1) = "--fail-under=150"
        
        call parse_config(args, config, success, error_message)
        
        passed = .not. success .and. (len_trim(error_message) > 0)
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=F with error message"
        else
            print *, "    PASSED"
        end if
    end function test_invalid_threshold

    function test_parse_verbose_flag() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: args(:)
        character(len=256) :: error_message
        logical :: success
        
        print *, "  Test 9: Parse verbose flag"
        
        allocate(character(len=10) :: args(1))
        args(1) = "--verbose"
        
        call parse_config(args, config, success, error_message)
        
        passed = success .and. config%verbose
        
        if (.not. passed) then
            print *, "    FAILED: Expected verbose=T, success=T"
        else
            print *, "    PASSED"
        end if
    end function test_parse_verbose_flag

    function test_show_help_message() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: args(:)
        character(len=256) :: error_message
        logical :: success
        
        print *, "  Test 10: Show help message"
        
        allocate(character(len=10) :: args(1))
        args(1) = "--help"
        
        call parse_config(args, config, success, error_message)
        
        passed = .not. success .and. config%show_help
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=F, show_help=T"
        else
            print *, "    PASSED"
        end if
    end function test_show_help_message

    function test_unknown_arguments() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: args(:)
        character(len=256) :: error_message
        logical :: success
        
        print *, "  Test 11: Handle unknown arguments"
        
        allocate(character(len=20) :: args(1))
        args(1) = "--unknown-option"
        
        call parse_config(args, config, success, error_message)
        
        passed = .not. success .and. (len_trim(error_message) > 0)
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=F with error message"
        else
            print *, "    PASSED"
        end if
    end function test_unknown_arguments

    function test_load_config_file() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: args(:)
        character(len=256) :: error_message
        logical :: success
        integer :: unit, iostat
        character(len=:), allocatable :: test_file
        
        print *, "  Test 12: Load config from file"
        
        ! Create a test namelist file
        test_file = "test_fortcov.nml"
        open(newunit=unit, file=test_file, status='replace', iostat=iostat)
        if (iostat == 0) then
            write(unit, '(A)') "&fortcov_config"
            write(unit, '(A)') "  input_format = 'lcov'"
            write(unit, '(A)') "  output_format = 'json'"
            write(unit, '(A)') "  output_path = 'test_output.json'"
            write(unit, '(A)') "  source_paths = 'src/', 'lib/', 'test/'"
            write(unit, '(A)') "  exclude_patterns = '*.mod', '*.o', 'build/*'"
            write(unit, '(A)') "  minimum_coverage = 85.5"
            write(unit, '(A)') "  verbose = .true."
            write(unit, '(A)') "/"
            close(unit)
        end if
        
        allocate(character(len=30) :: args(1))
        args(1) = "--config=" // test_file
        
        call parse_config(args, config, success, error_message)
        
        ! Clean up test file
        open(newunit=unit, file=test_file, status='old', iostat=iostat)
        if (iostat == 0) then
            close(unit, status='delete')
        end if
        
        ! Verify the configuration was loaded correctly
        passed = success .and. &
                 config%input_format == "lcov" .and. &
                 config%output_format == "json" .and. &
                 config%output_path == "test_output.json" .and. &
                 abs(config%minimum_coverage - 85.5) < 0.01 .and. &
                 config%verbose
        
        if (.not. passed) then
            if (.not. success) then
                print *, "    FAILED: " // trim(error_message)
            else
                print *, "    FAILED: Config values not loaded correctly"
                print *, "      input_format: ", config%input_format
                print *, "      output_format: ", config%output_format
                print *, "      output_path: ", config%output_path
                print *, "      minimum_coverage: ", config%minimum_coverage
                print *, "      verbose: ", config%verbose
            end if
        else
            print *, "    PASSED - Namelist config loaded successfully"
        end if
    end function test_load_config_file

end program test_fortcov_config