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
    
    ! Test 13: Parse single positional argument (NEW - RED PHASE)
    all_tests_passed = all_tests_passed .and. test_parse_single_positional()
    
    ! Test 14: Parse multiple positional arguments (NEW - RED PHASE)
    all_tests_passed = all_tests_passed .and. test_parse_multiple_positional()
    
    ! Test 15: Parse mixed positional and flag arguments (NEW - RED PHASE)
    all_tests_passed = all_tests_passed .and. test_parse_mixed_positional_flags()
    
    ! Test 16: Validate coverage file extensions (NEW - RED PHASE)
    all_tests_passed = all_tests_passed .and. test_validate_coverage_extensions()
    
    ! Test 17: Parse import flag for JSON import (NEW - RED PHASE)
    all_tests_passed = all_tests_passed .and. test_parse_import_flag()
    
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
        
        passed = success .and. (abs(config%minimum_coverage - 80.0) < 0.001)
        
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
                 abs(config%minimum_coverage - 85.5) < 0.001 .and. &
                 config%verbose
        
        if (.not. passed) then
            if (.not. success) then
                print *, "    FAILED: " // trim(error_message)
            else
                print *, "    FAILED: Config values not loaded correctly"
                if (allocated(config%input_format)) then
                    print *, "      input_format: ", config%input_format
                else
                    print *, "      input_format: <not allocated>"
                end if
                if (allocated(config%output_format)) then
                    print *, "      output_format: ", config%output_format
                else
                    print *, "      output_format: <not allocated>"
                end if
                if (allocated(config%output_path)) then
                    print *, "      output_path: ", config%output_path
                else
                    print *, "      output_path: <not allocated>"
                end if
                print *, "      minimum_coverage: ", config%minimum_coverage
                print *, "      verbose: ", config%verbose
            end if
        else
            print *, "    PASSED - Namelist config loaded successfully"
        end if
    end function test_load_config_file

    ! RED PHASE: These tests will FAIL until implementation is complete
    function test_parse_single_positional() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: args(:)
        character(len=256) :: error_message
        logical :: success
        
        print *, "  Test 13: Parse single positional argument"
        
        allocate(character(len=20) :: args(1))
        args(1) = "test_file.gcov"
        
        call parse_config(args, config, success, error_message)
        
        ! Expected behavior after implementation:
        ! - success = .true.
        ! - config%coverage_files should contain ["test_file.gcov"]
        passed = success .and. &
                 allocated(config%coverage_files) .and. &
                 size(config%coverage_files) == 1 .and. &
                 config%coverage_files(1) == "test_file.gcov"
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=T, coverage_files=['test_file.gcov']"
            if (.not. success) then
                print *, "      Error: ", trim(error_message)
            end if
        else
            print *, "    PASSED"
        end if
    end function test_parse_single_positional

    function test_parse_multiple_positional() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: args(:)
        character(len=256) :: error_message
        logical :: success
        
        print *, "  Test 14: Parse multiple positional arguments"
        
        allocate(character(len=20) :: args(3))
        args(1) = "file1.gcov"
        args(2) = "file2.gcov"
        args(3) = "module.gcov"
        
        call parse_config(args, config, success, error_message)
        
        ! Expected behavior after implementation:
        ! - success = .true.
        ! - config%coverage_files should contain all three files
        passed = success .and. &
                 allocated(config%coverage_files) .and. &
                 size(config%coverage_files) == 3 .and. &
                 config%coverage_files(1) == "file1.gcov" .and. &
                 config%coverage_files(2) == "file2.gcov" .and. &
                 config%coverage_files(3) == "module.gcov"
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=T with 3 coverage files"
            if (.not. success) then
                print *, "      Error: ", trim(error_message)
            end if
        else
            print *, "    PASSED"
        end if
    end function test_parse_multiple_positional

    function test_parse_mixed_positional_flags() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: args(:)
        character(len=256) :: error_message
        logical :: success
        
        print *, "  Test 15: Parse mixed positional and flag arguments"
        
        allocate(character(len=25) :: args(4))
        args(1) = "coverage.gcov"
        args(2) = "module.gcov"
        args(3) = "--output-format=json"
        args(4) = "--verbose"
        
        call parse_config(args, config, success, error_message)
        
        ! Expected behavior after implementation:
        ! - success = .true.
        ! - coverage_files should contain the .gcov files
        ! - flags should be processed correctly
        passed = success .and. &
                 allocated(config%coverage_files) .and. &
                 size(config%coverage_files) == 2 .and. &
                 config%coverage_files(1) == "coverage.gcov" .and. &
                 config%coverage_files(2) == "module.gcov" .and. &
                 config%output_format == "json" .and. &
                 config%verbose
        
        if (.not. passed) then
            print *, "    FAILED: Mixed args not parsed correctly"
            if (.not. success) then
                print *, "      Error: ", trim(error_message)
            end if
        else
            print *, "    PASSED"
        end if
    end function test_parse_mixed_positional_flags

    function test_validate_coverage_extensions() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: args(:)
        character(len=256) :: error_message
        logical :: success
        
        print *, "  Test 16: Validate coverage file extensions"
        
        allocate(character(len=20) :: args(2))
        args(1) = "valid.gcov"
        args(2) = "invalid.txt"
        
        call parse_config(args, config, success, error_message)
        
        ! Expected behavior after implementation:
        ! - success = .false. due to invalid extension
        ! - error_message should mention invalid file extension
        passed = (.not. success) .and. (len_trim(error_message) > 0)
        
        if (.not. passed) then
            print *, "    FAILED: Expected validation error for invalid extension"
        else
            print *, "    PASSED"
        end if
    end function test_validate_coverage_extensions

    function test_parse_import_flag() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=:), allocatable :: args(:)
        character(len=256) :: error_message
        logical :: success
        
        print *, "  Test 17: Parse import flag for JSON import"
        
        allocate(character(len=30) :: args(1))
        args(1) = "--import=coverage.json"
        
        call parse_config(args, config, success, error_message)
        
        ! Expected behavior after implementation:
        ! - success = .true.
        ! - config%import_file = "coverage.json"
        ! - config%input_format = "json" (should be set automatically)
        passed = success .and. &
                (config%import_file == "coverage.json") .and. &
                (config%input_format == "json")
        
        if (.not. passed) then
            if (.not. success) then
                print *, "    FAILED: Config parsing failed: ", trim(error_message)
            else if (config%import_file /= "coverage.json") then
                print *, "    FAILED: Import file not set correctly: ", config%import_file
            else if (config%input_format /= "json") then
                print *, "    FAILED: Input format not set to json: ", config%input_format
            else
                print *, "    FAILED: Unknown reason"
            end if
        else
            print *, "    PASSED"
        end if
    end function test_parse_import_flag

end program test_fortcov_config