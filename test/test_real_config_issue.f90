! Test for Issue #105: Configuration file parsing fails with provided example
!
! This test reproduces the exact issue reported: using the real example
! configuration file with the CLI parsing flow

program test_real_config_issue
    use fortcov_config
    implicit none
    
    logical :: all_tests_passed
    
    print *, "Testing real config issue (Issue #105)..."
    
    all_tests_passed = .true.
    
    ! Test 1: Use the actual example file with CLI argument parsing
    all_tests_passed = all_tests_passed .and. test_real_example_file()
    
    ! Test 2: Test the exact multi-line format from the example
    all_tests_passed = all_tests_passed .and. test_multiline_format()
    
    if (all_tests_passed) then
        print *, "All real config issue tests PASSED"
        call exit(0)
    else
        print *, "Some real config issue tests FAILED"
        call exit(1)
    end if

contains

    ! Test using the real example configuration file with CLI parsing
    function test_real_example_file() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test 1: Use real example file with CLI parsing"
        
        ! Create args like real CLI usage: fortcov --config=fortcov.nml.example
        allocate(character(len=256) :: args(1))
        args(1) = "--config=fortcov.nml.example"
        
        ! Parse config using real CLI flow
        call parse_config(args, config, success, error_message)
        
        if (success) then
            ! Verify the configuration was loaded correctly
            passed = (trim(config%input_format) == "gcov") .and. &
                     (trim(config%output_format) == "markdown") .and. &
                     (trim(config%output_path) == "coverage.md") .and. &
                     (trim(config%gcov_executable) == "gcov") .and. &
                     (abs(config%minimum_coverage - 80.0) < 0.01)
            
            if (passed) then
                print *, "    PASSED: Real example file loaded via CLI"
            else
                print *, "    FAILED: Configuration values incorrect"
                print *, "      input_format: '", trim(config%input_format), "'"
                print *, "      output_format: '", trim(config%output_format), "'"
                print *, "      output_path: '", trim(config%output_path), "'"
                print *, "      minimum_coverage: ", config%minimum_coverage
            end if
        else
            print *, "    FAILED: CLI parsing failed with real example"
            print *, "      Error: ", trim(error_message)
            passed = .false.
        end if
    end function test_real_example_file
    
    ! Test the exact multi-line array format from the example
    function test_multiline_format() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=256) :: test_config_file
        integer :: unit
        
        print *, "  Test 2: Test exact multi-line format"
        
        ! Create test file with exact multi-line format from example
        test_config_file = "test_multiline_exact.nml"
        open(newunit=unit, file=test_config_file, status='replace')
        write(unit, '(A)') "&fortcov_config"
        write(unit, '(A)') "    input_format = 'gcov'"
        write(unit, '(A)') "    output_format = 'markdown'"
        write(unit, '(A)') "    output_path = 'coverage.md'"
        write(unit, '(A)') "    source_paths = 'src/', "
        write(unit, '(A)') "                   'lib/', "
        write(unit, '(A)') "                   'app/'"
        write(unit, '(A)') "    exclude_patterns = '*.mod',"
        write(unit, '(A)') "                       '*.o',"
        write(unit, '(A)') "                       'build/*',"
        write(unit, '(A)') "                       'test/*',"
        write(unit, '(A)') "                       'external/*'"
        write(unit, '(A)') "    gcov_executable = 'gcov'"
        write(unit, '(A)') "    minimum_coverage = 80.0"
        write(unit, '(A)') "    verbose = .false."
        write(unit, '(A)') "    quiet = .false."
        write(unit, '(A)') "/"
        close(unit)
        
        ! Initialize config and set config file
        call initialize_config(config)
        config%config_file = test_config_file
        
        ! Try to load the config file directly
        call load_config_file(config, success, error_message)
        
        if (success) then
            ! Verify arrays were parsed correctly
            if (allocated(config%source_paths) .and. size(config%source_paths) == 3) then
                passed = (trim(config%source_paths(1)) == "src/") .and. &
                         (trim(config%source_paths(2)) == "lib/") .and. &
                         (trim(config%source_paths(3)) == "app/")
                
                if (passed) then
                    print *, "    PASSED: Multi-line arrays parsed correctly"
                else
                    print *, "    FAILED: Multi-line arrays not parsed correctly"
                    do unit = 1, size(config%source_paths)
                        print *, "      source_paths(", unit, "): '", &
                               trim(config%source_paths(unit)), "'"
                    end do
                end if
            else
                print *, "    FAILED: Source paths array incorrect size"
                if (allocated(config%source_paths)) then
                    print *, "      Size: ", size(config%source_paths)
                else
                    print *, "      Not allocated"
                end if
                passed = .false.
            end if
        else
            print *, "    FAILED: Multi-line format parsing failed"
            print *, "      Error: ", trim(error_message)
            passed = .false.
        end if
        
        ! Clean up
        open(newunit=unit, file=test_config_file)
        close(unit, status='delete')
    end function test_multiline_format

end program test_real_config_issue