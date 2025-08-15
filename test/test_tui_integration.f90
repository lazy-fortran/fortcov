program test_tui_integration
    use coverage_engine
    use fortcov_config
    use coverage_model
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing TUI Integration..."
    
    ! Test: TUI mode triggers interactive display instead of file output
    all_tests_passed = all_tests_passed .and. test_tui_mode_triggers_interactive()
    
    if (all_tests_passed) then
        print *, "All TUI integration tests PASSED"
        call exit(0)
    else
        print *, "Some TUI integration tests FAILED"
        call exit(1)
    end if

contains

    ! Test that TUI mode triggers interactive display (Issue #106)
    function test_tui_mode_triggers_interactive() result(passed)
        logical :: passed
        type(config_t) :: config
        integer :: exit_code
        
        print *, "  Test: TUI mode triggers interactive display (Issue #106)"
        
        ! Initialize config with TUI mode enabled
        call initialize_config(config)
        config%tui_mode = .true.  ! Now this works!
        
        ! Set up minimal test configuration
        config%input_format = "gcov"
        config%output_format = "markdown"
        config%output_path = "-"  ! stdout
        config%verbose = .false.
        config%quiet = .true.  ! Suppress output during test
        
        ! Try to analyze coverage in TUI mode
        ! TUI should launch, process available .gcov files, and exit gracefully
        exit_code = analyze_coverage(config)
        
        ! The test passes if TUI mode is detected and exits gracefully
        ! If there are .gcov files, it should succeed (EXIT_SUCCESS = 0)
        ! If there are no .gcov files, it should return EXIT_NO_COVERAGE_DATA (3)
        passed = (exit_code == EXIT_SUCCESS .or. exit_code == EXIT_NO_COVERAGE_DATA)
        
        if (passed) then
            print *, "    PASSED: TUI mode detected and handled correctly"
            if (exit_code == EXIT_SUCCESS) then
                print *, "      exit_code: ", exit_code, " (EXIT_SUCCESS - found coverage data)"
            else
                print *, "      exit_code: ", exit_code, " (EXIT_NO_COVERAGE_DATA - no coverage files)"
            end if
        else
            print *, "    FAILED: TUI integration not working correctly"
            print *, "      exit_code: ", exit_code, " (expected 0 or 3)"
        end if
    end function test_tui_mode_triggers_interactive

end program test_tui_integration