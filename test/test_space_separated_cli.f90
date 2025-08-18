! Test space-separated short CLI options (Issue #127)
!
! This test suite validates the critical fix for space-separated short options:
! - `-s src` should work (not just `--source=src`)
! - `-t 80` should work (not just `--fail-under=80`)
! - `-o file` should work (not just `--output=file`)
!
! Previously these documented CLI examples would fail with "Unknown argument" errors.
program test_space_separated_cli
    use fortcov_config
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing space-separated short CLI options (Issue #127)..."
    
    ! Test documented CLI examples from help text
    all_tests_passed = all_tests_passed .and. test_source_short_option()
    all_tests_passed = all_tests_passed .and. test_threshold_short_option()
    all_tests_passed = all_tests_passed .and. test_output_short_option()
    all_tests_passed = all_tests_passed .and. test_combined_short_options()
    all_tests_passed = all_tests_passed .and. test_mixed_syntax_compatibility()
    
    if (all_tests_passed) then
        print *, "All space-separated CLI tests PASSED"
        call exit(0)
    else
        print *, "Some space-separated CLI tests FAILED"
        call exit(1)
    end if

contains

    ! Test 1: -s src (space-separated source option)
    ! Given: Command line arguments -s src
    ! When: Parsing configuration
    ! Then: Should set source_paths(1) = "src"
    function test_source_short_option() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: -s src (space-separated source)"
        
        ! Setup: Space-separated short source option
        allocate(character(len=10) :: args(2))
        args(1) = "-s"
        args(2) = "src"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should parse source path correctly
        passed = success .and. allocated(config%source_paths) .and. &
                 size(config%source_paths) > 0 .and. &
                 trim(config%source_paths(1)) == "src"
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=T, source_paths(1)='src'"
            if (.not. success) print *, "      Got success=F, error: ", trim(error_message)
            if (.not. allocated(config%source_paths) .or. size(config%source_paths) == 0) then
                print *, "      Got source_paths not allocated or empty"
            else
                print *, "      Got source_paths(1)='", trim(config%source_paths(1)), "'"
            end if
        else
            print *, "    PASSED"
        end if
    end function test_source_short_option

    ! Test 2: -t 80 (space-separated threshold option)
    ! Given: Command line arguments -t 80
    ! When: Parsing configuration
    ! Then: Should set minimum_coverage = 80.0
    function test_threshold_short_option() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: -t 80 (space-separated threshold)"
        
        ! Setup: Space-separated short threshold option
        allocate(character(len=10) :: args(2))
        args(1) = "-t"
        args(2) = "80"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should parse threshold correctly
        passed = success .and. (abs(config%minimum_coverage - 80.0) < 0.001)
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=T, minimum_coverage=80.0"
            if (.not. success) then
                print *, "      Got success=F, error: ", trim(error_message)
            else
                print *, "      Got minimum_coverage=", config%minimum_coverage
            end if
        else
            print *, "    PASSED"
        end if
    end function test_threshold_short_option

    ! Test 3: -o report.md (space-separated output option)
    ! Given: Command line arguments -o report.md
    ! When: Parsing configuration
    ! Then: Should set output_path = "report.md"
    function test_output_short_option() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: -o report.md (space-separated output)"
        
        ! Setup: Space-separated short output option
        allocate(character(len=15) :: args(2))
        args(1) = "-o"
        args(2) = "report.md"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should parse output path correctly
        passed = success .and. trim(config%output_path) == "report.md"
        
        if (.not. passed) then
            print *, "    FAILED: Expected success=T, output_path='report.md'"
            if (.not. success) then
                print *, "      Got success=F, error: ", trim(error_message)
            else
                print *, "      Got output_path='", trim(config%output_path), "'"
            end if
        else
            print *, "    PASSED"
        end if
    end function test_output_short_option

    ! Test 4: Combined space-separated options
    ! Given: Command line arguments -s src -t 80 -o report.md
    ! When: Parsing configuration
    ! Then: Should parse all options correctly
    function test_combined_short_options() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: -s src -t 80 -o report.md (combined)"
        
        ! Setup: Multiple space-separated short options
        allocate(character(len=15) :: args(6))
        args(1) = "-s"
        args(2) = "src"
        args(3) = "-t"
        args(4) = "80"
        args(5) = "-o"
        args(6) = "report.md"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should parse all options correctly
        passed = success .and. &
                 allocated(config%source_paths) .and. size(config%source_paths) > 0 .and. &
                 trim(config%source_paths(1)) == "src" .and. &
                 (abs(config%minimum_coverage - 80.0) < 0.001) .and. &
                 trim(config%output_path) == "report.md"
        
        if (.not. passed) then
            print *, "    FAILED: Expected all options parsed correctly"
            if (.not. success) then
                print *, "      Parse failed with error: ", trim(error_message)
            else
                print *, "      Values:"
                if (allocated(config%source_paths) .and. size(config%source_paths) > 0) then
                    print *, "        source_paths(1) = '", trim(config%source_paths(1)), "'"
                else
                    print *, "        source_paths = not allocated or empty"
                end if
                print *, "        minimum_coverage = ", config%minimum_coverage
                print *, "        output_path = '", trim(config%output_path), "'"
            end if
        else
            print *, "    PASSED"
        end if
    end function test_combined_short_options

    ! Test 5: Mixed syntax compatibility (space-separated + equals)
    ! Given: Command line arguments -s src --fail-under=90 -o report.md
    ! When: Parsing configuration
    ! Then: Should parse mixed syntax correctly
    function test_mixed_syntax_compatibility() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        print *, "  Test: -s src --fail-under=90 -o report.md (mixed syntax)"
        
        ! Setup: Mix of space-separated and equals syntax
        allocate(character(len=20) :: args(5))
        args(1) = "-s"
        args(2) = "src"
        args(3) = "--fail-under=90"
        args(4) = "-o"
        args(5) = "report.md"
        
        ! Execute
        call parse_config(args, config, success, error_message)
        
        ! Verify: Should parse mixed syntax correctly
        passed = success .and. &
                 allocated(config%source_paths) .and. size(config%source_paths) > 0 .and. &
                 trim(config%source_paths(1)) == "src" .and. &
                 (abs(config%minimum_coverage - 90.0) < 0.001) .and. &
                 trim(config%output_path) == "report.md"
        
        if (.not. passed) then
            print *, "    FAILED: Expected mixed syntax parsed correctly"
            if (.not. success) then
                print *, "      Parse failed with error: ", trim(error_message)
            else
                print *, "      Values:"
                if (allocated(config%source_paths) .and. size(config%source_paths) > 0) then
                    print *, "        source_paths(1) = '", trim(config%source_paths(1)), "'"
                else
                    print *, "        source_paths = not allocated or empty"
                end if
                print *, "        minimum_coverage = ", config%minimum_coverage
                print *, "        output_path = '", trim(config%output_path), "'"
            end if
        else
            print *, "    PASSED"
        end if
    end function test_mixed_syntax_compatibility

end program test_space_separated_cli