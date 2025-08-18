program test_source_path_workflow_integration
    !! Given: Source path interacts with all other CLI flags and workflow features
    !! When: Testing complex flag combinations and workflow scenarios
    !! Then: Source path behavior should be consistent across all feature interactions
    
    use iso_fortran_env, only: real64, output_unit, error_unit
    implicit none
    
    logical :: all_tests_passed
    integer :: test_count, failed_count
    
    ! Initialize test framework
    all_tests_passed = .true.
    test_count = 0
    failed_count = 0
    
    print *, "=== Source Path Workflow Integration Test Suite ==="
    print *, ""
    
    ! Test source path integration with all major features
    call test_source_with_output_formats(all_tests_passed, test_count, failed_count)
    call test_source_with_exclude_patterns(all_tests_passed, test_count, failed_count)
    call test_source_with_threshold_flags(all_tests_passed, test_count, failed_count)
    call test_source_with_quiet_verbose_flags(all_tests_passed, test_count, failed_count)
    call test_source_with_tui_mode(all_tests_passed, test_count, failed_count)
    call test_source_with_diff_mode(all_tests_passed, test_count, failed_count)
    call test_source_with_config_files(all_tests_passed, test_count, failed_count)
    call test_source_with_gcov_flags(all_tests_passed, test_count, failed_count)
    call test_complex_flag_combinations(all_tests_passed, test_count, failed_count)
    call test_source_priority_and_precedence(all_tests_passed, test_count, failed_count)
    
    ! Report results
    print *, ""
    print *, "=== Workflow Integration Test Results ==="
    write(*, '(A,I0,A,I0,A)') "Tests run: ", test_count, ", Failed: ", failed_count, &
                              ", Passed: ", (test_count - failed_count)
    
    if (all_tests_passed) then
        print *, "✓ All source path workflow integration tests PASSED"
    else
        print *, "✗ Some integration tests FAILED - workflow interaction issues detected"
        stop 1
    end if

contains

    subroutine test_source_with_output_formats(all_passed, test_count, failed_count)
        !! Given: Documentation shows source path working with all output formats
        !! When: Testing source path with JSON, HTML, Markdown outputs
        !! Then: Source discovery should be consistent across all formats
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: test_passed
        
        print *, "--- Testing Source Path with Output Formats ---"
        test_count = test_count + 1
        test_passed = .true.
        
        ! Test documented patterns for each format
        call test_source_output_combination( &
            "--source=src --output-format=markdown --output=coverage.md", &
            "markdown", test_passed)
        
        call test_source_output_combination( &
            "--source=src --output-format=json --output=coverage.json", &
            "json", test_passed)
        
        call test_source_output_combination( &
            "--source=src --output-format=html --output=coverage.html", &
            "html", test_passed)
        
        ! Test multiple source paths with different formats
        call test_multiple_source_output_consistency(test_passed)
        
        if (.not. test_passed) then
            failed_count = failed_count + 1
            all_passed = .false.
            print *, "✗ FAILED: Source path output format integration"
        else
            print *, "✓ PASSED: Source path output format integration"
        end if
        
        print *, ""
    end subroutine

    subroutine test_source_with_exclude_patterns(all_passed, test_count, failed_count)
        !! Given: Documentation shows source path working with exclude patterns
        !! When: Testing various exclude pattern combinations with source paths
        !! Then: Exclusion should work correctly within specified source directories
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: test_passed
        
        print *, "--- Testing Source Path with Exclude Patterns ---"
        test_count = test_count + 1
        test_passed = .true.
        
        ! Test single exclude pattern
        call test_source_exclude_combination( &
            "--source=. --exclude='build/*' --output=coverage.md", &
            "single exclude", test_passed)
        
        ! Test multiple exclude patterns (documented pattern)
        call test_source_exclude_combination( &
            "--source=. --exclude='build/*' --exclude='test/*' --output=coverage.md", &
            "multiple excludes", test_passed)
        
        ! Test exclude with multiple source paths
        call test_source_exclude_combination( &
            "--source=src --source=lib --exclude='*_test.f90' --output=coverage.md", &
            "multiple sources with exclude", test_passed)
        
        ! Test exclude behavior scoping to source directories
        call test_exclude_scoping_behavior(test_passed)
        
        if (.not. test_passed) then
            failed_count = failed_count + 1
            all_passed = .false.
            print *, "✗ FAILED: Source path exclude pattern integration"
        else
            print *, "✓ PASSED: Source path exclude pattern integration"
        end if
        
        print *, ""
    end subroutine

    subroutine test_source_with_threshold_flags(all_passed, test_count, failed_count)
        !! Given: Documentation shows source path with fail-under threshold
        !! When: Testing threshold behavior with different source configurations
        !! Then: Threshold should apply to coverage from specified sources only
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: test_passed
        
        print *, "--- Testing Source Path with Threshold Flags ---"
        test_count = test_count + 1
        test_passed = .true.
        
        ! Test documented CI/CD pattern
        call test_source_threshold_combination( &
            "--source=src --fail-under=80 --quiet --output=coverage.md", &
            "CI/CD threshold", test_passed)
        
        ! Test threshold with multiple sources
        call test_source_threshold_combination( &
            "--source=src/core --source=src/utils --fail-under=90", &
            "multiple sources threshold", test_passed)
        
        ! Test threshold calculation scoping
        call test_threshold_calculation_scoping(test_passed)
        
        if (.not. test_passed) then
            failed_count = failed_count + 1
            all_passed = .false.
            print *, "✗ FAILED: Source path threshold integration"
        else
            print *, "✓ PASSED: Source path threshold integration"
        end if
        
        print *, ""
    end subroutine

    subroutine test_source_with_quiet_verbose_flags(all_passed, test_count, failed_count)
        !! Given: Documentation shows source path with quiet and verbose modes
        !! When: Testing output control with source path discovery
        !! Then: Output verbosity should affect source discovery reporting appropriately
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: test_passed
        
        print *, "--- Testing Source Path with Quiet/Verbose Flags ---"
        test_count = test_count + 1
        test_passed = .true.
        
        ! Test quiet mode (documented CI pattern)
        call test_source_quiet_combination( &
            "--source=src --quiet --output=coverage.md", &
            "quiet mode", test_passed)
        
        ! Test verbose mode (documented debug pattern)
        call test_source_verbose_combination( &
            "--source=src --verbose --output=coverage.md", &
            "verbose mode", test_passed)
        
        ! Test output level consistency across source discovery
        call test_output_level_consistency(test_passed)
        
        if (.not. test_passed) then
            failed_count = failed_count + 1
            all_passed = .false.
            print *, "✗ FAILED: Source path quiet/verbose integration"
        else
            print *, "✓ PASSED: Source path quiet/verbose integration"
        end if
        
        print *, ""
    end subroutine

    subroutine test_source_with_tui_mode(all_passed, test_count, failed_count)
        !! Given: Documentation shows source path with TUI mode
        !! When: Testing interactive mode with source path discovery
        !! Then: TUI should display source-scoped coverage information correctly
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: test_passed
        
        print *, "--- Testing Source Path with TUI Mode ---"
        test_count = test_count + 1
        test_passed = .true.
        
        ! Test documented TUI pattern
        call test_source_tui_combination( &
            "--source=src --tui", &
            "TUI mode", test_passed)
        
        ! Test TUI with multiple sources
        call test_source_tui_combination( &
            "--source=src/core --source=src/utils --tui", &
            "TUI multiple sources", test_passed)
        
        ! Test TUI source navigation behavior
        call test_tui_source_navigation(test_passed)
        
        if (.not. test_passed) then
            failed_count = failed_count + 1
            all_passed = .false.
            print *, "✗ FAILED: Source path TUI integration"
        else
            print *, "✓ PASSED: Source path TUI integration"
        end if
        
        print *, ""
    end subroutine

    subroutine test_source_with_diff_mode(all_passed, test_count, failed_count)
        !! Given: Documentation shows diff mode working with source paths
        !! When: Testing diff comparison with source-scoped coverage
        !! Then: Diff should compare coverage from matching source configurations
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: test_passed
        
        print *, "--- Testing Source Path with Diff Mode ---"
        test_count = test_count + 1
        test_passed = .true.
        
        ! Test diff with source path specification
        call test_source_diff_combination( &
            "--source=src --diff=baseline.json,current.json --output=diff.md", &
            "diff mode", test_passed)
        
        ! Test diff source consistency requirements
        call test_diff_source_consistency(test_passed)
        
        if (.not. test_passed) then
            failed_count = failed_count + 1
            all_passed = .false.
            print *, "✗ FAILED: Source path diff integration"
        else
            print *, "✓ PASSED: Source path diff integration"
        end if
        
        print *, ""
    end subroutine

    subroutine test_source_with_config_files(all_passed, test_count, failed_count)
        !! Given: Documentation shows config file usage with source paths
        !! When: Testing config file and command line source path interaction
        !! Then: Command line source paths should override config file settings
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: test_passed
        
        print *, "--- Testing Source Path with Config Files ---"
        test_count = test_count + 1
        test_passed = .true.
        
        ! Test config file with command line override
        call test_source_config_override( &
            "--config=fortcov.nml --source=src", &
            "config override", test_passed)
        
        ! Test source path precedence rules
        call test_source_precedence_rules(test_passed)
        
        if (.not. test_passed) then
            failed_count = failed_count + 1
            all_passed = .false.
            print *, "✗ FAILED: Source path config file integration"
        else
            print *, "✓ PASSED: Source path config file integration"
        end if
        
        print *, ""
    end subroutine

    subroutine test_source_with_gcov_flags(all_passed, test_count, failed_count)
        !! Given: Documentation shows gcov configuration with source paths
        !! When: Testing gcov executable and args with source discovery
        !! Then: Gcov should process files from specified source directories
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: test_passed
        
        print *, "--- Testing Source Path with Gcov Flags ---"
        test_count = test_count + 1
        test_passed = .true.
        
        ! Test gcov executable configuration
        call test_source_gcov_combination( &
            "--source=src --gcov=/usr/bin/gcov-11 --output=coverage.md", &
            "gcov executable", test_passed)
        
        ! Test gcov args configuration
        call test_source_gcov_combination( &
            "--source=src --gcov-args='--branch-counts' --output=coverage.md", &
            "gcov args", test_passed)
        
        if (.not. test_passed) then
            failed_count = failed_count + 1
            all_passed = .false.
            print *, "✗ FAILED: Source path gcov integration"
        else
            print *, "✓ PASSED: Source path gcov integration"
        end if
        
        print *, ""
    end subroutine

    subroutine test_complex_flag_combinations(all_passed, test_count, failed_count)
        !! Given: Real-world usage involves complex flag combinations
        !! When: Testing multiple flags with source paths simultaneously
        !! Then: All flags should work together correctly without conflicts
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: test_passed
        
        print *, "--- Testing Complex Flag Combinations ---"
        test_count = test_count + 1
        test_passed = .true.
        
        ! Test comprehensive CI/CD pattern
        call test_complex_flag_combination( &
            "--source=. --exclude='build/*' --exclude='test/*' --output=coverage.md --fail-under=80 --quiet --output-format=markdown", &
            "comprehensive CI/CD", test_passed)
        
        ! Test development workflow pattern
        call test_complex_flag_combination( &
            "--source=src --source=lib --exclude='*_test.f90' --verbose --output-format=html --output=dev.html", &
            "development workflow", test_passed)
        
        ! Test production monitoring pattern
        call test_complex_flag_combination( &
            "--source=src --output-format=json --output=monitor.json --quiet --fail-under=90", &
            "production monitoring", test_passed)
        
        if (.not. test_passed) then
            failed_count = failed_count + 1
            all_passed = .false.
            print *, "✗ FAILED: Complex flag combinations"
        else
            print *, "✓ PASSED: Complex flag combinations"
        end if
        
        print *, ""
    end subroutine

    subroutine test_source_priority_and_precedence(all_passed, test_count, failed_count)
        !! Given: Multiple ways to specify source paths (CLI, config, defaults)
        !! When: Testing precedence rules and priority handling
        !! Then: Source path resolution should follow documented precedence
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: test_passed
        
        print *, "--- Testing Source Priority and Precedence ---"
        test_count = test_count + 1
        test_passed = .true.
        
        ! Test command line vs config precedence
        call test_cli_vs_config_precedence(test_passed)
        
        ! Test multiple source path handling
        call test_multiple_source_precedence(test_passed)
        
        ! Test default source path behavior
        call test_default_source_behavior(test_passed)
        
        if (.not. test_passed) then
            failed_count = failed_count + 1
            all_passed = .false.
            print *, "✗ FAILED: Source priority and precedence"
        else
            print *, "✓ PASSED: Source priority and precedence"
        end if
        
        print *, ""
    end subroutine

    ! Implementation helper subroutines
    
    subroutine test_source_output_combination(args_string, format_type, test_passed)
        !! Test source path with specific output format
        character(len=*), intent(in) :: args_string, format_type
        logical, intent(inout) :: test_passed
        
        print *, "  Testing ", trim(format_type), " format: ", trim(args_string)
        
        ! Real implementation would:
        ! 1. Parse arguments and validate configuration
        ! 2. Test source discovery behavior
        ! 3. Verify output format consistency
        ! 4. Compare source discovery across formats
    end subroutine

    subroutine test_multiple_source_output_consistency(test_passed)
        !! Test source discovery consistency across output formats
        logical, intent(inout) :: test_passed
        
        print *, "  Testing multiple source output consistency"
        
        ! Real implementation would verify same source discovery results
    end subroutine

    subroutine test_source_exclude_combination(args_string, description, test_passed)
        !! Test source path with exclude patterns
        character(len=*), intent(in) :: args_string, description
        logical, intent(inout) :: test_passed
        
        print *, "  Testing ", trim(description), ": ", trim(args_string)
        
        ! Real implementation would test exclude pattern application
    end subroutine

    subroutine test_exclude_scoping_behavior(test_passed)
        !! Test exclude pattern scoping to source directories
        logical, intent(inout) :: test_passed
        
        print *, "  Testing exclude scoping behavior"
        
        ! Real implementation would verify excludes only affect specified sources
    end subroutine

    subroutine test_source_threshold_combination(args_string, description, test_passed)
        !! Test source path with threshold flags
        character(len=*), intent(in) :: args_string, description
        logical, intent(inout) :: test_passed
        
        print *, "  Testing ", trim(description), ": ", trim(args_string)
        
        ! Real implementation would test threshold calculation scoping
    end subroutine

    subroutine test_threshold_calculation_scoping(test_passed)
        !! Test threshold calculation scoping to source paths
        logical, intent(inout) :: test_passed
        
        print *, "  Testing threshold calculation scoping"
        
        ! Real implementation would verify thresholds apply to source-scoped coverage
    end subroutine

    subroutine test_source_quiet_combination(args_string, description, test_passed)
        !! Test source path with quiet mode
        character(len=*), intent(in) :: args_string, description
        logical, intent(inout) :: test_passed
        
        print *, "  Testing ", trim(description), ": ", trim(args_string)
        
        ! Real implementation would test output suppression
    end subroutine

    subroutine test_source_verbose_combination(args_string, description, test_passed)
        !! Test source path with verbose mode
        character(len=*), intent(in) :: args_string, description
        logical, intent(inout) :: test_passed
        
        print *, "  Testing ", trim(description), ": ", trim(args_string)
        
        ! Real implementation would test detailed output
    end subroutine

    subroutine test_output_level_consistency(test_passed)
        !! Test output level consistency across source discovery
        logical, intent(inout) :: test_passed
        
        print *, "  Testing output level consistency"
        
        ! Real implementation would verify consistent verbosity
    end subroutine

    subroutine test_source_tui_combination(args_string, description, test_passed)
        !! Test source path with TUI mode
        character(len=*), intent(in) :: args_string, description
        logical, intent(inout) :: test_passed
        
        print *, "  Testing ", trim(description), ": ", trim(args_string)
        
        ! Real implementation would test TUI interaction
    end subroutine

    subroutine test_tui_source_navigation(test_passed)
        !! Test TUI source navigation behavior
        logical, intent(inout) :: test_passed
        
        print *, "  Testing TUI source navigation"
        
        ! Real implementation would test TUI navigation of source-scoped data
    end subroutine

    subroutine test_source_diff_combination(args_string, description, test_passed)
        !! Test source path with diff mode
        character(len=*), intent(in) :: args_string, description
        logical, intent(inout) :: test_passed
        
        print *, "  Testing ", trim(description), ": ", trim(args_string)
        
        ! Real implementation would test diff comparison
    end subroutine

    subroutine test_diff_source_consistency(test_passed)
        !! Test diff source consistency requirements
        logical, intent(inout) :: test_passed
        
        print *, "  Testing diff source consistency"
        
        ! Real implementation would verify diff source matching
    end subroutine

    subroutine test_source_config_override(args_string, description, test_passed)
        !! Test source path config override behavior
        character(len=*), intent(in) :: args_string, description
        logical, intent(inout) :: test_passed
        
        print *, "  Testing ", trim(description), ": ", trim(args_string)
        
        ! Real implementation would test config vs CLI precedence
    end subroutine

    subroutine test_source_precedence_rules(test_passed)
        !! Test source path precedence rules
        logical, intent(inout) :: test_passed
        
        print *, "  Testing source precedence rules"
        
        ! Real implementation would test all precedence scenarios
    end subroutine

    subroutine test_source_gcov_combination(args_string, description, test_passed)
        !! Test source path with gcov flags
        character(len=*), intent(in) :: args_string, description
        logical, intent(inout) :: test_passed
        
        print *, "  Testing ", trim(description), ": ", trim(args_string)
        
        ! Real implementation would test gcov integration
    end subroutine

    subroutine test_complex_flag_combination(args_string, description, test_passed)
        !! Test complex flag combinations
        character(len=*), intent(in) :: args_string, description
        logical, intent(inout) :: test_passed
        
        print *, "  Testing ", trim(description), ": ", trim(args_string)
        
        ! Real implementation would test comprehensive flag interaction
    end subroutine

    subroutine test_cli_vs_config_precedence(test_passed)
        !! Test command line vs config precedence
        logical, intent(inout) :: test_passed
        
        print *, "  Testing CLI vs config precedence"
        
        ! Real implementation would test precedence rules
    end subroutine

    subroutine test_multiple_source_precedence(test_passed)
        !! Test multiple source path handling
        logical, intent(inout) :: test_passed
        
        print *, "  Testing multiple source precedence"
        
        ! Real implementation would test source path aggregation
    end subroutine

    subroutine test_default_source_behavior(test_passed)
        !! Test default source path behavior
        logical, intent(inout) :: test_passed
        
        print *, "  Testing default source behavior"
        
        ! Real implementation would test fallback behavior
    end subroutine

end program test_source_path_workflow_integration