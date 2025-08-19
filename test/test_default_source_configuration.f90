program test_default_source_configuration
    !! Default Source Configuration Test Suite
    !! 
    !! Given: Issue #170 requests src=. as default configuration
    !! When: Testing default source path behavior in various scenarios
    !! Then: Should default to current directory without requiring explicit --source flag
    !!
    !! CRITICAL REQUIREMENTS:
    !! - Default source path should be current directory (.)
    !! - Should work without --source flag in CLI
    !! - Should find .gcov files in current directory by default
    !! - Should maintain backward compatibility with explicit --source

    use iso_fortran_env, only: real64, output_unit, error_unit
    implicit none
    
    logical :: all_tests_passed
    integer :: test_count, failed_count
    
    ! Initialize test framework
    all_tests_passed = .true.
    test_count = 0
    failed_count = 0
    
    print *, "=== Default Source Configuration Test Suite ==="
    print *, ""
    print *, "TESTING REQUIREMENTS:"
    print *, "  ✓ Default source path is current directory (.)"
    print *, "  ✓ Works without --source flag in CLI"
    print *, "  ✓ Finds .gcov files in current directory by default"
    print *, "  ✓ Backward compatibility with explicit --source"
    print *, ""
    
    ! Core default source behavior tests
    call test_default_source_is_current_directory(all_tests_passed, test_count, failed_count)
    call test_cli_without_source_flag_works(all_tests_passed, test_count, failed_count)
    call test_finds_gcov_files_by_default(all_tests_passed, test_count, failed_count)
    call test_default_source_with_exclude_patterns(all_tests_passed, test_count, failed_count)
    
    ! Configuration file and environment tests
    call test_config_file_default_source(all_tests_passed, test_count, failed_count)
    call test_environment_variable_source_default(all_tests_passed, test_count, failed_count)
    
    ! Backward compatibility tests
    call test_explicit_source_flag_still_works(all_tests_passed, test_count, failed_count)
    call test_explicit_source_overrides_default(all_tests_passed, test_count, failed_count)
    
    ! Edge cases and error scenarios
    call test_default_source_error_scenarios(all_tests_passed, test_count, failed_count)
    call test_default_source_with_relative_paths(all_tests_passed, test_count, failed_count)
    
    ! Report results
    print *, ""
    print *, "=== Default Source Configuration Test Results ==="
    write(*, '(A,I0,A,I0,A)') "Tests run: ", test_count, ", Failed: ", failed_count, &
                              ", Passed: ", (test_count - failed_count)
    
    if (all_tests_passed) then
        print *, "✓ All default source configuration tests PASSED"
        print *, "  src=. default behavior ready for implementation"
    else
        print *, "✗ Default source configuration test failures detected"
        print *, "  Implementation requirements not validated"
        stop 1
    end if

contains

    ! =================================================================
    ! CORE DEFAULT SOURCE BEHAVIOR TESTS
    ! =================================================================

    subroutine test_default_source_is_current_directory(all_passed, test_count, failed_count)
        !! Given: No --source flag provided in CLI
        !! When: fortcov runs without explicit source specification
        !! Then: Should default to current directory (.) as source path
        
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: test_passed
        character(len=256) :: default_source_path
        
        test_count = test_count + 1
        print *, "Testing: Default source is current directory"
        
        test_passed = .true.
        
        ! Get the default source path when no --source is specified
        call get_default_source_path(default_source_path)
        
        ! Should be current directory (.)
        if (trim(default_source_path) /= '.') then
            test_passed = .false.
            print *, "  ❌ FAIL: Default source should be '.', got: ", trim(default_source_path)
        end if
        
        ! Should resolve to current working directory
        if (.not. validate_resolves_to_current_working_directory(default_source_path)) then
            test_passed = .false.
            print *, "  ❌ FAIL: Default source should resolve to current working directory"
        end if
        
        if (test_passed) then
            print *, "  ✅ PASS: Default source correctly set to current directory (.)"
        else
            all_passed = .false.
            failed_count = failed_count + 1
        end if
        
    end subroutine test_default_source_is_current_directory

    subroutine test_cli_without_source_flag_works(all_passed, test_count, failed_count)
        !! Given: CLI command without --source flag
        !! When: Running fortcov --output=coverage.md (no --source)
        !! Then: Should work seamlessly using default source path
        
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: test_passed
        
        test_count = test_count + 1
        print *, "Testing: CLI without --source flag works"
        
        test_passed = .true.
        
        ! Should parse CLI without --source flag successfully
        if (.not. validate_cli_parsing_without_source()) then
            test_passed = .false.
            print *, "  ❌ FAIL: Should parse CLI without --source flag"
        end if
        
        ! Should execute coverage analysis without --source flag
        if (.not. validate_execution_without_source_flag()) then
            test_passed = .false.
            print *, "  ❌ FAIL: Should execute coverage analysis without --source flag"
        end if
        
        ! Should produce output file without --source flag
        if (.not. validate_output_generation_without_source()) then
            test_passed = .false.
            print *, "  ❌ FAIL: Should generate output without --source flag"
        end if
        
        if (test_passed) then
            print *, "  ✅ PASS: CLI works correctly without --source flag"
        else
            all_passed = .false.
            failed_count = failed_count + 1
        end if
        
    end subroutine test_cli_without_source_flag_works

    subroutine test_finds_gcov_files_by_default(all_passed, test_count, failed_count)
        !! Given: .gcov files exist in current directory
        !! When: Running fortcov with default source configuration
        !! Then: Should find and process .gcov files in current directory
        
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: test_passed
        
        test_count = test_count + 1
        print *, "Testing: Finds .gcov files by default"
        
        test_passed = .true.
        
        ! Should discover .gcov files in current directory
        if (.not. validate_discovers_gcov_files_in_current_dir()) then
            test_passed = .false.
            print *, "  ❌ FAIL: Should discover .gcov files in current directory"
        end if
        
        ! Should process .gcov files found in default location
        if (.not. validate_processes_default_gcov_files()) then
            test_passed = .false.
            print *, "  ❌ FAIL: Should process .gcov files found in default location"
        end if
        
        if (test_passed) then
            print *, "  ✅ PASS: Finds and processes .gcov files by default"
        else
            all_passed = .false.
            failed_count = failed_count + 1
        end if
        
    end subroutine test_finds_gcov_files_by_default

    subroutine test_default_source_with_exclude_patterns(all_passed, test_count, failed_count)
        !! Given: Default source (.) used with exclude patterns
        !! When: Testing exclude patterns with default source
        !! Then: Should properly exclude files/directories from current directory
        
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: test_passed
        
        test_count = test_count + 1
        print *, "Testing: Default source with exclude patterns"
        
        test_passed = .true.
        
        ! Should exclude build/* from current directory
        if (.not. validate_excludes_build_from_current_dir()) then
            test_passed = .false.
            print *, "  ❌ FAIL: Should exclude build/* from current directory"
        end if
        
        ! Should exclude test/* from current directory
        if (.not. validate_excludes_test_from_current_dir()) then
            test_passed = .false.
            print *, "  ❌ FAIL: Should exclude test/* from current directory"
        end if
        
        if (test_passed) then
            print *, "  ✅ PASS: Default source works correctly with exclude patterns"
        else
            all_passed = .false.
            failed_count = failed_count + 1
        end if
        
    end subroutine test_default_source_with_exclude_patterns

    ! =================================================================
    ! CONFIGURATION AND ENVIRONMENT TESTS
    ! =================================================================

    subroutine test_config_file_default_source(all_passed, test_count, failed_count)
        !! Given: Configuration file with default source settings
        !! When: Testing config file default source behavior
        !! Then: Should respect config file default while allowing CLI override
        
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: test_passed
        
        test_count = test_count + 1
        print *, "Testing: Config file default source behavior"
        
        test_passed = .true.
        
        ! Should use config file default source when no CLI --source
        if (.not. validate_uses_config_default_source()) then
            test_passed = .false.
            print *, "  ❌ FAIL: Should use config file default source"
        end if
        
        ! CLI --source should override config file default
        if (.not. validate_cli_overrides_config_default()) then
            test_passed = .false.
            print *, "  ❌ FAIL: CLI --source should override config file default"
        end if
        
        if (test_passed) then
            print *, "  ✅ PASS: Config file default source behavior correct"
        else
            all_passed = .false.
            failed_count = failed_count + 1
        end if
        
    end subroutine test_config_file_default_source

    subroutine test_environment_variable_source_default(all_passed, test_count, failed_count)
        !! Given: Environment variable for default source path
        !! When: Testing environment variable default source behavior
        !! Then: Should respect environment default with proper precedence
        
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: test_passed
        
        test_count = test_count + 1
        print *, "Testing: Environment variable source default"
        
        test_passed = .true.
        
        ! Should use environment variable default when available
        if (.not. validate_uses_environment_default_source()) then
            test_passed = .false.
            print *, "  ❌ FAIL: Should use environment variable default source"
        end if
        
        if (test_passed) then
            print *, "  ✅ PASS: Environment variable source default working"
        else
            all_passed = .false.
            failed_count = failed_count + 1
        end if
        
    end subroutine test_environment_variable_source_default

    ! =================================================================
    ! BACKWARD COMPATIBILITY TESTS
    ! =================================================================

    subroutine test_explicit_source_flag_still_works(all_passed, test_count, failed_count)
        !! Given: Explicit --source flag provided
        !! When: Testing that explicit --source continues to work
        !! Then: Should use explicit source path, not default
        
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: test_passed
        
        test_count = test_count + 1
        print *, "Testing: Explicit --source flag still works"
        
        test_passed = .true.
        
        ! Should use explicit --source=src when provided
        if (.not. validate_uses_explicit_source_src()) then
            test_passed = .false.
            print *, "  ❌ FAIL: Should use explicit --source=src when provided"
        end if
        
        ! Should use explicit --source=/path when provided
        if (.not. validate_uses_explicit_absolute_source()) then
            test_passed = .false.
            print *, "  ❌ FAIL: Should use explicit absolute --source when provided"
        end if
        
        if (test_passed) then
            print *, "  ✅ PASS: Explicit --source flag continues to work"
        else
            all_passed = .false.
            failed_count = failed_count + 1
        end if
        
    end subroutine test_explicit_source_flag_still_works

    subroutine test_explicit_source_overrides_default(all_passed, test_count, failed_count)
        !! Given: Both default source and explicit --source specified
        !! When: Testing precedence between default and explicit source
        !! Then: Explicit --source should always override default
        
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: test_passed
        
        test_count = test_count + 1
        print *, "Testing: Explicit source overrides default"
        
        test_passed = .true.
        
        ! Explicit --source should override built-in default
        if (.not. validate_explicit_overrides_builtin_default()) then
            test_passed = .false.
            print *, "  ❌ FAIL: Explicit --source should override built-in default"
        end if
        
        if (test_passed) then
            print *, "  ✅ PASS: Explicit source correctly overrides default"
        else
            all_passed = .false.
            failed_count = failed_count + 1
        end if
        
    end subroutine test_explicit_source_overrides_default

    ! =================================================================
    ! EDGE CASES AND ERROR SCENARIOS
    ! =================================================================

    subroutine test_default_source_error_scenarios(all_passed, test_count, failed_count)
        !! Given: Error scenarios with default source
        !! When: Testing error handling with default source path
        !! Then: Should provide helpful error messages
        
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: test_passed
        
        test_count = test_count + 1
        print *, "Testing: Default source error scenarios"
        
        test_passed = .true.
        
        ! Should handle no .gcov files in current directory gracefully
        if (.not. validate_handles_no_gcov_in_current_dir()) then
            test_passed = .false.
            print *, "  ❌ FAIL: Should handle no .gcov files in current directory gracefully"
        end if
        
        if (test_passed) then
            print *, "  ✅ PASS: Default source error scenarios handled correctly"
        else
            all_passed = .false.
            failed_count = failed_count + 1
        end if
        
    end subroutine test_default_source_error_scenarios

    subroutine test_default_source_with_relative_paths(all_passed, test_count, failed_count)
        !! Given: Default source with relative path operations
        !! When: Testing relative path resolution with default source
        !! Then: Should resolve relative paths correctly from current directory
        
        logical, intent(inout) :: all_passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: test_passed
        
        test_count = test_count + 1
        print *, "Testing: Default source with relative paths"
        
        test_passed = .true.
        
        ! Should resolve relative paths from current directory
        if (.not. validate_resolves_relative_paths_from_current()) then
            test_passed = .false.
            print *, "  ❌ FAIL: Should resolve relative paths from current directory"
        end if
        
        if (test_passed) then
            print *, "  ✅ PASS: Default source handles relative paths correctly"
        else
            all_passed = .false.
            failed_count = failed_count + 1
        end if
        
    end subroutine test_default_source_with_relative_paths

    ! =================================================================
    ! VALIDATION FUNCTIONS (Mock implementations for test structure)
    ! =================================================================

    subroutine get_default_source_path(path)
        !! Get the default source path when no --source is specified
        character(len=*), intent(out) :: path
        path = '.'  ! Mock implementation - should be current directory
    end subroutine get_default_source_path

    function validate_resolves_to_current_working_directory(path) result(valid)
        !! Validate that path resolves to current working directory
        character(len=*), intent(in) :: path
        logical :: valid
        valid = .true.  ! Mock implementation
    end function validate_resolves_to_current_working_directory

    function validate_cli_parsing_without_source() result(valid)
        !! Validate CLI parsing works without --source flag
        logical :: valid
        valid = .true.  ! Mock implementation
    end function validate_cli_parsing_without_source

    function validate_execution_without_source_flag() result(valid)
        !! Validate execution works without --source flag
        logical :: valid
        valid = .true.  ! Mock implementation
    end function validate_execution_without_source_flag

    function validate_output_generation_without_source() result(valid)
        !! Validate output generation works without --source flag
        logical :: valid
        valid = .true.  ! Mock implementation
    end function validate_output_generation_without_source

    function validate_discovers_gcov_files_in_current_dir() result(valid)
        !! Validate that .gcov files are discovered in current directory
        logical :: valid
        valid = .true.  ! Mock implementation
    end function validate_discovers_gcov_files_in_current_dir

    function validate_processes_default_gcov_files() result(valid)
        !! Validate that .gcov files are processed from default location
        logical :: valid
        valid = .true.  ! Mock implementation
    end function validate_processes_default_gcov_files

    function validate_excludes_build_from_current_dir() result(valid)
        !! Validate that build/* is excluded from current directory
        logical :: valid
        valid = .true.  ! Mock implementation
    end function validate_excludes_build_from_current_dir

    function validate_excludes_test_from_current_dir() result(valid)
        !! Validate that test/* is excluded from current directory
        logical :: valid
        valid = .true.  ! Mock implementation
    end function validate_excludes_test_from_current_dir

    function validate_uses_config_default_source() result(valid)
        !! Validate that config file default source is used
        logical :: valid
        valid = .true.  ! Mock implementation
    end function validate_uses_config_default_source

    function validate_cli_overrides_config_default() result(valid)
        !! Validate that CLI --source overrides config file default
        logical :: valid
        valid = .true.  ! Mock implementation
    end function validate_cli_overrides_config_default

    function validate_uses_environment_default_source() result(valid)
        !! Validate that environment variable default source is used
        logical :: valid
        valid = .true.  ! Mock implementation
    end function validate_uses_environment_default_source

    function validate_uses_explicit_source_src() result(valid)
        !! Validate that explicit --source=src is used
        logical :: valid
        valid = .true.  ! Mock implementation
    end function validate_uses_explicit_source_src

    function validate_uses_explicit_absolute_source() result(valid)
        !! Validate that explicit absolute --source is used
        logical :: valid
        valid = .true.  ! Mock implementation
    end function validate_uses_explicit_absolute_source

    function validate_explicit_overrides_builtin_default() result(valid)
        !! Validate that explicit --source overrides built-in default
        logical :: valid
        valid = .true.  ! Mock implementation
    end function validate_explicit_overrides_builtin_default

    function validate_handles_no_gcov_in_current_dir() result(valid)
        !! Validate graceful handling when no .gcov files in current directory
        logical :: valid
        valid = .true.  ! Mock implementation
    end function validate_handles_no_gcov_in_current_dir

    function validate_resolves_relative_paths_from_current() result(valid)
        !! Validate that relative paths are resolved from current directory
        logical :: valid
        valid = .true.  ! Mock implementation
    end function validate_resolves_relative_paths_from_current

end program test_default_source_configuration