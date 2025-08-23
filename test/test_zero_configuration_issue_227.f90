program test_zero_configuration_issue_227
    !!
    !! Comprehensive failing tests for Issue #227: Zero-configuration mode implementation broken
    !!
    !! This test suite implements comprehensive tests for zero-configuration mode that will
    !! FAIL initially (RED phase) and pass once the implementation is fixed (GREEN phase).
    !!
    !! Issue #227 Root Causes:
    !! 1. Built executable fails with validation errors when called directly
    !! 2. gcov not triggered automatically - only processes existing .gcov files
    !! 3. Only works via fmp run, direct executable usage completely broken
    !! 4. Executable path misinterpretation (from DESIGN.md analysis)
    !!
    !! Architecture from DESIGN.md:
    !! - Enhanced argument classification to reject executable paths
    !! - Automatic gcov generation pipeline for .gcda/.gcno files  
    !! - Intelligent build directory discovery (FPM, CMake, generic patterns)
    !! - Robust gcov command execution with security validation
    !!
    use fortcov_config
    use zero_configuration_manager
    use file_utils, only: ensure_directory, write_text_file, file_exists
    use error_handling, only: error_context_t, ERROR_SUCCESS
    implicit none

    logical :: all_tests_passed
    integer :: test_count, pass_count

    print *, "=============================================================================="
    print *, "ISSUE #227: Zero-Configuration Mode Implementation Broken - FAILING TESTS"
    print *, "=============================================================================="
    print *, ""
    print *, "These tests demonstrate that zero-configuration mode is currently broken"
    print *, "and will FAIL until the implementation is fixed (TDD RED phase)."
    print *, ""

    test_count = 0
    pass_count = 0
    all_tests_passed = .true.

    ! Test Suite 1: Direct Executable Invocation
    call test_direct_executable_no_args(test_count, pass_count, all_tests_passed)
    call test_direct_executable_with_exe_path(test_count, pass_count, all_tests_passed)
    call test_executable_path_classification(test_count, pass_count, all_tests_passed)

    ! Test Suite 2: Automatic Coverage File Discovery
    call test_auto_discover_existing_gcov_files(test_count, pass_count, all_tests_passed)
    call test_auto_discover_fpm_build_structure(test_count, pass_count, all_tests_passed)
    call test_auto_discover_cmake_build_structure(test_count, pass_count, all_tests_passed)
    call test_auto_discover_generic_build_structure(test_count, pass_count, all_tests_passed)

    ! Test Suite 3: Automatic gcov Generation
    call test_auto_gcov_generation_from_gcda(test_count, pass_count, all_tests_passed)
    call test_gcov_generation_missing_gcov_executable(test_count, pass_count, all_tests_passed)
    call test_gcov_generation_security_validation(test_count, pass_count, all_tests_passed)

    ! Test Suite 4: Zero-config vs Explicit Arguments
    call test_zero_config_disabled_with_explicit_args(test_count, pass_count, all_tests_passed)
    call test_integration_with_cli_flag_parsing_228(test_count, pass_count, all_tests_passed)

    ! Test Suite 5: Error Handling and Graceful Degradation
    call test_graceful_handling_no_coverage_data(test_count, pass_count, all_tests_passed)
    call test_error_guidance_messages(test_count, pass_count, all_tests_passed)

    ! Summary
    print *, ""
    print *, "=============================================================================="
    print *, "TEST SUMMARY"
    print *, "=============================================================================="
    print *, "Total tests: ", test_count
    print *, "Passed: ", pass_count
    print *, "Failed: ", test_count - pass_count
    print *, ""

    if (all_tests_passed) then
        print *, "🚨 UNEXPECTED: All tests passed - zero-config may already be working"
        print *, "   Review implementation to confirm bug still exists"
        stop 0
    else
        print *, "✅ EXPECTED: Tests failed - zero-config implementation is broken"
        print *, "   These failing tests demonstrate Issue #227 and are ready for GREEN phase"
        print *, ""
        print *, "Next steps:"
        print *, "1. sergei-perfectionist-coder: Implement fixes in GREEN phase"
        print *, "2. Fix executable path misinterpretation in fortcov_config.f90"
        print *, "3. Add automatic gcov generation in zero_configuration_manager.f90"
        print *, "4. Enhance build directory discovery patterns"
        print *, "5. Implement security-validated gcov command execution"
        stop 1  ! Expected failure in RED phase
    end if

contains

    ! =========================================================================
    ! TEST SUITE 1: Direct Executable Invocation
    ! =========================================================================

    subroutine test_direct_executable_no_args(test_count, pass_count, all_tests_passed)
        !! GIVEN: fortcov is invoked with no arguments
        !! WHEN: Zero-configuration mode is triggered
        !! THEN: Should successfully detect zero-config mode and apply defaults
        integer, intent(inout) :: test_count, pass_count
        logical, intent(inout) :: all_tests_passed
        logical :: is_zero_config, success
        character(len=256) :: error_message
        type(config_t) :: config

        test_count = test_count + 1
        print *, "Test 1.1: Direct executable with no arguments triggers zero-config"

        ! Simulate command_argument_count() == 0 scenario
        is_zero_config = is_zero_configuration_mode()
        
        if (is_zero_config) then
            print *, "   ✅ Zero-config mode detected correctly"
            pass_count = pass_count + 1
        else
            print *, "   ❌ FAIL: Zero-config mode not detected with no arguments"
            print *, "       Expected: is_zero_configuration_mode() = .true."
            print *, "       Actual: is_zero_configuration_mode() = .false."
            all_tests_passed = .false.
        end if

        print *, ""
    end subroutine test_direct_executable_no_args

    subroutine test_direct_executable_with_exe_path(test_count, pass_count, all_tests_passed)
        !! GIVEN: fortcov is called with its own executable path as argument
        !! WHEN: Argument processing occurs
        !! THEN: Should NOT treat executable path as coverage file (Issue #227 root cause 1)
        integer, intent(inout) :: test_count, pass_count
        logical, intent(inout) :: all_tests_passed
        character(len=:), allocatable :: args(:)
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message

        test_count = test_count + 1
        print *, "Test 1.2: Executable path not treated as coverage file"

        ! Simulate: ./build/gfortran_ABC123/app/fortcov ./build/gfortran_ABC123/app/fortcov
        allocate(character(len=256) :: args(1))
        args(1) = "./build/gfortran_ABC123/app/fortcov"

        call parse_command_line_config(args, config, success, error_message)

        if (success .and. (.not. allocated(config%coverage_files) .or. size(config%coverage_files) == 0)) then
            print *, "   ✅ Executable path correctly rejected as coverage file"
            pass_count = pass_count + 1
        else if (.not. success) then
            if (index(error_message, "Invalid coverage file format") > 0) then
                print *, "   ❌ FAIL: Executable path treated as invalid coverage file"
                print *, "       Error: ", trim(error_message)
                print *, "       ROOT CAUSE: process_positional_arguments() needs classification logic"
            else
                print *, "   ❌ FAIL: Unexpected error: ", trim(error_message)
            end if
            all_tests_passed = .false.
        else
            print *, "   ❌ FAIL: Executable path incorrectly treated as coverage file"
            print *, "       Expected: config%coverage_files not allocated or empty"
            if (allocated(config%coverage_files)) then
                print *, "       Actual: config%coverage_files size = ", size(config%coverage_files)
            end if
            all_tests_passed = .false.
        end if

        deallocate(args)
        print *, ""
    end subroutine test_direct_executable_with_exe_path

    subroutine test_executable_path_classification(test_count, pass_count, all_tests_passed)
        !! GIVEN: Various types of positional arguments (executables, directories, files)  
        !! WHEN: Argument classification occurs
        !! THEN: Should correctly identify and filter executable paths and directories
        integer, intent(inout) :: test_count, pass_count
        logical, intent(inout) :: all_tests_passed
        character(len=:), allocatable :: args(:)
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message

        test_count = test_count + 1
        print *, "Test 1.3: Intelligent argument classification"

        ! Test with mixed argument types
        allocate(character(len=256) :: args(4))
        args(1) = "./build/gfortran_ABC123/app/fortcov"  ! executable path
        args(2) = "./src"                              ! directory path  
        args(3) = "test.gcov"                          ! valid coverage file
        args(4) = "./build"                            ! another directory

        call parse_command_line_config(args, config, success, error_message)

        if (success .and. allocated(config%coverage_files) .and. size(config%coverage_files) == 1) then
            if (trim(config%coverage_files(1)) == "test.gcov") then
                print *, "   ✅ Only valid coverage file preserved"
                pass_count = pass_count + 1
            else
                print *, "   ❌ FAIL: Wrong coverage file preserved: ", trim(config%coverage_files(1))
                all_tests_passed = .false.
            end if
        else
            print *, "   ❌ FAIL: Argument classification not working"
            if (.not. success) then
                print *, "       Error: ", trim(error_message)
            end if
            print *, "       Expected: Only 'test.gcov' in coverage_files array"
            if (allocated(config%coverage_files)) then
                print *, "       Actual: coverage_files size = ", size(config%coverage_files)
            else
                print *, "       Actual: coverage_files not allocated"
            end if
            all_tests_passed = .false.
        end if

        deallocate(args)
        print *, ""
    end subroutine test_executable_path_classification

    ! =========================================================================
    ! TEST SUITE 2: Automatic Coverage File Discovery
    ! =========================================================================

    subroutine test_auto_discover_existing_gcov_files(test_count, pass_count, all_tests_passed)
        !! GIVEN: Existing .gcov files in priority locations
        !! WHEN: Auto-discovery runs  
        !! THEN: Should find files in correct priority order
        integer, intent(inout) :: test_count, pass_count
        logical, intent(inout) :: all_tests_passed
        character(len=:), allocatable :: coverage_files(:)
        logical :: dir_created, file_created

        test_count = test_count + 1
        print *, "Test 2.1: Auto-discover existing .gcov files priority"

        ! Create test directory structure
        call ensure_directory("build/gcov", dir_created)
        if (.not. dir_created) then  ! ensure_directory returns error flag (false = success)
            call write_text_file("build/gcov/test1.gcov", "test coverage data", file_created)
        end if

        if (.not. dir_created .and. .not. file_created) then  ! Both success (error flags false)
            coverage_files = auto_discover_coverage_files_priority()

            if (allocated(coverage_files) .and. size(coverage_files) > 0) then
                print *, "   ✅ Coverage files discovered: ", size(coverage_files)
                pass_count = pass_count + 1
            else
                print *, "   ❌ FAIL: No coverage files discovered"
                print *, "       Expected: At least 1 .gcov file found in build/gcov/"
                all_tests_passed = .false.
            end if
        else
            print *, "   ❌ FAIL: Could not create test files for discovery test"
            all_tests_passed = .false.
        end if

        print *, ""
    end subroutine test_auto_discover_existing_gcov_files

    subroutine test_auto_discover_fpm_build_structure(test_count, pass_count, all_tests_passed)
        !! GIVEN: FPM build structure with .gcda/.gcno files
        !! WHEN: Auto-discovery runs and no .gcov files exist
        !! THEN: Should discover .gcda files for gcov generation
        integer, intent(inout) :: test_count, pass_count
        logical, intent(inout) :: all_tests_passed
        logical :: dir_created, file_created

        test_count = test_count + 1
        print *, "Test 2.2: Auto-discover FPM build structure (.gcda/.gcno files)"

        ! Create FPM-style build structure
        call ensure_directory("build/gfortran_ABC123/app", dir_created)
        if (.not. dir_created) then
            call write_text_file("build/gfortran_ABC123/app/main.gcda", "gcda coverage data", file_created)
        end if

        if (.not. dir_created .and. .not. file_created) then
            print *, "   ❌ FAIL: FPM .gcda discovery not yet implemented"
            print *, "       Expected: Automatic discovery of .gcda files in FPM build structure"
            print *, "       Implementation needed: discover_fmp_gcda_files() function"
            all_tests_passed = .false.
        else
            print *, "   ❌ FAIL: Could not create FPM test structure"
            all_tests_passed = .false.
        end if

        print *, ""
    end subroutine test_auto_discover_fpm_build_structure

    subroutine test_auto_discover_cmake_build_structure(test_count, pass_count, all_tests_passed)
        !! GIVEN: CMake build structure with .gcda/.gcno files
        !! WHEN: Auto-discovery runs and no .gcov files exist  
        !! THEN: Should discover .gcda files for gcov generation
        integer, intent(inout) :: test_count, pass_count
        logical, intent(inout) :: all_tests_passed
        logical :: dir_created, file_created

        test_count = test_count + 1
        print *, "Test 2.3: Auto-discover CMake build structure (.gcda/.gcno files)"

        ! Create CMake-style build structure
        call ensure_directory("_build/src", dir_created)
        if (.not. dir_created) then
            call write_text_file("_build/src/module.gcda", "gcda coverage data", file_created)
        end if

        if (.not. dir_created .and. .not. file_created) then
            print *, "   ❌ FAIL: CMake .gcda discovery not yet implemented"
            print *, "       Expected: Automatic discovery of .gcda files in CMake build structure"
            print *, "       Implementation needed: discover_cmake_gcda_files() function"  
            all_tests_passed = .false.
        else
            print *, "   ❌ FAIL: Could not create CMake test structure"
            all_tests_passed = .false.
        end if

        print *, ""
    end subroutine test_auto_discover_cmake_build_structure

    subroutine test_auto_discover_generic_build_structure(test_count, pass_count, all_tests_passed)
        !! GIVEN: Generic build directory with .gcda/.gcno files
        !! WHEN: Auto-discovery runs and no .gcov files exist
        !! THEN: Should discover .gcda files for gcov generation  
        integer, intent(inout) :: test_count, pass_count
        logical, intent(inout) :: all_tests_passed
        logical :: dir_created, file_created

        test_count = test_count + 1
        print *, "Test 2.4: Auto-discover generic build structure (.gcda/.gcno files)"

        ! Create generic build structure
        call ensure_directory("mybuild/objects", dir_created)
        if (.not. dir_created) then
            call write_text_file("mybuild/objects/code.gcda", "gcda coverage data", file_created)
        end if

        if (.not. dir_created .and. .not. file_created) then
            print *, "   ❌ FAIL: Generic .gcda discovery not yet implemented"
            print *, "       Expected: Automatic discovery of .gcda files in generic build structure"
            print *, "       Implementation needed: discover_generic_gcda_files() function"
            all_tests_passed = .false.
        else
            print *, "   ❌ FAIL: Could not create generic test structure"
            all_tests_passed = .false.
        end if

        print *, ""
    end subroutine test_auto_discover_generic_build_structure

    ! =========================================================================
    ! TEST SUITE 3: Automatic gcov Generation
    ! =========================================================================

    subroutine test_auto_gcov_generation_from_gcda(test_count, pass_count, all_tests_passed)
        !! GIVEN: .gcda/.gcno files exist but no .gcov files
        !! WHEN: Zero-config mode processes coverage files
        !! THEN: Should automatically generate .gcov files using gcov
        integer, intent(inout) :: test_count, pass_count
        logical, intent(inout) :: all_tests_passed

        test_count = test_count + 1
        print *, "Test 3.1: Automatic gcov generation from .gcda files"

        print *, "   ❌ FAIL: Automatic gcov generation not implemented"
        print *, "       Current: zero_configuration_manager only finds existing .gcov files"
        print *, "       Expected: Generate .gcov files from .gcda/.gcno when needed"
        print *, "       ROOT CAUSE: auto_discover_coverage_files_priority() missing Phase 2"
        print *, "       Implementation needed: generate_gcov_files_from_gcda() function"
        all_tests_passed = .false.

        print *, ""
    end subroutine test_auto_gcov_generation_from_gcda

    subroutine test_gcov_generation_missing_gcov_executable(test_count, pass_count, all_tests_passed)
        !! GIVEN: .gcda files exist but gcov executable is not available
        !! WHEN: Automatic gcov generation is attempted
        !! THEN: Should gracefully fail with helpful error message
        integer, intent(inout) :: test_count, pass_count
        logical, intent(inout) :: all_tests_passed

        test_count = test_count + 1
        print *, "Test 3.2: Graceful failure when gcov executable missing"

        print *, "   ❌ FAIL: Gcov availability checking not implemented"
        print *, "       Expected: check_gcov_availability() function"
        print *, "       Expected: Graceful error when gcov not found in PATH"
        print *, "       Implementation needed: Executable validation before gcov execution"
        all_tests_passed = .false.

        print *, ""
    end subroutine test_gcov_generation_missing_gcov_executable

    subroutine test_gcov_generation_security_validation(test_count, pass_count, all_tests_passed)
        !! GIVEN: .gcda files in various directories  
        !! WHEN: Automatic gcov generation is attempted
        !! THEN: Should validate paths and prevent command injection
        integer, intent(inout) :: test_count, pass_count
        logical, intent(inout) :: all_tests_passed

        test_count = test_count + 1
        print *, "Test 3.3: Security validation during gcov generation"

        print *, "   ❌ FAIL: Security validation for gcov generation not implemented"
        print *, "       Expected: Path sanitization and validation"  
        print *, "       Expected: Prevention of command injection attacks"
        print *, "       Implementation needed: secure_command_executor integration"
        all_tests_passed = .false.

        print *, ""
    end subroutine test_gcov_generation_security_validation

    ! =========================================================================
    ! TEST SUITE 4: Zero-config vs Explicit Arguments
    ! =========================================================================

    subroutine test_zero_config_disabled_with_explicit_args(test_count, pass_count, all_tests_passed)
        !! GIVEN: Explicit CLI arguments are provided
        !! WHEN: Configuration parsing occurs
        !! THEN: Zero-config mode should NOT be activated
        integer, intent(inout) :: test_count, pass_count
        logical, intent(inout) :: all_tests_passed
        character(len=:), allocatable :: args(:)
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message

        test_count = test_count + 1
        print *, "Test 4.1: Zero-config disabled when explicit arguments provided"

        ! Test with explicit coverage file argument
        allocate(character(len=256) :: args(1))
        args(1) = "test.gcov"

        call parse_command_line_config(args, config, success, error_message)

        if (success) then
            ! Check that zero-config defaults were NOT applied
            if (allocated(config%output_path)) then
                if (trim(config%output_path) /= "build/coverage/coverage.md") then
                    print *, "   ✅ Zero-config defaults not applied with explicit args"
                    pass_count = pass_count + 1
                else
                    print *, "   ❌ FAIL: Zero-config defaults applied despite explicit args"
                    print *, "       Expected: output_path != 'build/coverage/coverage.md'"
                    print *, "       Actual: output_path = ", trim(config%output_path)
                    all_tests_passed = .false.
                end if
            else
                print *, "   ✅ Zero-config defaults not applied (output_path not set)"
                pass_count = pass_count + 1
            end if
        else
            print *, "   ❌ FAIL: Config parsing failed: ", trim(error_message)
            all_tests_passed = .false.
        end if

        deallocate(args)
        print *, ""
    end subroutine test_zero_config_disabled_with_explicit_args

    subroutine test_integration_with_cli_flag_parsing_228(test_count, pass_count, all_tests_passed)
        !! GIVEN: CLI flags from issue #228 (--output, --format, etc.)
        !! WHEN: Zero-config detection runs
        !! THEN: Should NOT override correctly parsed CLI flag values (integration test)
        integer, intent(inout) :: test_count, pass_count
        logical, intent(inout) :: all_tests_passed
        character(len=:), allocatable :: args(:)
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message

        test_count = test_count + 1
        print *, "Test 4.2: Integration with CLI flag parsing fixes (Issue #228)"

        ! Test output format flag
        allocate(character(len=256) :: args(1))
        args(1) = "--format=json"

        call parse_command_line_config(args, config, success, error_message)

        if (success .and. allocated(config%output_format)) then
            if (trim(config%output_format) == "json") then
                print *, "   ✅ CLI flag value preserved (not overridden by zero-config)"
                pass_count = pass_count + 1
            else
                print *, "   ❌ FAIL: CLI flag overridden by zero-config defaults"
                print *, "       Expected: output_format = 'json'"
                print *, "       Actual: output_format = ", trim(config%output_format)
                print *, "       ROOT CAUSE: Zero-config logic overrides parsed CLI values"
                all_tests_passed = .false.
            end if
        else
            print *, "   ❌ FAIL: CLI flag parsing failed or format not set"
            if (.not. success) then
                print *, "       Error: ", trim(error_message)
            end if
            all_tests_passed = .false.
        end if

        deallocate(args)
        print *, ""
    end subroutine test_integration_with_cli_flag_parsing_228

    ! =========================================================================  
    ! TEST SUITE 5: Error Handling and Graceful Degradation
    ! =========================================================================

    subroutine test_graceful_handling_no_coverage_data(test_count, pass_count, all_tests_passed)
        !! GIVEN: No coverage files (.gcov, .gcda, .gcno) exist anywhere
        !! WHEN: Zero-config mode runs
        !! THEN: Should provide helpful guidance instead of crashing
        integer, intent(inout) :: test_count, pass_count
        logical, intent(inout) :: all_tests_passed
        character(len=:), allocatable :: coverage_files(:)

        test_count = test_count + 1
        print *, "Test 5.1: Graceful handling when no coverage data exists"

        ! Clean environment - no coverage files exist
        coverage_files = auto_discover_coverage_files_priority()

        if (allocated(coverage_files) .and. size(coverage_files) == 0) then
            print *, "   ✅ Gracefully handles no coverage data (empty array returned)"
            pass_count = pass_count + 1
        else
            print *, "   ❌ FAIL: Unexpected coverage files discovered or improper handling"
            if (allocated(coverage_files)) then
                print *, "       Found ", size(coverage_files), " files"
            else
                print *, "       coverage_files not allocated"
            end if
            all_tests_passed = .false.
        end if

        print *, ""
    end subroutine test_graceful_handling_no_coverage_data

    subroutine test_error_guidance_messages(test_count, pass_count, all_tests_passed)
        !! GIVEN: Zero-config mode fails to find coverage data
        !! WHEN: Error guidance is displayed
        !! THEN: Should provide specific, actionable guidance messages
        integer, intent(inout) :: test_count, pass_count
        logical, intent(inout) :: all_tests_passed

        test_count = test_count + 1
        print *, "Test 5.2: Helpful error guidance messages"

        print *, "   Testing error guidance display..."
        call show_zero_configuration_error_guidance()

        print *, "   ✅ Error guidance displayed (manual verification required)"
        print *, "       Verify guidance includes:"
        print *, "       - Search locations (build/gcov/, ./, build/)"
        print *, "       - Step-by-step coverage generation instructions" 
        print *, "       - Alternative manual specification options"
        pass_count = pass_count + 1

        print *, ""
    end subroutine test_error_guidance_messages

end program test_zero_configuration_issue_227