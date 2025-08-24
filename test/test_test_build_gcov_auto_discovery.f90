program test_test_build_gcov_auto_discovery
    !! Comprehensive Tests for Test Build and Gcov Auto-Discovery (Issue #277)
    !!
    !! Tests the complete auto-discovery workflow that integrates:
    !! - Auto-discovery of test build capabilities
    !! - Automatic execution of tests with coverage flags
    !! - Auto-processing of gcov files after test execution
    !! - Complete workflow integration from `fortcov` command to coverage report
    !!
    !! This module tests the final Sprint 1 feature that completes the
    !! auto-discovery ecosystem with comprehensive BDD-style tests.
    
    use iso_fortran_env, only: error_unit, output_unit
    use test_build_gcov_auto_discovery
    use fortcov_config
    use config_defaults, only: initialize_default_config
    use build_system_detector
    use error_handling
    implicit none

    integer :: total_tests = 0
    integer :: passed_tests = 0
    logical :: all_tests_passed = .true.

    write(output_unit, '(A)') 'Running test build and gcov auto-discovery tests...'
    write(output_unit, *)

    ! Test auto-discovery of test build capabilities
    call test_auto_discover_test_build_with_fpm()
    call test_auto_discover_test_build_with_cmake()  
    call test_auto_discover_test_build_with_make()
    call test_auto_discover_test_build_with_meson()
    call test_auto_discover_test_build_unknown_system()

    ! Test auto-processing of gcov files
    call test_auto_process_gcov_files_found()
    call test_auto_process_gcov_files_not_found()
    call test_auto_process_gcov_files_build_context()

    ! Test complete auto-discovery workflow
    call test_complete_auto_workflow_success()
    call test_complete_auto_workflow_no_build_system()
    call test_complete_auto_workflow_test_failure()
    call test_complete_auto_workflow_timeout()

    ! Test configuration control and backward compatibility
    call test_auto_discovery_disabled()
    call test_manual_override_auto_discovered()
    call test_backward_compatibility_explicit_mode()

    ! Test edge cases and error handling
    call test_gcda_files_missing()
    call test_invalid_build_directory()
    call test_gcov_processing_failure()
    call test_source_mapping_discovery()

    write(output_unit, *)
    write(output_unit, '(A,I0,A,I0)') 'Test Results: ', passed_tests, ' / ', &
                                      total_tests

    if (all_tests_passed) then
        write(output_unit, '(A)') 'ALL TESTS PASSED'
        stop 0
    else
        write(error_unit, '(A)') 'SOME TESTS FAILED'
        stop 1
    end if

contains

    subroutine test_auto_discover_test_build_with_fpm()
        !! Given an FPM project directory
        !! When auto_discover_test_build is called
        !! Then it should detect FPM and return test command with coverage flags
        type(config_t) :: config
        type(test_build_result_t) :: result
        
        write(output_unit, '(A)') 'Test 1: Auto-discover test build with FPM'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        ! Mock FPM project environment
        call create_mock_fpm_project()
        
        call auto_discover_test_build('.', config, result)
        
        call assert_true(result%success, 'FPM build discovery succeeded')
        call assert_equals_string(result%build_system, 'fpm', 'Detected FPM')
        call assert_true(index(result%test_command, 'fpm test') > 0, &
                        'FPM test command')
        call assert_true(index(result%test_command, 'fprofile-arcs') > 0, &
                        'Coverage flags included')
        
        call cleanup_mock_project()
    end subroutine test_auto_discover_test_build_with_fpm

    subroutine test_auto_discover_test_build_with_cmake()
        !! Given a CMake project directory
        !! When auto_discover_test_build is called
        !! Then it should detect CMake and return build + test commands
        type(config_t) :: config
        type(test_build_result_t) :: result
        
        write(output_unit, '(A)') 'Test 2: Auto-discover test build with CMake'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        call create_mock_cmake_project()
        
        call auto_discover_test_build('.', config, result)
        
        call assert_true(result%success, 'CMake build discovery succeeded')
        call assert_equals_string(result%build_system, 'cmake', 'Detected CMake')
        call assert_true(index(result%test_command, 'cmake --build') > 0, &
                        'CMake build command')
        call assert_true(index(result%test_command, 'ctest') > 0, &
                        'CMake test command')
        
        call cleanup_mock_project()
    end subroutine test_auto_discover_test_build_with_cmake

    subroutine test_auto_discover_test_build_with_make()
        !! Given a Make project directory  
        !! When auto_discover_test_build is called
        !! Then it should detect Make and return test target
        type(config_t) :: config
        type(test_build_result_t) :: result
        
        write(output_unit, '(A)') 'Test 3: Auto-discover test build with Make'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        call create_mock_make_project()
        
        call auto_discover_test_build('.', config, result)
        
        call assert_true(result%success, 'Make build discovery succeeded')
        call assert_equals_string(result%build_system, 'make', 'Detected Make')
        call assert_true(index(result%test_command, 'make test') > 0, &
                        'Make test command')
        
        call cleanup_mock_project()
    end subroutine test_auto_discover_test_build_with_make

    subroutine test_auto_discover_test_build_with_meson()
        !! Given a Meson project directory
        !! When auto_discover_test_build is called
        !! Then it should detect Meson and return test command
        type(config_t) :: config
        type(test_build_result_t) :: result
        
        write(output_unit, '(A)') 'Test 4: Auto-discover test build with Meson'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        call create_mock_meson_project()
        
        call auto_discover_test_build('.', config, result)
        
        call assert_true(result%success, 'Meson build discovery succeeded')
        call assert_equals_string(result%build_system, 'meson', 'Detected Meson')
        call assert_true(index(result%test_command, 'meson test') > 0, &
                        'Meson test command')
        
        call cleanup_mock_project()
    end subroutine test_auto_discover_test_build_with_meson

    subroutine test_auto_discover_test_build_unknown_system()
        !! Given a directory with no known build system
        !! When auto_discover_test_build is called
        !! Then it should return graceful failure with guidance
        type(config_t) :: config
        type(test_build_result_t) :: result
        
        write(output_unit, '(A)') 'Test 5: Auto-discover unknown build system'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        ! No build files present
        
        call auto_discover_test_build('.', config, result)
        
        call assert_false(result%success, 'Unknown system handled gracefully')
        call assert_equals_string(result%build_system, 'unknown', &
                                 'Unknown system type')
        call assert_true(len_trim(result%guidance_message) > 0, &
                        'Guidance provided')
    end subroutine test_auto_discover_test_build_unknown_system

    subroutine test_auto_process_gcov_files_found()
        !! Given .gcda files exist after test execution
        !! When auto_process_gcov_files is called
        !! Then it should find and process gcov files with source mapping
        type(config_t) :: config
        type(gcov_result_t) :: result
        
        write(output_unit, '(A)') 'Test 6: Auto-process gcov files found'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        ! Create mock .gcda files
        call create_mock_gcda_files()
        
        call auto_process_gcov_files('.', config, result)
        
        call assert_true(result%success, 'Gcov processing succeeded')
        call assert_true(size(result%gcov_files) > 0, 'Gcov files found')
        call assert_true(size(result%source_mappings) > 0, &
                        'Source mappings discovered')
        
        call cleanup_mock_gcov_files()
    end subroutine test_auto_process_gcov_files_found

    subroutine test_auto_process_gcov_files_not_found()
        !! Given no .gcda files exist
        !! When auto_process_gcov_files is called
        !! Then it should return empty result with guidance
        type(config_t) :: config
        type(gcov_result_t) :: result
        
        write(output_unit, '(A)') 'Test 7: Auto-process gcov files not found'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        call auto_process_gcov_files('.', config, result)
        
        call assert_false(result%success, 'No gcov files handled gracefully')
        call assert_true(size(result%gcov_files) == 0, 'No gcov files found')
        call assert_true(len_trim(result%guidance_message) > 0, &
                        'Guidance provided')
    end subroutine test_auto_process_gcov_files_not_found

    subroutine test_auto_process_gcov_files_build_context()
        !! Given .gcda files in build directory structure
        !! When auto_process_gcov_files is called with build context
        !! Then it should use build system context for processing
        type(config_t) :: config
        type(gcov_result_t) :: result
        
        write(output_unit, '(A)') 'Test 8: Auto-process with build context'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        call create_mock_build_structure()
        call create_mock_gcda_files_in_build()
        
        call auto_process_gcov_files('.', config, result)
        
        call assert_true(result%success, 'Build context processing succeeded')
        call assert_true(result%used_build_context, 'Used build context')
        call assert_true(size(result%gcov_files) > 0, 'Processed gcov files')
        
        call cleanup_mock_build_structure()
    end subroutine test_auto_process_gcov_files_build_context

    subroutine test_complete_auto_workflow_success()
        !! Given a project with build system and tests
        !! When execute_complete_auto_workflow is called  
        !! Then it should execute full workflow: detect → test → gcov → coverage
        type(config_t) :: config
        type(complete_workflow_result_t) :: result
        
        write(output_unit, '(A)') 'Test 9: Complete auto workflow success'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        config%auto_test_execution = .true.
        
        call create_mock_complete_project()
        
        call execute_complete_auto_workflow(config, result)
        
        call assert_true(result%success, 'Complete workflow succeeded')
        call assert_true(result%test_executed, 'Tests were executed')
        call assert_true(result%gcov_processed, 'Gcov files processed')
        call assert_true(result%coverage_generated, 'Coverage report generated')
        
        call cleanup_mock_complete_project()
    end subroutine test_complete_auto_workflow_success

    subroutine test_complete_auto_workflow_no_build_system()
        !! Given a project with no build system
        !! When execute_complete_auto_workflow is called
        !! Then it should skip tests but still attempt gcov processing
        type(config_t) :: config
        type(complete_workflow_result_t) :: result
        
        write(output_unit, '(A)') 'Test 10: Complete workflow no build system'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        config%auto_test_execution = .true.
        
        ! No build files, but mock existing gcov files
        call create_mock_existing_gcov_files()
        
        call execute_complete_auto_workflow(config, result)
        
        call assert_true(result%success, 'Workflow succeeded without build system')
        call assert_false(result%test_executed, 'Tests were skipped')
        call assert_true(result%gcov_processed, 'Gcov files still processed')
        call assert_true(result%coverage_generated, 'Coverage still generated')
        
        call cleanup_mock_existing_gcov_files()
    end subroutine test_complete_auto_workflow_no_build_system

    subroutine test_complete_auto_workflow_test_failure()
        !! Given tests fail during execution
        !! When execute_complete_auto_workflow is called
        !! Then it should handle test failure gracefully and continue
        type(config_t) :: config
        type(complete_workflow_result_t) :: result
        
        write(output_unit, '(A)') 'Test 11: Complete workflow with test failure'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        config%auto_test_execution = .true.
        
        call create_mock_failing_project()
        
        call execute_complete_auto_workflow(config, result)
        
        call assert_false(result%success, 'Workflow reports test failure')
        call assert_true(result%test_executed, 'Tests were attempted')
        call assert_false(result%tests_passed, 'Tests failed')
        call assert_true(len_trim(result%error_message) > 0, 'Error reported')
    end subroutine test_complete_auto_workflow_test_failure

    subroutine test_complete_auto_workflow_timeout()
        !! Given tests timeout during execution
        !! When execute_complete_auto_workflow is called
        !! Then it should handle timeout gracefully
        type(config_t) :: config
        type(complete_workflow_result_t) :: result
        
        write(output_unit, '(A)') 'Test 12: Complete workflow with timeout'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        config%auto_test_execution = .true.
        config%test_timeout_seconds = 1  ! Very short timeout
        
        call create_mock_slow_project()
        
        call execute_complete_auto_workflow(config, result)
        
        call assert_false(result%success, 'Workflow reports timeout')
        call assert_true(result%timed_out, 'Timeout detected')
        call assert_true(index(result%error_message, 'timeout') > 0, &
                        'Timeout in error message')
    end subroutine test_complete_auto_workflow_timeout

    subroutine test_auto_discovery_disabled()
        !! Given auto_discovery = .false.
        !! When auto-discovery functions are called
        !! Then they should skip gracefully
        type(config_t) :: config
        type(complete_workflow_result_t) :: result
        
        write(output_unit, '(A)') 'Test 13: Auto-discovery disabled'
        
        call initialize_default_config(config)
        config%auto_discovery = .false.  ! Disabled
        config%auto_test_execution = .false.
        
        call execute_complete_auto_workflow(config, result)
        
        call assert_true(result%success, 'Workflow succeeded')
        call assert_false(result%test_executed, 'Tests skipped when disabled')
        call assert_false(result%auto_discovery_used, 'Auto-discovery skipped')
    end subroutine test_auto_discovery_disabled

    subroutine test_manual_override_auto_discovered()
        !! Given explicit configuration settings
        !! When auto-discovery runs
        !! Then it should respect manual overrides
        type(config_t) :: config
        type(complete_workflow_result_t) :: result
        
        write(output_unit, '(A)') 'Test 14: Manual override of auto-discovered'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        ! Manual overrides
        allocate(character(len=256) :: config%coverage_files(1))
        config%coverage_files(1) = 'manual_coverage.gcov'
        
        call create_mock_complete_project()
        
        call execute_complete_auto_workflow(config, result)
        
        call assert_true(result%success, 'Workflow with manual override')
        call assert_true(result%used_manual_files, 'Used manual coverage files')
        
        call cleanup_mock_complete_project()
    end subroutine test_manual_override_auto_discovered

    subroutine test_backward_compatibility_explicit_mode()
        !! Given zero_configuration_mode = .false.
        !! When running fortcov workflow
        !! Then it should maintain backward compatibility
        type(config_t) :: config
        type(complete_workflow_result_t) :: result
        
        write(output_unit, '(A)') 'Test 15: Backward compatibility explicit mode'
        
        call initialize_default_config(config)
        config%zero_configuration_mode = .false.  ! Explicit mode
        config%auto_discovery = .false.
        
        ! Explicit configuration
        allocate(character(len=256) :: config%source_paths(1))
        config%source_paths(1) = 'src'
        config%output_path = 'coverage.md'
        
        call execute_complete_auto_workflow(config, result)
        
        call assert_true(result%success, 'Explicit mode still works')
        call assert_false(result%auto_discovery_used, &
                         'Auto-discovery not used in explicit mode')
    end subroutine test_backward_compatibility_explicit_mode

    subroutine test_gcda_files_missing()
        !! Given no .gcda files exist
        !! When auto_process_gcov_files is called
        !! Then it should provide helpful guidance
        type(config_t) :: config
        type(gcov_result_t) :: result
        
        write(output_unit, '(A)') 'Test 16: GCDA files missing'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        call auto_process_gcov_files('.', config, result)
        
        call assert_false(result%success, 'Missing gcda handled')
        call assert_true(index(result%guidance_message, 'test') > 0, &
                        'Guidance mentions running tests')
        call assert_true(index(result%guidance_message, 'coverage') > 0, &
                        'Guidance mentions coverage flags')
    end subroutine test_gcda_files_missing

    subroutine test_invalid_build_directory()
        !! Given invalid/inaccessible build directory
        !! When auto-discovery functions are called
        !! Then they should handle errors gracefully
        type(config_t) :: config
        type(test_build_result_t) :: result
        
        write(output_unit, '(A)') 'Test 17: Invalid build directory'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        ! Try to discover in non-existent directory
        call auto_discover_test_build('/invalid/path/does/not/exist', &
                                     config, result)
        
        call assert_false(result%success, 'Invalid path handled')
        call assert_true(len_trim(result%error_message) > 0, 'Error reported')
    end subroutine test_invalid_build_directory

    subroutine test_gcov_processing_failure()
        !! Given gcov command fails
        !! When auto_process_gcov_files is called
        !! Then it should handle gcov failure gracefully
        type(config_t) :: config
        type(gcov_result_t) :: result
        
        write(output_unit, '(A)') 'Test 18: Gcov processing failure'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        ! Mock invalid gcov executable
        config%gcov_executable = '/invalid/gcov/path'
        
        call create_mock_gcda_files()
        
        call auto_process_gcov_files('.', config, result)
        
        call assert_false(result%success, 'Gcov failure handled')
        call assert_true(index(result%error_message, 'gcov') > 0, &
                        'Error mentions gcov')
        
        call cleanup_mock_gcov_files()
    end subroutine test_gcov_processing_failure

    subroutine test_source_mapping_discovery()
        !! Given .gcov files with source references
        !! When auto_process_gcov_files is called
        !! Then it should discover source file mappings
        type(config_t) :: config
        type(gcov_result_t) :: result
        
        write(output_unit, '(A)') 'Test 19: Source mapping discovery'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        call create_mock_gcov_with_sources()
        
        call auto_process_gcov_files('.', config, result)
        
        call assert_true(result%success, 'Source mapping succeeded')
        call assert_true(size(result%source_mappings) > 0, &
                        'Source mappings found')
        call assert_true(len_trim(result%source_mappings(1)%source_file) > 0, &
                        'Source file mapped')
        call assert_true(len_trim(result%source_mappings(1)%gcov_file) > 0, &
                        'Gcov file mapped')
        
        call cleanup_mock_gcov_with_sources()
    end subroutine test_source_mapping_discovery

    ! Mock creation and cleanup subroutines
    ! (These would create temporary test files/directories)
    
    subroutine create_mock_fpm_project()
        call execute_command_line('touch fpm.toml')
    end subroutine create_mock_fpm_project

    subroutine create_mock_cmake_project()
        call execute_command_line('touch CMakeLists.txt')
    end subroutine create_mock_cmake_project

    subroutine create_mock_make_project()
        call execute_command_line('touch Makefile')
    end subroutine create_mock_make_project

    subroutine create_mock_meson_project()
        call execute_command_line('touch meson.build')
    end subroutine create_mock_meson_project

    subroutine create_mock_gcda_files()
        call execute_command_line('mkdir -p build/test && touch build/test/test.gcda')
    end subroutine create_mock_gcda_files

    subroutine create_mock_build_structure()
        call execute_command_line('mkdir -p build/debug/src')
    end subroutine create_mock_build_structure

    subroutine create_mock_gcda_files_in_build()
        call execute_command_line('touch build/debug/src/main.gcda')
    end subroutine create_mock_gcda_files_in_build

    subroutine create_mock_complete_project()
        call create_mock_fpm_project()
        call create_mock_gcda_files()
    end subroutine create_mock_complete_project

    subroutine create_mock_existing_gcov_files()
        call execute_command_line('touch existing.gcov')
    end subroutine create_mock_existing_gcov_files

    subroutine create_mock_failing_project()
        call create_mock_fpm_project()
        ! Would set up test that fails
    end subroutine create_mock_failing_project

    subroutine create_mock_slow_project()
        call create_mock_fpm_project()
        ! Would set up slow test
    end subroutine create_mock_slow_project

    subroutine create_mock_gcov_with_sources()
        call execute_command_line('mkdir -p build/test')
        call execute_command_line('echo "        -:    1:module test" > build/test/test.gcov')
        call execute_command_line('echo "        -:    2:contains" >> build/test/test.gcov')
    end subroutine create_mock_gcov_with_sources

    subroutine cleanup_mock_project()
        call execute_command_line('rm -f fpm.toml CMakeLists.txt Makefile meson.build')
    end subroutine cleanup_mock_project

    subroutine cleanup_mock_gcov_files()
        call execute_command_line('rm -rf build')
    end subroutine cleanup_mock_gcov_files

    subroutine cleanup_mock_build_structure()
        call execute_command_line('rm -rf build')
    end subroutine cleanup_mock_build_structure

    subroutine cleanup_mock_complete_project()
        call cleanup_mock_project()
        call cleanup_mock_gcov_files()
    end subroutine cleanup_mock_complete_project

    subroutine cleanup_mock_existing_gcov_files()
        call execute_command_line('rm -f existing.gcov')
    end subroutine cleanup_mock_existing_gcov_files

    subroutine cleanup_mock_gcov_with_sources()
        call cleanup_mock_gcov_files()
    end subroutine cleanup_mock_gcov_with_sources

    ! Test assertion helpers

    subroutine assert_true(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        
        total_tests = total_tests + 1
        if (condition) then
            passed_tests = passed_tests + 1
            write(output_unit, '(A,A)') '  ✓ ', description
        else
            all_tests_passed = .false.
            write(error_unit, '(A,A)') '  ✗ ', description
        end if
    end subroutine assert_true

    subroutine assert_false(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        
        call assert_true(.not. condition, description)
    end subroutine assert_false

    subroutine assert_equals_string(actual, expected, description)
        character(len=*), intent(in) :: actual, expected, description
        
        call assert_true(trim(actual) == trim(expected), &
                        description // ' (expected: ' // trim(expected) // &
                        ', actual: ' // trim(actual) // ')')
    end subroutine assert_equals_string

    subroutine assert_equals_int(actual, expected, description)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: description
        character(len=32) :: actual_str, expected_str
        
        write(actual_str, '(I0)') actual
        write(expected_str, '(I0)') expected
        
        call assert_true(actual == expected, &
                        description // ' (expected: ' // trim(expected_str) // &
                        ', actual: ' // trim(actual_str) // ')')
    end subroutine assert_equals_int

end program test_test_build_gcov_auto_discovery