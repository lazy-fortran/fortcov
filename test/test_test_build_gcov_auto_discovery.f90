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
    !!
    !! DECOMPOSED: Test logic extracted to focused modules for architecture 
    !! size compliance (<500 lines per file)
    
    use iso_fortran_env, only: output_unit
    use test_framework_utilities
    use test_auto_discovery_mocks
    use test_build_system_discovery
    use test_gcov_auto_processing
    use test_auto_discovery_workflows
    implicit none

    type(test_counter_t) :: test_counter

    write(output_unit, '(A)') 'Running test build and gcov auto-discovery tests...'
    write(output_unit, *)

    ! Initialize test framework
    call init_test_counter(test_counter)

    ! Test auto-discovery of test build capabilities
    call test_auto_discover_test_build_with_fpm(test_counter)
    call test_auto_discover_test_build_with_cmake(test_counter)
    call test_auto_discover_test_build_with_make(test_counter)
    call test_auto_discover_test_build_with_meson(test_counter)
    call test_auto_discover_test_build_unknown_system(test_counter)

    ! Test auto-processing of gcov files
    call test_auto_process_gcov_files_found(test_counter)
    call test_auto_process_gcov_files_not_found(test_counter)
    call test_auto_process_gcov_files_build_context(test_counter)

    ! Test complete auto-discovery workflow
    call test_complete_auto_workflow_success(test_counter)
    call test_complete_auto_workflow_no_build_system(test_counter)
    call test_complete_auto_workflow_test_failure(test_counter)
    call test_complete_auto_workflow_timeout(test_counter)

    ! Test configuration control and backward compatibility
    call test_auto_discovery_disabled(test_counter)
    call test_manual_override_auto_discovered(test_counter)
    call test_backward_compatibility_explicit_mode(test_counter)

    ! Test edge cases and error conditions
    call test_gcda_files_missing(test_counter)
    call test_invalid_build_directory(test_counter)
    call test_gcov_processing_failure(test_counter)
    call test_source_mapping_discovery(test_counter)

    ! Report results
    call print_test_summary(test_counter, "Test Build and Gcov Auto-Discovery")

end program test_test_build_gcov_auto_discovery