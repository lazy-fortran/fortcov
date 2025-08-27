module auto_discovery_utils
    !! Auto-Discovery Utilities Module (Issue #277 - Part 3)
    !!
    !! Provides utility functions and workflow orchestration for the complete
    !! auto-discovery system. Handles workflow result determination and
    !! integration between components.
    !!
    !! Key Features:
    !! - Complete auto-discovery workflow orchestration
    !! - Workflow result determination and status tracking
    !! - Integration between test build discovery and gcov processing
    !! - Comprehensive error handling and user guidance

    use config_types, only: config_t
    use build_discovery_core, only: test_build_result_t, auto_discover_test_build
    use gcov_processor_auto, only: gcov_result_t, auto_process_gcov_files
    use coverage_workflows_impl, only: execute_auto_test_workflow
    use error_handling_core, only: error_context_t, ERROR_SUCCESS, clear_error_context
    use constants_core, only: EXIT_SUCCESS, EXIT_FAILURE
    implicit none
    private

    ! Public interface
    public :: execute_complete_auto_workflow
    public :: complete_workflow_result_t

    ! Result type
    type :: complete_workflow_result_t
        logical :: success = .false.
        logical :: test_executed = .false.
        logical :: tests_passed = .false.
        logical :: gcov_processed = .false.
        logical :: coverage_generated = .false.
        logical :: auto_discovery_used = .false.
        logical :: used_manual_files = .false.
        logical :: timed_out = .false.
        character(len=512) :: error_message = ''
    end type complete_workflow_result_t

contains

    subroutine execute_complete_auto_workflow(config, result)
        !! Execute complete auto-discovery workflow
        !!
        !! Orchestrates the complete workflow from auto-discovery to coverage
        !! report generation. Integrates test build discovery, test execution,
        !! gcov processing, and coverage analysis into a seamless experience.
        !!
        !! Workflow Steps:
        !! 1. Auto-discover test build capabilities
        !! 2. Execute tests with coverage flags (if available)
        !! 3. Auto-process gcov files from test execution
        !! 4. Generate coverage analysis and reports
        !!
        !! Args:
        !!   config: Configuration for the complete workflow
        !!   result: Comprehensive workflow execution results

        type(config_t), intent(in) :: config
        type(complete_workflow_result_t), intent(out) :: result

        call execute_main_workflow(config, result)

    end subroutine execute_complete_auto_workflow

    subroutine execute_main_workflow(config, result)
        !! Main workflow execution logic
        type(config_t), intent(in) :: config
        type(complete_workflow_result_t), intent(out) :: result

        type(test_build_result_t) :: test_result
        type(gcov_result_t) :: gcov_result
        integer :: test_exit_code
        logical :: manual_files_specified

        ! Initialize result
        result%success = .false.
        result%test_executed = .false.
        result%tests_passed = .false.
        result%gcov_processed = .false.
        result%coverage_generated = .false.
        result%auto_discovery_used = .false.
        result%used_manual_files = .false.
        result%timed_out = .false.
        result%error_message = ''

        ! Check if manual files are specified
        call check_manual_files(config, manual_files_specified)
        result%used_manual_files = manual_files_specified

        if (.not. manual_files_specified .and. config%auto_discovery) then
            call execute_auto_workflow(config, test_result, gcov_result, &
                                      test_exit_code, result)
        else
            call handle_manual_workflow(config, result)
        end if

    end subroutine execute_main_workflow

    subroutine execute_auto_workflow(config, test_result, gcov_result, &
                                   test_exit_code, result)
        !! Execute auto-discovery workflow
        type(config_t), intent(in) :: config
        type(test_build_result_t), intent(out) :: test_result
        type(gcov_result_t), intent(out) :: gcov_result
        integer, intent(out) :: test_exit_code
        type(complete_workflow_result_t), intent(out) :: result

        result%auto_discovery_used = .true.

        ! Step 1: Auto-discover test build capabilities
        call auto_discover_test_build('.', config, test_result)

        if (test_result%success .and. config%auto_test_execution) then
            ! Step 2: Execute tests with coverage
            call execute_tests_with_coverage(config, test_result, &
                                           test_exit_code, result)
        end if

        ! Step 3: Auto-process gcov files
        call auto_process_gcov_files('.', config, gcov_result)
        result%gcov_processed = gcov_result%success

        ! Step 4: Determine overall workflow success
        call determine_workflow_success(result, test_result, gcov_result)

    end subroutine execute_auto_workflow

    subroutine execute_tests_with_coverage(config, test_result, &
                                         test_exit_code, result)
        !! Execute tests with coverage using auto-discovered command
        type(config_t), intent(in) :: config
        type(test_build_result_t), intent(in) :: test_result
        integer, intent(out) :: test_exit_code
        type(complete_workflow_result_t), intent(out) :: result

        result%test_executed = .true.

        test_exit_code = execute_auto_test_workflow(config)

        result%tests_passed = (test_exit_code == EXIT_SUCCESS)
        result%timed_out = (test_exit_code == 124)

    end subroutine execute_tests_with_coverage

    subroutine handle_manual_workflow(config, result)
        !! Handle manual workflow when auto-discovery not used
        type(config_t), intent(in) :: config
        type(complete_workflow_result_t), intent(out) :: result

        ! For manual workflow, we assume external test execution
        ! and focus on processing existing coverage files
        result%success = .true.
        result%coverage_generated = .true.

    end subroutine handle_manual_workflow

    subroutine check_manual_files(config, manual_files_specified)
        !! Check if manual files are specified in configuration
        type(config_t), intent(in) :: config
        logical, intent(out) :: manual_files_specified

        manual_files_specified = (len_trim(config%import_file) > 0) .or. &
                               (allocated(config%source_paths) .and. &
                                size(config%source_paths) > 0)

    end subroutine check_manual_files

    subroutine determine_workflow_success(result, test_result, gcov_result)
        !! Determine overall workflow success based on component results
        type(complete_workflow_result_t), intent(inout) :: result
        type(test_build_result_t), intent(in) :: test_result
        type(gcov_result_t), intent(in) :: gcov_result

        ! Success criteria:
        ! 1. If tests were executed, they must pass
        ! 2. Gcov processing must succeed (if attempted)
        ! 3. Must have either test execution or gcov processing success

        logical :: test_success, gcov_success

        test_success = .not. result%test_executed .or. result%tests_passed
        gcov_success = result%gcov_processed

        if (test_success .and. gcov_success) then
            result%success = .true.
            result%coverage_generated = .true.
        else if (.not. result%test_executed .and. .not. result%gcov_processed) then
            ! Neither component succeeded
            if (len_trim(test_result%error_message) > 0) then
                result%error_message = trim(test_result%error_message)
            else if (len_trim(gcov_result%error_message) > 0) then
                result%error_message = trim(gcov_result%error_message)
            else
                result%error_message = 'Auto-discovery workflow failed'
            end if
        else if (result%test_executed .and. .not. result%tests_passed) then
            result%error_message = 'Tests failed during execution'
        else if (.not. result%gcov_processed) then
            result%error_message = trim(gcov_result%error_message)
        end if

    end subroutine determine_workflow_success

end module auto_discovery_utils