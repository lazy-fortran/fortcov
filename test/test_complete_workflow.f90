program test_complete_workflow
    !! Complete Auto-Discovery Workflow Tests (Issue #277 - Part 3)
    !!
    !! Tests for the auto_discovery_utilities module focusing on
    !! complete workflow orchestration and integration.

    use auto_discovery_utilities
    use config_types, only: config_t
    use iso_fortran_env, only: output_unit
    implicit none

    integer :: test_count = 0
    integer :: passed_count = 0

    write(output_unit, '(A)') 'Running complete workflow tests...'
    write(output_unit, '(A)') ''

    call test_complete_auto_workflow_success()
    call test_complete_auto_workflow_no_build_system()
    call test_complete_auto_workflow_test_failure()
    call test_complete_auto_workflow_timeout()

    write(output_unit, '(A)') ''
    write(output_unit, '(A,I0,A,I0,A)') 'Test Results: ', passed_count, ' / ', &
                                        test_count, ' tests passed'
    
    if (passed_count /= test_count) then
        write(output_unit, '(A)') 'Some tests failed!'
        stop 1
    end if

contains

    subroutine test_complete_auto_workflow_success()
        !! Given a complete project with build system and tests
        !! When execute_complete_auto_workflow is called
        !! Then it should succeed end-to-end
        type(config_t) :: config
        type(complete_workflow_result_t) :: result
        
        write(output_unit, '(A)') 'Test 9: Complete auto workflow success'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        config%auto_test_execution = .false.  ! Skip test execution to avoid recursion
        
        call create_mock_complete_project()
        
        call execute_complete_auto_workflow(config, result)
        
        call assert_true(result%success, 'Complete workflow succeeded')
        call assert_true(result%auto_discovery_used, 'Auto-discovery was used')
        ! Since auto_test_execution is disabled, tests should not execute
        call assert_false(result%test_executed, 'Tests skipped to avoid recursion')
        call assert_true(result%gcov_processed, 'Gcov was processed')
        call assert_true(result%coverage_generated, 'Coverage was generated')
        
        call cleanup_mock_complete_project()
    end subroutine test_complete_auto_workflow_success

    subroutine test_complete_auto_workflow_no_build_system()
        !! Given no build system detected
        !! When execute_complete_auto_workflow is called
        !! Then it should handle gracefully
        type(config_t) :: config
        type(complete_workflow_result_t) :: result
        
        write(output_unit, '(A)') 'Test 10: Complete auto workflow - no build system'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        ! No mock build files created
        
        call execute_complete_auto_workflow(config, result)
        
        call assert_false(result%success, 'Workflow failed appropriately')
        call assert_true(result%auto_discovery_used, 'Auto-discovery was attempted')
        call assert_true(len_trim(result%error_message) > 0, 'Error message provided')
        
    end subroutine test_complete_auto_workflow_no_build_system

    subroutine test_complete_auto_workflow_test_failure()
        !! Given tests that fail
        !! When execute_complete_auto_workflow is called
        !! Then it should report test failure
        type(config_t) :: config
        type(complete_workflow_result_t) :: result
        
        write(output_unit, '(A)') 'Test 11: Complete auto workflow - test failure'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        config%auto_test_execution = .true.
        
        call create_mock_fpm_project()
        call create_mock_failing_tests()
        
        call execute_complete_auto_workflow(config, result)
        
        call assert_false(result%success, 'Workflow reported failure')
        call assert_true(result%test_executed, 'Tests were executed')
        call assert_false(result%tests_passed, 'Tests failed as expected')
        call assert_true(index(result%error_message, 'fail') > 0, &
                        'Error message mentions failure')
        
        call cleanup_mock_failing_tests()
        call cleanup_mock_project()
    end subroutine test_complete_auto_workflow_test_failure

    subroutine test_complete_auto_workflow_timeout()
        !! Given tests that timeout
        !! When execute_complete_auto_workflow is called
        !! Then it should handle timeout gracefully
        type(config_t) :: config
        type(complete_workflow_result_t) :: result
        
        write(output_unit, '(A)') 'Test 12: Complete auto workflow - timeout'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        config%auto_test_execution = .true.
        config%test_timeout_seconds = 1  ! Very short timeout
        
        call create_mock_fpm_project()
        call create_mock_slow_tests()
        
        call execute_complete_auto_workflow(config, result)
        
        call assert_false(result%success, 'Workflow handled timeout')
        call assert_true(result%test_executed, 'Tests were started')
        call assert_true(result%timed_out, 'Timeout was detected')
        
        call cleanup_mock_slow_tests()
        call cleanup_mock_project()
    end subroutine test_complete_auto_workflow_timeout

    ! Mock creation and cleanup subroutines
    
    subroutine create_mock_fpm_project()
        call execute_command_line('mkdir -p test_temp_dir')
        call execute_command_line('touch test_temp_dir/fpm.toml')
    end subroutine create_mock_fpm_project

    subroutine create_mock_complete_project()
        call create_mock_fpm_project()
        call create_mock_gcda_files()
    end subroutine create_mock_complete_project

    subroutine create_mock_gcda_files()
        call execute_command_line('mkdir -p test_temp_dir/build/test')
        call execute_command_line('touch test_temp_dir/build/test/test.gcda')
    end subroutine create_mock_gcda_files

    subroutine create_mock_failing_tests()
        ! Create mock test that will fail
        call execute_command_line('echo "exit 1" > mock_failing_test.sh')
        call execute_command_line('chmod +x mock_failing_test.sh')
    end subroutine create_mock_failing_tests

    subroutine create_mock_slow_tests()
        ! Create mock test that will timeout
        call execute_command_line('echo "sleep 10" > mock_slow_test.sh')
        call execute_command_line('chmod +x mock_slow_test.sh')
    end subroutine create_mock_slow_tests

    subroutine cleanup_mock_project()
        call execute_command_line('rm -rf test_temp_dir')
    end subroutine cleanup_mock_project

    subroutine cleanup_mock_complete_project()
        call cleanup_mock_project()
        call cleanup_mock_gcov_files()
    end subroutine cleanup_mock_complete_project

    subroutine cleanup_mock_gcov_files()
        call execute_command_line('rm -rf test_temp_dir/build')
    end subroutine cleanup_mock_gcov_files

    subroutine cleanup_mock_failing_tests()
        call execute_command_line('rm -f mock_failing_test.sh')
    end subroutine cleanup_mock_failing_tests

    subroutine cleanup_mock_slow_tests()
        call execute_command_line('rm -f mock_slow_test.sh')
    end subroutine cleanup_mock_slow_tests

    ! Test assertion helpers
    
    subroutine assert_true(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        
        test_count = test_count + 1
        if (condition) then
            passed_count = passed_count + 1
            write(output_unit, '(A,A)') '  ✓ ', description
        else
            write(output_unit, '(A,A)') '  ✗ ', description
        end if
    end subroutine assert_true

    subroutine assert_false(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        
        call assert_true(.not. condition, description)
    end subroutine assert_false

    subroutine initialize_default_config(config)
        !! Initialize config with default values
        type(config_t), intent(out) :: config
        
        config%auto_discovery = .false.
        config%auto_test_execution = .true.
        config%test_timeout_seconds = 30
        config%verbose = .false.
        config%gcov_executable = 'gcov'
    end subroutine initialize_default_config

end program test_complete_workflow