program test_auto_test_integration
    !! Integration test for auto-test execution feature
    !!
    !! Creates a real test scenario with an FPM project structure
    !! and demonstrates the auto-test execution functionality
    use iso_fortran_env, only: error_unit, output_unit
    use coverage_workflows, only: execute_auto_test_workflow
    use config_core, only: config_t
    use config_defaults_core, only: initialize_default_config
    implicit none

    integer :: total_tests = 0
    integer :: passed_tests = 0
    logical :: all_tests_passed = .true.
    character(len=256) :: current_dir

    write(output_unit, '(A)') 'Running auto-test integration tests...'
    write(output_unit, *)

    ! Save current directory
    call getcwd(current_dir)

    call test_auto_test_with_real_fpm_project()
    call test_auto_test_with_no_build_system()
    call test_auto_test_disabled_integration()

    ! Restore directory and cleanup
    call chdir(current_dir)
    call cleanup_test_files()

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

    subroutine test_auto_test_with_real_fpm_project()
        !! Test auto-test execution with real FPM project structure
        type(config_t) :: config
        integer :: result_code
        
        write(output_unit, '(A)') 'Test 1: Auto-test with real FPM project'
        
        ! Setup a temporary FPM project
        call setup_fpm_test_project()
        
        ! Configure for auto-test execution
        call initialize_default_config(config)
        config%auto_test_execution = .true.
        config%test_timeout_seconds = 30  ! Short timeout for testing
        config%quiet = .false.  ! Show output for debugging
        
        ! Execute auto-test workflow
        result_code = execute_auto_test_workflow(config)
        
        ! Result should be 0 (skip due to no FPM tool) or test execution result
        call assert_result_acceptable(result_code, &
                                     'FPM project auto-test handling')
        
        ! Cleanup
        call chdir(current_dir)
    end subroutine test_auto_test_with_real_fpm_project

    subroutine test_auto_test_with_no_build_system()
        !! Test auto-test execution with no build system
        type(config_t) :: config
        integer :: result_code
        
        write(output_unit, '(A)') 'Test 2: Auto-test with no build system'
        
        ! Setup empty directory
        call setup_empty_test_directory()
        
        call initialize_default_config(config)
        config%auto_test_execution = .true.
        config%quiet = .false.
        
        result_code = execute_auto_test_workflow(config)
        
        ! Should return 0 (graceful skip)
        call assert_equals_int(result_code, 0, &
                              'No build system handling')
        
        call chdir(current_dir)
    end subroutine test_auto_test_with_no_build_system

    subroutine test_auto_test_disabled_integration()
        !! Test that disabled auto-test execution works correctly
        type(config_t) :: config
        integer :: result_code
        
        write(output_unit, '(A)') 'Test 3: Disabled auto-test integration'
        
        call initialize_default_config(config)
        config%auto_test_execution = .false.  ! Disabled
        config%quiet = .false.
        
        result_code = execute_auto_test_workflow(config)
        
        call assert_equals_int(result_code, 0, &
                              'Disabled auto-test execution')
        
    end subroutine test_auto_test_disabled_integration

    ! Test utilities
    subroutine setup_fpm_test_project()
        !! Create a simple FPM project structure for testing
        integer :: unit, stat
        
        call execute_command_line('mkdir -p test_auto_fpm', wait=.true.)
        call chdir('test_auto_fpm')
        
        ! Create fpm.toml
        open(newunit=unit, file='fpm.toml', status='replace', iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') 'name = "test_project"'
            write(unit, '(A)') 'version = "0.1.0"'
            write(unit, '(A)') 'license = "MIT"'
            write(unit, '(A)') 'author = "Test Author"'
            write(unit, '(A)') ''
            write(unit, '(A)') '[build]'
            write(unit, '(A)') 'auto-executables = true'
            write(unit, '(A)') 'auto-tests = true'
            close(unit)
        end if
        
        ! Create a simple src file
        call execute_command_line('mkdir -p src', wait=.true.)
        open(newunit=unit, file='src/test_module.f90', status='replace', &
             iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') 'module test_module'
            write(unit, '(A)') '    implicit none'
            write(unit, '(A)') 'contains'
            write(unit, '(A)') '    function add_numbers(a, b) result(sum)'
            write(unit, '(A)') '        integer, intent(in) :: a, b'
            write(unit, '(A)') '        integer :: sum'
            write(unit, '(A)') '        sum = a + b'
            write(unit, '(A)') '    end function add_numbers'
            write(unit, '(A)') 'end module test_module'
            close(unit)
        end if
        
        ! Create a simple test
        call execute_command_line('mkdir -p test', wait=.true.)
        open(newunit=unit, file='test/test_basic.f90', status='replace', &
             iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') 'program test_basic'
            write(unit, '(A)') '    use test_module'
            write(unit, '(A)') '    implicit none'
            write(unit, '(A)') '    integer :: result'
            write(unit, '(A)') '    result = add_numbers(2, 3)'
            write(unit, '(A)') '    if (result == 5) then'
            write(unit, '(A)') '        print *, "Test passed"'
            write(unit, '(A)') '        stop 0'
            write(unit, '(A)') '    else'
            write(unit, '(A)') '        print *, "Test failed"'
            write(unit, '(A)') '        stop 1'
            write(unit, '(A)') '    end if'
            write(unit, '(A)') 'end program test_basic'
            close(unit)
        end if
    end subroutine setup_fpm_test_project

    subroutine setup_empty_test_directory()
        !! Create empty test directory
        call execute_command_line('mkdir -p test_empty', wait=.true.)
        call chdir('test_empty')
    end subroutine setup_empty_test_directory

    subroutine cleanup_test_files()
        !! Clean up test files and directories
        call execute_command_line( &
            'rm -rf test_auto_fpm test_empty', wait=.true.)
    end subroutine cleanup_test_files

    ! Assertion utilities
    subroutine assert_equals_int(actual, expected, message)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: message
        
        total_tests = total_tests + 1
        if (actual == expected) then
            passed_tests = passed_tests + 1
            write(output_unit, '(A,A)') '  PASS: ', message
        else
            all_tests_passed = .false.
            write(error_unit, '(A,A)') '  FAIL: ', message
            write(error_unit, '(A,I0,A,I0)') '    Expected: ', expected, &
                                             ', Got: ', actual
        end if
    end subroutine assert_equals_int

    subroutine assert_result_acceptable(result, message)
        !! Assert that result is acceptable (0 or expected failure)
        integer, intent(in) :: result
        character(len=*), intent(in) :: message
        
        total_tests = total_tests + 1
        ! Accept 0 (success/skip) or other expected codes
        if (result == 0 .or. result == 2) then
            passed_tests = passed_tests + 1
            write(output_unit, '(A,A,A,I0,A)') '  PASS: ', message, &
                                               ' (exit code: ', result, ')'
        else
            all_tests_passed = .false.
            write(error_unit, '(A,A)') '  FAIL: ', message
            write(error_unit, '(A,I0)') '    Unexpected exit code: ', result
        end if
    end subroutine assert_result_acceptable

end program test_auto_test_integration