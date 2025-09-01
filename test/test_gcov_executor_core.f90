program test_gcov_executor_core
    !! Focused core tests for gcov_executor module

    use iso_fortran_env, only: output_unit, error_unit
    use gcov_executor, only: gcov_executor_t
    use error_handling_core, only: error_context_t, ERROR_SUCCESS
    use file_ops_secure, only: safe_mkdir, safe_remove_file, safe_remove_directory
    use portable_temp_utils, only: get_temp_dir
    implicit none

    integer :: test_count = 0
    integer :: passed_count = 0
    logical :: all_tests_passed = .true.
    character(len=:), allocatable :: test_workspace

    write(output_unit, '(A)') 'Running gcov_executor core tests...'
    write(output_unit, '(A)') ''

    block
        character(len=:), allocatable :: temp_base
        temp_base = get_temp_dir()
        test_workspace = trim(temp_base) // '/fortcov_tests/gcov_executor/core'
    end block
    call setup_test_environment()

    ! Core functionality tests
    call test_gcov_executor_initialization()
    call test_execute_gcov_success_path()
    call test_execute_gcov_missing_source()
    call test_execute_gcov_missing_gcda()
    call test_execute_gcov_no_output()

    ! Configuration method tests
    call test_set_branch_coverage()
    call test_set_working_directory()
    call test_set_gcov_command()
    call test_set_gcov_output_directory()

    call cleanup_test_environment()

    write(output_unit, '(A)') ''
    write(output_unit, '(A,I0,A,I0,A)') 'Test Results: ', passed_count, ' / ', &
                                        test_count, ' tests passed'

    if (passed_count /= test_count) then
        write(error_unit, '(A)') 'Some gcov_executor core tests failed!'
        all_tests_passed = .false.
        stop 1
    else
        write(output_unit, '(A)') 'All gcov_executor core tests passed!'
    end if

contains

    subroutine setup_test_environment()
        type(error_context_t) :: error_ctx
        call safe_mkdir(test_workspace, error_ctx)
    end subroutine setup_test_environment

    subroutine cleanup_test_environment()
        type(error_context_t) :: error_ctx
        call safe_remove_directory(test_workspace, error_ctx)
    end subroutine cleanup_test_environment

    subroutine test_gcov_executor_initialization()
        call run_test('gcov_executor_t initialization', test_executor_default_initialization)
    end subroutine test_gcov_executor_initialization

    logical function test_executor_default_initialization() result(success)
        type(gcov_executor_t) :: executor
        call executor%set_branch_coverage(.false.)
        call executor%set_gcov_command('gcov')
        success = .true.
    end function test_executor_default_initialization

    subroutine test_execute_gcov_success_path()
        call run_test('execute_gcov with valid inputs', test_execute_gcov_valid_inputs)
    end subroutine test_execute_gcov_success_path

    logical function test_execute_gcov_valid_inputs() result(success)
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx, temp_error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=256) :: source_file, gcda_file, output_dir
        integer :: unit, iostat

        success = .false.
        source_file = trim(test_workspace) // '/test_source.f90'
        gcda_file   = 'test_source.gcda'
        output_dir  = trim(test_workspace) // '/gcov_output'

        call safe_mkdir(output_dir, temp_error_ctx)
        call executor%set_gcov_output_directory(output_dir)

        open(newunit=unit, file=source_file, status='replace', iostat=iostat); if (iostat /= 0) return
        write(unit, '(A)') 'program test'
        write(unit, '(A)') 'end program'
        close(unit)

        open(newunit=unit, file=gcda_file, status='replace', iostat=iostat); if (iostat /= 0) return
        write(unit, '(A)') 'mock gcda data'
        close(unit)

        call executor%execute_gcov(source_file, gcov_files, error_ctx)

        success = (error_ctx%error_code /= ERROR_SUCCESS) .or. &
                  (allocated(gcov_files) .and. size(gcov_files) == 0)

        call safe_remove_file(source_file, error_ctx)
        call safe_remove_file(gcda_file,   error_ctx)
        call safe_remove_directory(output_dir, error_ctx)
    end function test_execute_gcov_valid_inputs

    subroutine test_execute_gcov_missing_source()
        call run_test('execute_gcov with missing source file', test_execute_gcov_no_source)
    end subroutine test_execute_gcov_missing_source

    logical function test_execute_gcov_no_source() result(success)
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=256) :: nonexistent_file

        nonexistent_file = trim(test_workspace) // '/nonexistent.f90'
        call executor%execute_gcov(nonexistent_file, gcov_files, error_ctx)
        success = (error_ctx%error_code /= ERROR_SUCCESS)
    end function test_execute_gcov_no_source

    subroutine test_execute_gcov_missing_gcda()
        call run_test('execute_gcov with missing gcda file', test_execute_gcov_no_gcda)
    end subroutine test_execute_gcov_missing_gcda

    logical function test_execute_gcov_no_gcda() result(success)
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=256) :: source_file
        integer :: unit, iostat

        success = .false.
        source_file = trim(test_workspace) // '/source_only.f90'
        open(newunit=unit, file=source_file, status='replace', iostat=iostat); if (iostat /= 0) return
        write(unit, '(A)') 'program test'
        close(unit)

        call executor%execute_gcov(source_file, gcov_files, error_ctx)
        success = (error_ctx%error_code /= ERROR_SUCCESS)

        call safe_remove_file(source_file, error_ctx)
    end function test_execute_gcov_no_gcda

    subroutine test_execute_gcov_no_output()
        call run_test('execute_gcov with no gcov output generated', test_execute_gcov_empty_result)
    end subroutine test_execute_gcov_no_output

    logical function test_execute_gcov_empty_result() result(success)
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx, temp_error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=256) :: source_file, gcda_file, output_dir
        integer :: unit, iostat

        success = .false.
        source_file = trim(test_workspace) // '/empty_result.f90'
        gcda_file   = 'empty_result.gcda'
        output_dir  = trim(test_workspace) // '/gcov_output2'

        call safe_mkdir(output_dir, temp_error_ctx)
        call executor%set_gcov_output_directory(output_dir)

        open(newunit=unit, file=source_file, status='replace', iostat=iostat); if (iostat /= 0) return
        write(unit, '(A)') 'program test'
        close(unit)

        open(newunit=unit, file=gcda_file, status='replace', iostat=iostat); if (iostat /= 0) return
        write(unit, '(A)') 'mock data'
        close(unit)

        call executor%execute_gcov(source_file, gcov_files, error_ctx)

        success = (error_ctx%error_code /= ERROR_SUCCESS) .or. &
                  (allocated(gcov_files) .and. size(gcov_files) == 0)

        call safe_remove_file(source_file, error_ctx)
        call safe_remove_file(gcda_file,   error_ctx)
        call safe_remove_directory(output_dir, error_ctx)
    end function test_execute_gcov_empty_result

    subroutine test_set_branch_coverage()
        call run_test('set_branch_coverage configuration', test_branch_coverage_setting)
    end subroutine test_set_branch_coverage

    logical function test_branch_coverage_setting() result(success)
        type(gcov_executor_t) :: executor
        call executor%set_branch_coverage(.true.)
        call executor%set_branch_coverage(.false.)
        success = .true.
    end function test_branch_coverage_setting

    subroutine test_set_working_directory()
        call run_test('set_working_directory configuration', test_working_directory_setting)
    end subroutine test_set_working_directory

    logical function test_working_directory_setting() result(success)
        type(gcov_executor_t) :: executor
        call executor%set_working_directory('/tmp/test')
        call executor%set_working_directory('')
        call executor%set_working_directory(test_workspace)
        success = .true.
    end function test_working_directory_setting

    subroutine test_set_gcov_command()
        call run_test('set_gcov_command configuration', test_gcov_command_setting)
    end subroutine test_set_gcov_command

    logical function test_gcov_command_setting() result(success)
        type(gcov_executor_t) :: executor
        call executor%set_gcov_command('gcov')
        call executor%set_gcov_command('/usr/bin/gcov')
        call executor%set_gcov_command('custom-gcov')
        success = .true.
    end function test_gcov_command_setting

    subroutine test_set_gcov_output_directory()
        call run_test('set_gcov_output_directory configuration', test_output_directory_setting)
    end subroutine test_set_gcov_output_directory

    logical function test_output_directory_setting() result(success)
        type(gcov_executor_t) :: executor
        call executor%set_gcov_output_directory('build/coverage')
        call executor%set_gcov_output_directory(trim(test_workspace) // '/output')
        call executor%set_gcov_output_directory('')
        success = .true.
    end function test_output_directory_setting

    subroutine run_test(test_name, test_function)
        character(len=*), intent(in) :: test_name
        interface
            logical function test_function()
            end function test_function
        end interface
        logical :: result
        test_count = test_count + 1
        result = test_function()
        if (result) then
            passed_count = passed_count + 1
            write(output_unit, '(A,A)') '  			✓ ', test_name
        else
            write(output_unit, '(A,A)') '  ✗ ', test_name
            all_tests_passed = .false.
        end if
    end subroutine run_test

end program test_gcov_executor_core
