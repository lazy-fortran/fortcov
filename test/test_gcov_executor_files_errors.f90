program test_gcov_executor_files_errors
    !! File management and error handling tests for gcov_executor

    use iso_fortran_env, only: output_unit, error_unit
    use gcov_executor, only: gcov_executor_t
    use error_handling_core, only: error_context_t, ERROR_PERMISSION_DENIED
    use file_ops_secure, only: safe_mkdir, safe_remove_file, safe_remove_directory
    implicit none

    integer :: test_count = 0
    integer :: passed_count = 0
    logical :: all_tests_passed = .true.
    character(len=:), allocatable :: test_workspace

    write(output_unit, '(A)') 'Running gcov_executor file/error tests...'
    write(output_unit, '(A)') ''

    test_workspace = './gcov_executor_test/files'
    call setup_test_environment()

    ! File management tests
    call test_cleanup_gcov_files()
    call test_cleanup_gcov_files_nonexistent()

    ! Edge case and error handling tests
    call test_memory_allocation_failures()
    call test_output_directory_creation()
    call test_file_operations_errors()

    call cleanup_test_environment()

    write(output_unit, '(A)') ''
    write(output_unit, '(A,I0,A,I0,A)') 'Test Results: ', passed_count, ' / ', &
                                        test_count, ' tests passed'

    if (passed_count /= test_count) then
        write(error_unit, '(A)') 'Some gcov_executor file/error tests failed!'
        all_tests_passed = .false.
        stop 1
    else
        write(output_unit, '(A)') 'All gcov_executor file/error tests passed!'
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

    subroutine test_cleanup_gcov_files()
        call run_test('cleanup_gcov_files with existing files', test_cleanup_existing_files)
    end subroutine test_cleanup_gcov_files

    logical function test_cleanup_existing_files() result(success)
        type(gcov_executor_t) :: executor
        character(len=256) :: gcov_files(2)
        character(len=256) :: file1, file2
        integer :: unit, iostat
        logical :: exists1, exists2

        success = .false.
        file1 = trim(test_workspace) // '/test1.gcov'
        file2 = trim(test_workspace) // '/test2.gcov'
        gcov_files(1) = file1
        gcov_files(2) = file2

        open(newunit=unit, file=file1, status='replace', iostat=iostat); if (iostat /= 0) return
        write(unit, '(A)') 'gcov content 1'
        close(unit)

        open(newunit=unit, file=file2, status='replace', iostat=iostat); if (iostat /= 0) return
        write(unit, '(A)') 'gcov content 2'
        close(unit)

        inquire(file=file1, exist=exists1)
        inquire(file=file2, exist=exists2)
        if (.not. (exists1 .and. exists2)) return

        call executor%cleanup_gcov_files(gcov_files)

        inquire(file=file1, exist=exists1)
        inquire(file=file2, exist=exists2)
        success = .not. (exists1 .or. exists2)
    end function test_cleanup_existing_files

    subroutine test_cleanup_gcov_files_nonexistent()
        call run_test('cleanup_gcov_files with nonexistent files', test_cleanup_nonexistent_files)
    end subroutine test_cleanup_gcov_files_nonexistent

    logical function test_cleanup_nonexistent_files() result(success)
        type(gcov_executor_t) :: executor
        character(len=256) :: gcov_files(2)
        gcov_files(1) = trim(test_workspace) // '/nonexistent1.gcov'
        gcov_files(2) = trim(test_workspace) // '/nonexistent2.gcov'
        call executor%cleanup_gcov_files(gcov_files)
        success = .true.
    end function test_cleanup_nonexistent_files

    subroutine test_memory_allocation_failures()
        call run_test('memory allocation error handling', test_allocation_handling)
    end subroutine test_memory_allocation_failures

    logical function test_allocation_handling() result(success)
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=256) :: source_file, gcda_file
        integer :: unit, iostat

        success = .false.
        source_file = trim(test_workspace) // '/alloc_test.f90'
        gcda_file   = trim(test_workspace) // '/alloc_test.gcda'

        open(newunit=unit, file=source_file, status='replace', iostat=iostat); if (iostat /= 0) return
        write(unit, '(A)') 'program test'
        close(unit)

        open(newunit=unit, file=gcda_file, status='replace', iostat=iostat); if (iostat /= 0) return
        write(unit, '(A)') 'mock data'
        close(unit)

        call executor%execute_gcov(source_file, gcov_files, error_ctx)
        success = .true.

        call safe_remove_file(source_file, error_ctx)
        call safe_remove_file(gcda_file,   error_ctx)
    end function test_allocation_handling

    subroutine test_output_directory_creation()
        call run_test('output directory creation', test_directory_creation)
    end subroutine test_output_directory_creation

    logical function test_directory_creation() result(success)
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=256) :: source_file, gcda_file, output_dir
        integer :: unit, iostat
        logical :: dir_exists

        success = .false.
        source_file = trim(test_workspace) // '/dir_test.f90'
        gcda_file   = 'dir_test.gcda'
        output_dir  = './test_new_output_dir'

        call executor%set_gcov_output_directory(output_dir)

        open(newunit=unit, file=source_file, status='replace', iostat=iostat); if (iostat /= 0) return
        write(unit, '(A)') 'program test'
        close(unit)

        open(newunit=unit, file=gcda_file, status='replace', iostat=iostat); if (iostat /= 0) return
        write(unit, '(A)') 'mock data'
        close(unit)

        call executor%execute_gcov(source_file, gcov_files, error_ctx)

        inquire(file=output_dir, exist=dir_exists)
        success = dir_exists .or. (error_ctx%error_code == ERROR_PERMISSION_DENIED)

        call safe_remove_file(source_file, error_ctx)
        call safe_remove_file(gcda_file,   error_ctx)
        call safe_remove_directory(output_dir, error_ctx)
    end function test_directory_creation

    subroutine test_file_operations_errors()
        call run_test('file operations error handling', test_file_error_handling)
    end subroutine test_file_operations_errors

    logical function test_file_error_handling() result(success)
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=256) :: source_file, gcda_file
        integer :: unit, iostat

        success = .false.
        source_file = trim(test_workspace) // '/error_test.f90'
        gcda_file   = trim(test_workspace) // '/error_test.gcda'

        open(newunit=unit, file=source_file, status='replace', iostat=iostat); if (iostat /= 0) return
        write(unit, '(A)') 'program test'
        close(unit)

        open(newunit=unit, file=gcda_file, status='replace', iostat=iostat); if (iostat /= 0) return
        write(unit, '(A)') 'mock data'
        close(unit)

        call executor%set_gcov_output_directory('/root/readonly_dir')
        call executor%execute_gcov(source_file, gcov_files, error_ctx)

        success = .true.

        call safe_remove_file(source_file, error_ctx)
        call safe_remove_file(gcda_file,   error_ctx)
    end function test_file_error_handling

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
            write(output_unit, '(A,A)') '  ✓ ', test_name
        else
            write(output_unit, '(A,A)') '  ✗ ', test_name
            all_tests_passed = .false.
        end if
    end subroutine run_test

end program test_gcov_executor_files_errors

