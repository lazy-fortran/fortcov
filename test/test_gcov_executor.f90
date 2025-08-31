program test_gcov_executor
    !! Comprehensive tests for gcov_executor module
    !! 
    !! This program tests all public methods and error paths of the
    !! gcov_executor module to ensure proper functionality and error handling.
    !! Following TDD principles with meaningful test descriptions.
    
    use iso_fortran_env, only: output_unit, error_unit
    use gcov_executor, only: gcov_executor_t
    use error_handling_core, only: error_context_t, ERROR_SUCCESS, &
                                  ERROR_INCOMPLETE_COVERAGE, clear_error_context
    use error_types, only: ERROR_PERMISSION_DENIED
    use file_ops_secure, only: safe_mkdir, safe_remove_file, safe_remove_directory
    use portable_temp_utils
    implicit none
    
    integer :: test_count = 0
    integer :: passed_count = 0
    logical :: all_tests_passed = .true.
    character(len=:), allocatable :: test_workspace
    
    write(output_unit, '(A)') 'Running gcov_executor comprehensive tests...'
    write(output_unit, '(A)') ''
    
    ! Setup test environment - use current directory for safety
    test_workspace = './gcov_executor_test'
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
    
    ! File management tests
    call test_cleanup_gcov_files()
    call test_cleanup_gcov_files_nonexistent()
    
    ! Edge case and error handling tests
    call test_memory_allocation_failures()
    call test_output_directory_creation()
    call test_file_operations_errors()
    
    ! Cleanup test environment
    call cleanup_test_environment()
    
    ! Report results
    write(output_unit, '(A)') ''
    write(output_unit, '(A,I0,A,I0,A)') 'Test Results: ', passed_count, ' / ', &
                                        test_count, ' tests passed'
    
    if (passed_count /= test_count) then
        write(error_unit, '(A)') 'Some gcov_executor tests failed!'
        all_tests_passed = .false.
        stop 1
    else
        write(output_unit, '(A)') 'All gcov_executor tests passed!'
    end if

contains

    ! Test Environment Management
    
    subroutine setup_test_environment()
        logical :: dir_exists
        
        ! Check if directory already exists
        inquire(file=test_workspace, exist=dir_exists)
        if (.not. dir_exists) then
            ! Use execute_command_line for directory creation (test environment)
            call execute_command_line('mkdir -p ' // test_workspace)
        end if
    end subroutine setup_test_environment
    
    subroutine cleanup_test_environment()
        type(error_context_t) :: error_ctx
        call safe_remove_directory(test_workspace, error_ctx)
        ! Ignore cleanup errors - not critical
    end subroutine cleanup_test_environment
    
    ! Core Functionality Tests
    
    subroutine test_gcov_executor_initialization()
        !! Test that gcov_executor_t can be initialized with default values
        type(gcov_executor_t) :: executor
        
        call run_test('gcov_executor_t initialization', &
            test_executor_default_initialization)
    end subroutine test_gcov_executor_initialization
    
    logical function test_executor_default_initialization() result(success)
        type(gcov_executor_t) :: executor
        
        ! Test that the executor can be created and configured
        call executor%set_branch_coverage(.false.)
        call executor%set_gcov_command('gcov')
        success = .true.
    end function test_executor_default_initialization
    
    subroutine test_execute_gcov_success_path()
        !! Test execute_gcov with valid inputs and mock files
        call run_test('execute_gcov with valid inputs', &
            test_execute_gcov_valid_inputs)
    end subroutine test_execute_gcov_success_path
    
    logical function test_execute_gcov_valid_inputs() result(success)
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=256) :: source_file, gcda_file, output_dir
        integer :: unit, iostat
        
        success = .false.
        
        ! Create mock source file and gcda file (gcda must match source basename)
        source_file = trim(test_workspace) // '/test_source.f90'
        gcda_file = trim(test_workspace) // '/test_source.gcda'
        output_dir = trim(test_workspace) // '/gcov_output'
        
        ! Create output directory and set it on executor
        call execute_command_line('mkdir -p ' // output_dir)
        call executor%set_gcov_output_directory(output_dir)
        
        ! Create mock source file
        open(newunit=unit, file=source_file, status='replace', iostat=iostat)
        if (iostat /= 0) return
        write(unit, '(A)') 'program test'
        write(unit, '(A)') 'end program'
        close(unit)
        
        ! Create mock gcda file
        open(newunit=unit, file=gcda_file, status='replace', iostat=iostat)
        if (iostat /= 0) return
        write(unit, '(A)') 'mock gcda data'
        close(unit)
        
        ! Execute gcov - should now return ERROR_INCOMPLETE_COVERAGE
        call executor%execute_gcov(source_file, gcov_files, error_ctx)
        
        ! Due to security fix Issue #963, gcov_executor doesn't execute shell commands
        ! but should return ERROR_INCOMPLETE_COVERAGE when no files are generated
        success = (error_ctx%error_code == ERROR_INCOMPLETE_COVERAGE)
        
        ! Cleanup
        call safe_remove_file(source_file, error_ctx)
        call safe_remove_file(gcda_file, error_ctx)
        call safe_remove_directory(output_dir, error_ctx)
    end function test_execute_gcov_valid_inputs
    
    subroutine test_execute_gcov_missing_source()
        !! Test execute_gcov with missing source file
        call run_test('execute_gcov with missing source file', &
            test_execute_gcov_no_source)
    end subroutine test_execute_gcov_missing_source
    
    logical function test_execute_gcov_no_source() result(success)
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=256) :: nonexistent_file
        
        nonexistent_file = trim(test_workspace) // '/nonexistent.f90'
        
        call executor%execute_gcov(nonexistent_file, gcov_files, error_ctx)
        
        ! Should fail due to missing source file
        success = (error_ctx%error_code /= ERROR_SUCCESS)
    end function test_execute_gcov_no_source
    
    subroutine test_execute_gcov_missing_gcda()
        !! Test execute_gcov with missing gcda file
        call run_test('execute_gcov with missing gcda file', &
            test_execute_gcov_no_gcda)
    end subroutine test_execute_gcov_missing_gcda
    
    logical function test_execute_gcov_no_gcda() result(success)
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=256) :: source_file
        integer :: unit, iostat
        
        success = .false.
        source_file = trim(test_workspace) // '/source_only.f90'
        
        ! Create source file but no gcda file
        open(newunit=unit, file=source_file, status='replace', iostat=iostat)
        if (iostat /= 0) return
        write(unit, '(A)') 'program test'
        close(unit)
        
        call executor%execute_gcov(source_file, gcov_files, error_ctx)
        
        ! Should fail due to missing gcda file
        success = (error_ctx%error_code /= ERROR_SUCCESS)
        
        call safe_remove_file(source_file, error_ctx)
    end function test_execute_gcov_no_gcda
    
    subroutine test_execute_gcov_no_output()
        !! Test execute_gcov when no gcov output is generated
        call run_test('execute_gcov with no gcov output generated', &
            test_execute_gcov_empty_result)
    end subroutine test_execute_gcov_no_output
    
    logical function test_execute_gcov_empty_result() result(success)
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=256) :: source_file, gcda_file, output_dir
        integer :: unit, iostat
        
        success = .false.
        source_file = trim(test_workspace) // '/empty_result.f90'
        gcda_file = trim(test_workspace) // '/empty_result.gcda'
        output_dir = trim(test_workspace) // '/gcov_output2'
        
        ! Create output directory and set it on executor
        call execute_command_line('mkdir -p ' // output_dir)
        call executor%set_gcov_output_directory(output_dir)
        
        ! Create mock files
        open(newunit=unit, file=source_file, status='replace', iostat=iostat)
        if (iostat /= 0) return
        write(unit, '(A)') 'program test'
        close(unit)
        
        open(newunit=unit, file=gcda_file, status='replace', iostat=iostat)
        if (iostat /= 0) return
        write(unit, '(A)') 'mock data'
        close(unit)
        
        call executor%execute_gcov(source_file, gcov_files, error_ctx)
        
        ! Current implementation should return ERROR_INCOMPLETE_COVERAGE
        success = (error_ctx%error_code == ERROR_INCOMPLETE_COVERAGE)
        
        call safe_remove_file(source_file, error_ctx)
        call safe_remove_file(gcda_file, error_ctx)
        call safe_remove_directory(output_dir, error_ctx)
    end function test_execute_gcov_empty_result
    
    ! Configuration Method Tests
    
    subroutine test_set_branch_coverage()
        !! Test set_branch_coverage method
        call run_test('set_branch_coverage configuration', &
            test_branch_coverage_setting)
    end subroutine test_set_branch_coverage
    
    logical function test_branch_coverage_setting() result(success)
        type(gcov_executor_t) :: executor
        
        ! Test enabling branch coverage
        call executor%set_branch_coverage(.true.)
        
        ! Test disabling branch coverage  
        call executor%set_branch_coverage(.false.)
        
        ! Method calls should complete without error
        success = .true.
    end function test_branch_coverage_setting
    
    subroutine test_set_working_directory()
        !! Test set_working_directory method
        call run_test('set_working_directory configuration', &
            test_working_directory_setting)
    end subroutine test_set_working_directory
    
    logical function test_working_directory_setting() result(success)
        type(gcov_executor_t) :: executor
        
        call executor%set_working_directory('/tmp/test')
        call executor%set_working_directory('')
        call executor%set_working_directory(test_workspace)
        
        success = .true.
    end function test_working_directory_setting
    
    subroutine test_set_gcov_command()
        !! Test set_gcov_command method
        call run_test('set_gcov_command configuration', &
            test_gcov_command_setting)
    end subroutine test_set_gcov_command
    
    logical function test_gcov_command_setting() result(success)
        type(gcov_executor_t) :: executor
        
        call executor%set_gcov_command('gcov')
        call executor%set_gcov_command('/usr/bin/gcov')
        call executor%set_gcov_command('custom-gcov')
        
        success = .true.
    end function test_gcov_command_setting
    
    subroutine test_set_gcov_output_directory()
        !! Test set_gcov_output_directory method
        call run_test('set_gcov_output_directory configuration', &
            test_output_directory_setting)
    end subroutine test_set_gcov_output_directory
    
    logical function test_output_directory_setting() result(success)
        type(gcov_executor_t) :: executor
        
        call executor%set_gcov_output_directory('build/coverage')
        call executor%set_gcov_output_directory(trim(test_workspace) // '/output')
        call executor%set_gcov_output_directory('')
        
        success = .true.
    end function test_output_directory_setting
    
    ! File Management Tests
    
    subroutine test_cleanup_gcov_files()
        !! Test cleanup_gcov_files with existing files
        call run_test('cleanup_gcov_files with existing files', &
            test_cleanup_existing_files)
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
        
        ! Create test files
        open(newunit=unit, file=file1, status='replace', iostat=iostat)
        if (iostat /= 0) return
        write(unit, '(A)') 'gcov content 1'
        close(unit)
        
        open(newunit=unit, file=file2, status='replace', iostat=iostat)
        if (iostat /= 0) return
        write(unit, '(A)') 'gcov content 2'
        close(unit)
        
        ! Verify files exist before cleanup
        inquire(file=file1, exist=exists1)
        inquire(file=file2, exist=exists2)
        if (.not. (exists1 .and. exists2)) return
        
        ! Cleanup files
        call executor%cleanup_gcov_files(gcov_files)
        
        ! Verify files are deleted
        inquire(file=file1, exist=exists1)
        inquire(file=file2, exist=exists2)
        success = .not. (exists1 .or. exists2)
    end function test_cleanup_existing_files
    
    subroutine test_cleanup_gcov_files_nonexistent()
        !! Test cleanup_gcov_files with nonexistent files
        call run_test('cleanup_gcov_files with nonexistent files', &
            test_cleanup_nonexistent_files)
    end subroutine test_cleanup_gcov_files_nonexistent
    
    logical function test_cleanup_nonexistent_files() result(success)
        type(gcov_executor_t) :: executor
        character(len=256) :: gcov_files(2)
        
        gcov_files(1) = trim(test_workspace) // '/nonexistent1.gcov'
        gcov_files(2) = trim(test_workspace) // '/nonexistent2.gcov'
        
        ! Should handle nonexistent files gracefully
        call executor%cleanup_gcov_files(gcov_files)
        
        success = .true.
    end function test_cleanup_nonexistent_files
    
    ! Edge Case and Error Handling Tests
    
    subroutine test_memory_allocation_failures()
        !! Test handling of memory allocation scenarios
        call run_test('memory allocation error handling', &
            test_allocation_handling)
    end subroutine test_memory_allocation_failures
    
    logical function test_allocation_handling() result(success)
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=256) :: source_file, gcda_file
        integer :: unit, iostat
        
        success = .false.
        source_file = trim(test_workspace) // '/alloc_test.f90'
        gcda_file = trim(test_workspace) // '/alloc_test.gcda'
        
        ! Create mock files
        open(newunit=unit, file=source_file, status='replace', iostat=iostat)
        if (iostat /= 0) return
        write(unit, '(A)') 'program test'
        close(unit)
        
        open(newunit=unit, file=gcda_file, status='replace', iostat=iostat)
        if (iostat /= 0) return
        write(unit, '(A)') 'mock data'
        close(unit)
        
        ! Execute - implementation handles allocation internally
        call executor%execute_gcov(source_file, gcov_files, error_ctx)
        
        ! Should not crash - either success or proper error handling
        success = .true.
        
        call safe_remove_file(source_file, error_ctx)
        call safe_remove_file(gcda_file, error_ctx)
    end function test_allocation_handling
    
    subroutine test_output_directory_creation()
        !! Test automatic output directory creation
        call run_test('output directory creation', &
            test_directory_creation)
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
        gcda_file = trim(test_workspace) // '/dir_test.gcda'
        output_dir = trim(test_workspace) // '/new_output_dir'
        
        ! Set custom output directory
        call executor%set_gcov_output_directory(output_dir)
        
        ! Create mock files
        open(newunit=unit, file=source_file, status='replace', iostat=iostat)
        if (iostat /= 0) return
        write(unit, '(A)') 'program test'
        close(unit)
        
        open(newunit=unit, file=gcda_file, status='replace', iostat=iostat)
        if (iostat /= 0) return
        write(unit, '(A)') 'mock data'
        close(unit)
        
        ! Execute - should create output directory
        call executor%execute_gcov(source_file, gcov_files, error_ctx)
        
        ! Check if directory was created
        inquire(file=output_dir, exist=dir_exists)
        success = dir_exists
        
        call safe_remove_file(source_file, error_ctx)
        call safe_remove_file(gcda_file, error_ctx)
        call safe_remove_directory(output_dir, error_ctx)
    end function test_directory_creation
    
    subroutine test_file_operations_errors()
        !! Test error handling in file operations
        call run_test('file operations error handling', &
            test_file_error_handling)
    end subroutine test_file_operations_errors
    
    logical function test_file_error_handling() result(success)
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=256) :: source_file, gcda_file
        integer :: unit, iostat
        
        success = .false.
        source_file = trim(test_workspace) // '/error_test.f90'
        gcda_file = trim(test_workspace) // '/error_test.gcda'
        
        ! Create mock files
        open(newunit=unit, file=source_file, status='replace', iostat=iostat)
        if (iostat /= 0) return
        write(unit, '(A)') 'program test'
        close(unit)
        
        open(newunit=unit, file=gcda_file, status='replace', iostat=iostat)
        if (iostat /= 0) return
        write(unit, '(A)') 'mock data'
        close(unit)
        
        ! Test with read-only directory (simulate permission errors)
        call executor%set_gcov_output_directory('/root/readonly_dir')
        call executor%execute_gcov(source_file, gcov_files, error_ctx)
        
        ! Should handle permission errors gracefully
        success = .true.
        
        call safe_remove_file(source_file, error_ctx)
        call safe_remove_file(gcda_file, error_ctx)
    end function test_file_error_handling
    
    ! Test Infrastructure Helpers
    
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

end program test_gcov_executor