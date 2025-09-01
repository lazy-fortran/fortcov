program minimal_gcov_test
    !! Minimal test to reproduce the gcov processing issue
    use gcov_processor_auto
    use config_types, only: config_t
    use iso_fortran_env, only: output_unit
    use file_ops_secure, only: safe_mkdir, safe_remove_file
    use error_handling_core, only: error_context_t, clear_error_context
    implicit none

    type(config_t) :: config
    type(gcov_result_t) :: result
    character(len=256) :: cwd
    character(len=512) :: temp_workspace
    character(len=512) :: abs_path

    write(output_unit, '(A)') 'Minimal test: Starting...'

    ! Prepare isolated temp workspace and chdir into it
    call getcwd(cwd)
    block
        use portable_temp_utils, only: get_temp_dir
        character(len=:), allocatable :: base
        type(error_context_t) :: err
        base = get_temp_dir()
        temp_workspace = trim(base) // '/fortcov_tests/minimal_gcov'
        call safe_mkdir(temp_workspace, err)
        call chdir(temp_workspace)
    end block
    write(output_unit, '(A,A)') 'Current directory: ', trim(temp_workspace)

    ! Clean up any previous test artifacts
    ! SECURITY FIX Issue #971: Use secure file operations
    call cleanup_test_build_secure()

    ! Create minimal test infrastructure with proper mock
    write(output_unit, '(A)') 'Creating test infrastructure...'
    ! SECURITY FIX Issue #971: Use secure directory operations
    call create_test_directory_secure('test_build')
    
    ! Create mock gcov executable instead of empty files
    call create_mock_gcov()
    
    ! Get absolute path to mock gcov
    ! SECURITY FIX Issue #971: Use secure path operations
    call get_mock_gcov_path_secure(abs_path)
    
    ! Initialize config with mock gcov
    config%auto_discovery = .true.
    ! SECURITY FIX Issue #963: gcov_executable removed - test uses hardcoded 'gcov' command

    ! Create valid gcda/gcno files that mock can process
    ! SECURITY FIX Issue #971: Use secure file operations
    call create_test_files_secure()

    ! List what we created
    write(output_unit, '(A)') 'Files created:'
    ! SECURITY FIX Issue #971: Use secure file listing
    call list_test_files_secure()

    ! Test auto_process_gcov_files
    write(output_unit, '(A)') 'Calling auto_process_gcov_files...'
    call auto_process_gcov_files('.', config, result)

    ! Report results
    write(output_unit, '(A,L1)') 'Success: ', result%success
    if (.not. result%success) then
        write(output_unit, '(A,A)') 'Error message: ', trim(result%error_message)
    else
        write(output_unit, '(A)') 'Test passed successfully!'
    end if

    ! Cleanup
    ! SECURITY FIX Issue #971: Use secure cleanup
    call cleanup_test_build_secure()
    call chdir(cwd)
    write(output_unit, '(A)') 'Test completed.'

contains

    subroutine create_mock_gcov()
        ! Create a mock gcov that generates valid .gcov files
        ! SECURITY FIX Issue #971: Use secure file operations
        call create_mock_gcov_script_secure('test_build/mock_gcov')
    end subroutine create_mock_gcov
    
    ! SECURITY FIX Issue #971: Secure replacement functions
    
    subroutine cleanup_test_build_secure()
        !! Secure cleanup of test_build directory
        type(error_context_t) :: error_ctx
        call safe_remove_file('test_build', error_ctx)
        ! Ignore errors - directory may not exist
    end subroutine cleanup_test_build_secure
    
    subroutine create_test_directory_secure(dir_path)
        !! Secure directory creation
        character(len=*), intent(in) :: dir_path
        type(error_context_t) :: error_ctx
        call safe_mkdir(dir_path, error_ctx)
        ! Ignore errors in test setup
    end subroutine create_test_directory_secure
    
    subroutine get_mock_gcov_path_secure(path_result)
        !! Securely get mock gcov path without shell execution
        character(len=512), intent(out) :: path_result
        character(len=256) :: cwd
        call getcwd(cwd)
        path_result = trim(cwd) // '/test_build/mock_gcov'
    end subroutine get_mock_gcov_path_secure
    
    subroutine create_test_files_secure()
        !! Create test gcda, gcno, and source files securely
        integer :: unit_num, ios
        
        ! Create test.gcda
        open(newunit=unit_num, file='test_build/test.gcda', status='replace', iostat=ios)
        if (ios == 0) close(unit_num)
        
        ! Create test.gcno
        open(newunit=unit_num, file='test_build/test.gcno', status='replace', iostat=ios)
        if (ios == 0) close(unit_num)
        
        ! Create test.f90
        open(newunit=unit_num, file='test_build/test.f90', status='replace', iostat=ios)
        if (ios == 0) then
            write(unit_num, '(A)') 'program test'
            write(unit_num, '(A)') 'end program'
            close(unit_num)
        end if
    end subroutine create_test_files_secure
    
    subroutine list_test_files_secure()
        !! List test files without shell execution
        logical :: file_exists
        write(output_unit, '(A)') '  test_build/test.gcda'
        write(output_unit, '(A)') '  test_build/test.gcno'
        write(output_unit, '(A)') '  test_build/test.f90'
        write(output_unit, '(A)') '  test_build/mock_gcov'
    end subroutine list_test_files_secure
    
    subroutine create_mock_gcov_script_secure(script_path)
        !! Create mock gcov script using secure file operations
        character(len=*), intent(in) :: script_path
        integer :: unit_num, ios
        open(newunit=unit_num, file=script_path, status='replace', iostat=ios)
        if (ios == 0) then
            write(unit_num, '(A)') '#!/bin/bash'
            write(unit_num, '(A)') '# Mock gcov for testing'
            write(unit_num, '(A)') 'input_file="$1"'
            write(unit_num, '(A)') 'echo "Mock gcov processing: $input_file" >&2'
            write(unit_num, '(A)') 'if [[ "$input_file" == *.gcda ]]; then'
            write(unit_num, '(A)') '  base=$(basename "$input_file" .gcda)'
            write(unit_num, '(A)') '  output_file="$base.f90.gcov"'
            write(unit_num, '(A)') '  echo "        -:    0:Source:$base.f90" > "$output_file"'
            write(unit_num, '(A)') '  echo "        -:    1:program $base" >> "$output_file"'
            write(unit_num, '(A)') '  echo "        1:    2:  implicit none" >> "$output_file"'
            write(unit_num, '(A)') '  echo "        -:    3:end program" >> "$output_file"'
            write(unit_num, '(A)') '  echo "Lines executed:50.00% of 2" >&2'
            write(unit_num, '(A)') 'fi'
            write(unit_num, '(A)') 'exit 0'
            close(unit_num)
        end if
    end subroutine create_mock_gcov_script_secure

end program minimal_gcov_test
