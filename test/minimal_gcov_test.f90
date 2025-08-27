program minimal_gcov_test
    !! Minimal test to reproduce the gcov processing issue
    use gcov_processor_auto
    use config_types, only: config_t
    use iso_fortran_env, only: output_unit
    implicit none

    type(config_t) :: config
    type(gcov_result_t) :: result
    character(len=256) :: cwd

    write(output_unit, '(A)') 'Minimal test: Starting...'

    ! Get current directory
    call getcwd(cwd)
    write(output_unit, '(A,A)') 'Current directory: ', trim(cwd)

    ! Clean up any previous test artifacts
    call execute_command_line('rm -rf test_build 2>/dev/null || true')

    ! Initialize config
    config%auto_discovery = .true.
    config%gcov_executable = 'gcov'  ! Use standard gcov

    ! Create minimal test infrastructure
    write(output_unit, '(A)') 'Creating test files...'
    call execute_command_line('mkdir -p test_build')
    call execute_command_line('touch test_build/test.gcda')
    call execute_command_line('touch test_build/test.gcno')

    ! List what we created
    write(output_unit, '(A)') 'Files created:'
    call execute_command_line('find test_build -type f -ls')

    ! Test auto_process_gcov_files
    write(output_unit, '(A)') 'Calling auto_process_gcov_files...'
    call auto_process_gcov_files('.', config, result)

    ! Report results
    write(output_unit, '(A,L1)') 'Success: ', result%success
    write(output_unit, '(A,A)') 'Error message: ', trim(result%error_message)

    ! Cleanup
    call execute_command_line('rm -rf test_build')
    write(output_unit, '(A)') 'Test completed.'

end program minimal_gcov_test