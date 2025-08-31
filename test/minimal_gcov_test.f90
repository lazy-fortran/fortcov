program minimal_gcov_test
    !! Minimal test to reproduce the gcov processing issue
    use gcov_processor_auto
    use config_types, only: config_t
    use iso_fortran_env, only: output_unit
    implicit none

    type(config_t) :: config
    type(gcov_result_t) :: result
    character(len=256) :: cwd
    character(len=512) :: abs_path

    write(output_unit, '(A)') 'Minimal test: Starting...'

    ! Get current directory
    call getcwd(cwd)
    write(output_unit, '(A,A)') 'Current directory: ', trim(cwd)

    ! Clean up any previous test artifacts
    call execute_command_line('rm -rf test_build 2>/dev/null || true')

    ! Create minimal test infrastructure with proper mock
    write(output_unit, '(A)') 'Creating test infrastructure...'
    call execute_command_line('mkdir -p test_build')
    
    ! Create mock gcov executable instead of empty files
    call create_mock_gcov()
    
    ! Get absolute path to mock gcov
    call execute_command_line('realpath test_build/mock_gcov > test_build/mock_path.txt')
    open(unit=10, file='test_build/mock_path.txt', status='old')
    read(10, '(A)') abs_path
    close(10)
    
    ! Initialize config with mock gcov
    config%auto_discovery = .true.
    ! SECURITY FIX Issue #963: gcov_executable removed - test uses hardcoded 'gcov' command

    ! Create valid gcda/gcno files that mock can process
    call execute_command_line('touch test_build/test.gcda')
    call execute_command_line('touch test_build/test.gcno')
    call execute_command_line('echo "program test" > test_build/test.f90')
    call execute_command_line('echo "end program" >> test_build/test.f90')

    ! List what we created
    write(output_unit, '(A)') 'Files created:'
    call execute_command_line('find test_build -type f -ls')

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
    call execute_command_line('rm -rf test_build')
    write(output_unit, '(A)') 'Test completed.'

contains

    subroutine create_mock_gcov()
        ! Create a mock gcov that generates valid .gcov files
        call execute_command_line('cat > test_build/mock_gcov << "MOCKGCOV"' // char(10) // &
            '#!/bin/bash' // char(10) // &
            '# Mock gcov for testing' // char(10) // &
            'input_file="$1"' // char(10) // &
            'echo "Mock gcov processing: $input_file" >&2' // char(10) // &
            'if [[ "$input_file" == *.gcda ]]; then' // char(10) // &
            '  base=$(basename "$input_file" .gcda)' // char(10) // &
            '  output_file="$base.f90.gcov"' // char(10) // &
            '  echo "        -:    0:Source:$base.f90" > "$output_file"' // char(10) // &
            '  echo "        -:    1:program $base" >> "$output_file"' // char(10) // &
            '  echo "        1:    2:  implicit none" >> "$output_file"' // char(10) // &
            '  echo "        -:    3:end program" >> "$output_file"' // char(10) // &
            '  echo "Lines executed:50.00% of 2" >&2' // char(10) // &
            'fi' // char(10) // &
            'exit 0' // char(10) // &
            'MOCKGCOV')
        call execute_command_line('chmod +x test_build/mock_gcov')
    end subroutine create_mock_gcov

end program minimal_gcov_test
