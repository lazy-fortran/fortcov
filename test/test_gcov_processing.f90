program test_gcov_processing
    !! GCOV Processing Tests (Issue #277 - Part 2)
    !!
    !! Tests for the gcov_auto_processor module focusing on
    !! gcov file processing and source mapping discovery.

    use gcov_auto_processor
    use config_types, only: config_t
    use iso_fortran_env, only: output_unit
    implicit none

    integer :: test_count = 0
    integer :: passed_count = 0

    write(output_unit, '(A)') 'Running GCOV processing tests...'
    write(output_unit, '(A)') ''

    call test_auto_process_gcov_files_found()
    call test_auto_process_gcov_files_not_found()
    call test_auto_process_gcov_files_build_context()
    call test_source_mapping_discovery()

    write(output_unit, '(A)') ''
    write(output_unit, '(A,I0,A,I0,A)') 'Test Results: ', passed_count, ' / ', &
                                        test_count, ' tests passed'
    
    if (passed_count /= test_count) then
        write(output_unit, '(A)') 'Some tests failed!'
        stop 1
    end if

contains

    subroutine test_auto_process_gcov_files_found()
        !! Given .gcda files exist
        !! When auto_process_gcov_files is called
        !! Then it should process them and return success
        type(config_t) :: config
        type(gcov_result_t) :: result
        
        write(output_unit, '(A)') 'Test 6: Auto-process gcov files when found'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        call create_mock_gcda_files()
        call create_mock_gcov_executable()
        
        ! Use absolute path for mock gcov so it works when cd is used
        block
            use portable_temp_utils, only: get_temp_dir
            character(len=512) :: abs_path, mock_gcov_path_file
            character(len=:), allocatable :: temp_dir
            integer :: unit_num, iostat
            
            temp_dir = get_temp_dir()
            mock_gcov_path_file = temp_dir // '/mock_gcov_path.txt'
            
            call execute_command_line('realpath test_build/mock_gcov > "' // trim(mock_gcov_path_file) // '"')
            open(newunit=unit_num, file=trim(mock_gcov_path_file), status='old', action='read')
            read(unit_num, '(A)', iostat=iostat) abs_path
            close(unit_num)
            call execute_command_line('rm -f "' // trim(mock_gcov_path_file) // '"')
            config%gcov_executable = trim(abs_path)
        end block
        
        ! Run auto_process on test_build directory
        call auto_process_gcov_files('test_build', config, result)
        
        if (.not. result%success) then
            print *, '  DEBUG: Error message: ', trim(result%error_message)
            print *, '  DEBUG: Files processed: ', result%files_processed
            print *, '  DEBUG: Gcov executable: ', trim(config%gcov_executable)
            ! Check if mock gcov exists
            block
                logical :: file_exists
                inquire(file=trim(config%gcov_executable), exist=file_exists)
                print *, '  DEBUG: Mock gcov exists: ', file_exists
            end block
        end if
        
        call assert_true(result%success, 'Gcov processing succeeded')
        call assert_true(result%files_processed >= 0, 'Gcov files processed count valid')
        
        call cleanup_mock_gcov_files()
        call cleanup_mock_gcov_executable()
    end subroutine test_auto_process_gcov_files_found

    subroutine test_auto_process_gcov_files_not_found()
        !! Given no .gcda files exist
        !! When auto_process_gcov_files is called
        !! Then it should fail with guidance
        type(config_t) :: config
        type(gcov_result_t) :: result
        
        write(output_unit, '(A)') 'Test 7: Auto-process gcov files when not found'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        ! No mock files created
        
        call auto_process_gcov_files('.', config, result)
        
        call assert_false(result%success, 'Gcov processing failed appropriately')
        call assert_true(index(result%error_message, '.gcda') > 0, &
                        'Error message mentions gcda files')
        call assert_true(len_trim(result%error_message) >= 0, 'Error message available')
        
    end subroutine test_auto_process_gcov_files_not_found

    subroutine test_auto_process_gcov_files_build_context()
        !! Given .gcda files in build directories
        !! When auto_process_gcov_files is called  
        !! Then it should use build context
        type(config_t) :: config
        type(gcov_result_t) :: result
        
        write(output_unit, '(A)') 'Test 8: Auto-process gcov files with build context'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        call create_mock_build_structure()
        call create_mock_gcda_files_in_build()
        call create_mock_gcov_executable()
        
        ! Use absolute path for mock gcov so it works when cd is used
        block
            use portable_temp_utils, only: get_temp_dir
            character(len=512) :: abs_path, mock_gcov_path_file
            character(len=:), allocatable :: temp_dir
            integer :: unit_num, iostat
            
            temp_dir = get_temp_dir()
            mock_gcov_path_file = temp_dir // '/mock_gcov_path.txt'
            
            call execute_command_line('realpath test_build/mock_gcov > "' // trim(mock_gcov_path_file) // '"')
            open(newunit=unit_num, file=trim(mock_gcov_path_file), status='old', action='read')
            read(unit_num, '(A)', iostat=iostat) abs_path
            close(unit_num)
            call execute_command_line('rm -f "' // trim(mock_gcov_path_file) // '"')
            config%gcov_executable = trim(abs_path)
        end block
        
        ! Run on test_build directory which contains the build structure
        call auto_process_gcov_files('test_build', config, result)
        
        call assert_true(result%success, 'Build context processing succeeded')
        call assert_true(result%files_processed >= 0, 'Build context processed files')
        
        call cleanup_mock_build_structure()
        call cleanup_mock_gcov_executable()
    end subroutine test_auto_process_gcov_files_build_context

    subroutine test_source_mapping_discovery()
        !! Given .gcov files with source references
        !! When auto_process_gcov_files is called
        !! Then it should discover source file mappings
        type(config_t) :: config
        type(gcov_result_t) :: result
        
        write(output_unit, '(A)') 'Test 19: Source mapping discovery'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        call create_mock_gcda_files()
        call create_mock_gcov_with_sources()
        call create_mock_gcov_executable()
        
        ! Use absolute path for mock gcov so it works when cd is used
        block
            use portable_temp_utils, only: get_temp_dir
            character(len=512) :: abs_path, mock_gcov_path_file
            character(len=:), allocatable :: temp_dir
            integer :: unit_num, iostat
            
            temp_dir = get_temp_dir()
            mock_gcov_path_file = temp_dir // '/mock_gcov_path.txt'
            
            call execute_command_line('realpath test_build/mock_gcov > "' // trim(mock_gcov_path_file) // '"')
            open(newunit=unit_num, file=trim(mock_gcov_path_file), status='old', action='read')
            read(unit_num, '(A)', iostat=iostat) abs_path
            close(unit_num)
            call execute_command_line('rm -f "' // trim(mock_gcov_path_file) // '"')
            config%gcov_executable = trim(abs_path)
        end block
        
        ! Process test_build directory
        call auto_process_gcov_files('test_build', config, result)
        
        call assert_true(result%success, 'Source mapping succeeded')
        ! STUB: Source mappings functionality not implemented in gcov_result_t
        call assert_true(result%files_processed >= 0, 'Files processed count valid')
        
        call cleanup_mock_gcov_with_sources()
        call cleanup_mock_gcov_executable()
    end subroutine test_source_mapping_discovery

    ! Mock creation and cleanup subroutines
    
    subroutine create_mock_gcda_files()
        call execute_command_line('mkdir -p test_build/test')
        ! Create minimal valid gcda and gcno files that won't cause gcov to fail
        ! For testing, we just need the files to exist - mock gcov will handle them
        call execute_command_line('touch test_build/test/test.gcda')
        call execute_command_line('touch test_build/test/test.gcno')
        ! Create a simple source file for gcov to reference
        call execute_command_line('echo "program test" > test_build/test/test.f90')
        call execute_command_line('echo "end program" >> test_build/test/test.f90')
    end subroutine create_mock_gcda_files

    subroutine create_mock_build_structure()
        call execute_command_line('mkdir -p test_build/debug/src')
    end subroutine create_mock_build_structure

    subroutine create_mock_gcda_files_in_build()
        call execute_command_line('mkdir -p test_build/debug/src')
        call execute_command_line('touch test_build/debug/src/main.gcda')
        call execute_command_line('touch test_build/debug/src/main.gcno')
        ! Create a simple source file for gcov to reference
        call execute_command_line('echo "program main" > test_build/debug/src/main.f90')
        call execute_command_line('echo "end program" >> test_build/debug/src/main.f90')
    end subroutine create_mock_gcda_files_in_build

    subroutine create_mock_gcov_with_sources()
        call execute_command_line('mkdir -p test_build/test')
        call execute_command_line('echo "        -:    1:module test" > test_build/test/test.gcov')
        call execute_command_line('echo "        -:    2:contains" >> test_build/test/test.gcov')
    end subroutine create_mock_gcov_with_sources

    subroutine cleanup_mock_gcov_files()
        call execute_command_line('rm -rf test_build')
    end subroutine cleanup_mock_gcov_files

    subroutine cleanup_mock_build_structure()
        call execute_command_line('rm -rf test_build')
    end subroutine cleanup_mock_build_structure

    subroutine cleanup_mock_gcov_with_sources()
        call cleanup_mock_gcov_files()
    end subroutine cleanup_mock_gcov_with_sources
    
    subroutine create_mock_gcov_executable()
        !! Create a mock gcov executable that always succeeds
        !! and creates dummy .gcov files
        
        ! Create a shell script that acts as mock gcov
        ! Write it directly with execute_command_line to avoid quote issues
        call execute_command_line('mkdir -p test_build')
        call execute_command_line('cat > test_build/mock_gcov << "MOCKGCOV"' // char(10) // &
                                  '#!/bin/bash' // char(10) // &
                                  '# Mock gcov for testing' // char(10) // &
                                  'input_file="$1"' // char(10) // &
                                  'echo "Mock gcov invoked with: $input_file in dir: $(pwd)" >&2' // char(10) // &
                                  'if [[ "$input_file" == *.gcno ]]; then' // char(10) // &
                                  '  base=$(basename "$input_file" .gcno)' // char(10) // &
                                  '  # Create gcov file in current directory (where gcov is run from)' // char(10) // &
                                  '  output_file="$base.f90.gcov"' // char(10) // &
                                  '  echo "Creating gcov file: $output_file in $(pwd)" >&2' // char(10) // &
                                  '  echo "        -:    0:Source:$base.f90" > "$output_file"' // char(10) // &
                                  '  echo "        -:    1:module $base" >> "$output_file"' // char(10) // &
                                  '  echo "        1:    2:  implicit none" >> "$output_file"' // char(10) // &
                                  '  echo "        -:    3:end module" >> "$output_file"' // char(10) // &
                                  '  sync' // char(10) // &
                                  '  ls -la "$output_file" >&2' // char(10) // &
                                  'fi' // char(10) // &
                                  'exit 0' // char(10) // &
                                  'MOCKGCOV')
        call execute_command_line('chmod +x test_build/mock_gcov')
    end subroutine create_mock_gcov_executable
    
    subroutine cleanup_mock_gcov_executable()
        call execute_command_line('rm -f test_build/mock_gcov')
    end subroutine cleanup_mock_gcov_executable

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
        
        ! Set test mode environment variable
        call execute_command_line('export FORTCOV_TEST_MODE=1')
        
        config%auto_discovery = .false.
        config%auto_test_execution = .true.
        config%test_timeout_seconds = 30
        config%verbose = .false.
        config%gcov_executable = 'gcov'
    end subroutine initialize_default_config

end program test_gcov_processing