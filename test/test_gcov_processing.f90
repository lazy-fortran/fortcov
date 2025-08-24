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
        
        call auto_process_gcov_files('.', config, result)
        
        call assert_true(result%success, 'Gcov processing succeeded')
        call assert_true(size(result%gcov_files) > 0, 'Gcov files found')
        
        call cleanup_mock_gcov_files()
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
        call assert_true(len_trim(result%guidance_message) > 0, 'Guidance provided')
        
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
        
        call auto_process_gcov_files('.', config, result)
        
        call assert_true(result%success, 'Build context processing succeeded')
        call assert_true(result%used_build_context, 'Used build context')
        
        call cleanup_mock_build_structure()
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
        
        call auto_process_gcov_files('.', config, result)
        
        call assert_true(result%success, 'Source mapping succeeded')
        if (allocated(result%source_mappings)) then
            call assert_true(size(result%source_mappings) > 0, &
                            'Source mappings found')
            if (size(result%source_mappings) > 0) then
                call assert_true(len_trim(result%source_mappings(1)%source_file) > 0, &
                                'Source file mapped')
                call assert_true(len_trim(result%source_mappings(1)%gcov_file) > 0, &
                                'Gcov file mapped')
            end if
        else
            write(output_unit, '(A)') '  Warning: source_mappings not allocated'
        end if
        
        call cleanup_mock_gcov_with_sources()
    end subroutine test_source_mapping_discovery

    ! Mock creation and cleanup subroutines
    
    subroutine create_mock_gcda_files()
        call execute_command_line('mkdir -p build/test && touch build/test/test.gcda')
    end subroutine create_mock_gcda_files

    subroutine create_mock_build_structure()
        call execute_command_line('mkdir -p build/debug/src')
    end subroutine create_mock_build_structure

    subroutine create_mock_gcda_files_in_build()
        call execute_command_line('touch build/debug/src/main.gcda')
    end subroutine create_mock_gcda_files_in_build

    subroutine create_mock_gcov_with_sources()
        call execute_command_line('mkdir -p build/test')
        call execute_command_line('echo "        -:    1:module test" > build/test/test.gcov')
        call execute_command_line('echo "        -:    2:contains" >> build/test/test.gcov')
    end subroutine create_mock_gcov_with_sources

    subroutine cleanup_mock_gcov_files()
        call execute_command_line('rm -rf build')
    end subroutine cleanup_mock_gcov_files

    subroutine cleanup_mock_build_structure()
        call execute_command_line('rm -rf build')
    end subroutine cleanup_mock_build_structure

    subroutine cleanup_mock_gcov_with_sources()
        call cleanup_mock_gcov_files()
    end subroutine cleanup_mock_gcov_with_sources

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

end program test_gcov_processing