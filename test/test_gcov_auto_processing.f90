module test_gcov_auto_processing
    !! GCov Auto-Processing Test Suite
    !!
    !! Comprehensive tests for automated gcov file discovery and processing.
    !! Tests both successful processing scenarios and error handling cases
    !! including directory validation, file discovery, and bulk processing.
    
    use test_framework_utilities
    use gcov_processor_auto, only: gcov_result_t, auto_process_gcov_files, &
                                   gcov_file_summary_t
    use config_types, only: config_t
    use directory_ops_core, only: ensure_directory_safe
    use error_handling_core, only: error_context_t, clear_error_context
    implicit none
    private
    
    public :: test_gcov_file_processing, test_gcov_error_handling
    public :: test_gcov_directory_validation, test_gcov_bulk_processing
    public :: test_gcov_no_files_scenario, test_gcov_result_statistics
    
contains
    
    subroutine test_gcov_file_processing(counter)
        !! Test basic gcov auto-processing functionality
        type(test_counter_t), intent(inout) :: counter
        
        type(config_t) :: config
        type(gcov_result_t) :: result
        character(len=256) :: test_directory
        logical :: test_passed
        
        print *, "Test: GCov auto-processing basic functionality"
        
        ! Initialize test configuration
        call initialize_test_config(config)
        block
            use portable_temp_utils, only: get_temp_dir
            character(len=:), allocatable :: temp_dir
            temp_dir = get_temp_dir()
            test_directory = temp_dir // "/gcov_test_workspace"
        end block
        
        ! Execute auto-processing
        call auto_process_gcov_files(test_directory, config, result)
        
        ! Validate basic result structure
        test_passed = validate_basic_result_structure(result)
        
        if (test_passed) then
            call increment_pass(counter)
            print *, "  ✅ PASS: GCov auto-processing completed successfully"
            call print_result_summary(result)
        else
            call increment_fail(counter)
            print *, "  failed basic validation"
            print *, "    Error: ", trim(result%error_message)
        end if
    end subroutine test_gcov_file_processing
    
    subroutine test_gcov_error_handling(counter)
        !! Test error handling for invalid directories and scenarios
        type(test_counter_t), intent(inout) :: counter
        
        type(config_t) :: config
        type(gcov_result_t) :: result
        character(len=256) :: invalid_directory
        logical :: test_passed
        
        print *, "Test: GCov auto-processing error handling"
        
        call initialize_test_config(config)
        invalid_directory = "/nonexistent/directory/path"
        
        ! Execute auto-processing on invalid directory
        call auto_process_gcov_files(invalid_directory, config, result)
        
        ! Validate error handling
        test_passed = validate_error_handling(result)
        
        if (test_passed) then
            call increment_pass(counter)
            print *, "  ✅ PASS: Error handling works correctly"
            print *, "    Expected error: ", trim(result%error_message)
        else
            call increment_fail(counter)
            print *, "  ❌ FAIL: Error handling not working as expected"
        end if
    end subroutine test_gcov_error_handling
    
    subroutine test_gcov_directory_validation(counter)
        !! Test directory validation functionality
        type(test_counter_t), intent(inout) :: counter
        
        type(config_t) :: config
        type(gcov_result_t) :: result
        logical :: test_passed
        
        print *, "Test: GCov directory validation"
        
        call initialize_test_config(config)
        
        ! Test empty directory string
        call auto_process_gcov_files("", config, result)
        test_passed = (.not. result%success) .and. (len_trim(result%error_message) > 0)
        
        if (test_passed) then
            call increment_pass(counter)
            print *, "  ✅ PASS: Directory validation prevents empty paths"
        else
            call increment_fail(counter)
            print *, "  ❌ FAIL: Directory validation should reject empty paths"
        end if
    end subroutine test_gcov_directory_validation
    
    subroutine test_gcov_bulk_processing(counter)
        !! Test bulk processing of multiple gcov files
        type(test_counter_t), intent(inout) :: counter
        
        type(config_t) :: config
        type(gcov_result_t) :: result
        character(len=256) :: test_directory
        logical :: test_passed
        
        print *, "Test: GCov bulk processing capabilities"
        
        call initialize_test_config(config)
        block
            use portable_temp_utils, only: get_temp_dir
            character(len=:), allocatable :: temp_dir
            temp_dir = get_temp_dir()
            test_directory = temp_dir // "/gcov_bulk_test_workspace"
        end block
        
        ! Execute bulk processing
        call auto_process_gcov_files(test_directory, config, result)
        
        ! Validate bulk processing results
        test_passed = validate_bulk_processing_results(result)
        
        if (test_passed) then
            call increment_pass(counter)
            print *, "  ✅ PASS: Bulk processing handles multiple files correctly"
            call print_detailed_statistics(result)
        else
            call increment_fail(counter)
            print *, "  failed"
        end if
    end subroutine test_gcov_bulk_processing
    
    subroutine test_gcov_no_files_scenario(counter)
        !! Test behavior when no .gcov files are found
        type(test_counter_t), intent(inout) :: counter
        
        type(config_t) :: config
        type(gcov_result_t) :: result
        character(len=256) :: empty_directory
        type(error_context_t) :: error_ctx
        logical :: test_passed
        
        print *, "Test: GCov no files found scenario"
        
        call initialize_test_config(config)
        block  
            use portable_temp_utils, only: get_temp_dir
            character(len=:), allocatable :: temp_dir
            temp_dir = get_temp_dir()
            empty_directory = temp_dir // "/empty_gcov_test_dir"
        end block
        
        ! Create empty test directory
        call ensure_directory_safe(empty_directory, error_ctx)
        
        ! Execute processing on empty directory
        call auto_process_gcov_files(empty_directory, config, result)
        
        ! Validate no-files scenario handling
        test_passed = validate_no_files_handling(result)
        
        if (test_passed) then
            call increment_pass(counter)
            print *, "  ✅ PASS: Handles no files scenario correctly"
        else
            call increment_fail(counter)
            print *, "  ❌ FAIL: No files scenario handling incorrect"
        end if
        
        ! Cleanup test directory
        call cleanup_test_directory(empty_directory)
    end subroutine test_gcov_no_files_scenario
    
    subroutine test_gcov_result_statistics(counter)
        !! Test detailed result statistics and summary information
        type(test_counter_t), intent(inout) :: counter
        
        type(config_t) :: config
        type(gcov_result_t) :: result
        logical :: test_passed
        
        print *, "Test: GCov result statistics and summaries"
        
        call initialize_test_config(config)
        
        ! Execute processing
        call auto_process_gcov_files(".", config, result)
        
        ! Validate statistics accuracy
        test_passed = validate_result_statistics(result)
        
        if (test_passed) then
            call increment_pass(counter)
            print *, "  ✅ PASS: Result statistics are accurate and complete"
        else
            call increment_fail(counter)
            print *, "  failed"
        end if
    end subroutine test_gcov_result_statistics
    
    ! Helper subroutines for test setup and validation
    
    subroutine initialize_test_config(config)
        !! Initialize test configuration with sensible defaults
        type(config_t), intent(out) :: config
        
        config%gcov_executable = 'gcov'  ! Set the gcov executable
        config%auto_discovery = .true.
        config%auto_test_execution = .false.
        config%verbose = .false.
        config%quiet = .true.
        config%keep_gcov_files = .true.
        config%threads = 1
        config%minimum_coverage = 0.0
        config%fail_under_threshold = 0.0
    end subroutine initialize_test_config
    
    function validate_basic_result_structure(result) result(is_valid)
        !! Validate that result has expected structure and fields
        type(gcov_result_t), intent(in) :: result
        logical :: is_valid
        
        is_valid = .true.
        
        ! Check essential fields are present
        if (result%files_discovered < 0) is_valid = .false.
        if (result%files_processed < 0) is_valid = .false.
        if (result%successful_files < 0) is_valid = .false.
        if (result%failed_files < 0) is_valid = .false.
        if (result%total_lines_processed < 0) is_valid = .false.
        
        ! Check consistency of counts
        if (result%files_processed /= (result%successful_files + result%failed_files)) then
            is_valid = .false.
        end if
        
        ! Check that file summaries match discovered files
        if (allocated(result%file_summaries)) then
            if (size(result%file_summaries) /= result%files_discovered) then
                is_valid = .false.
            end if
        else if (result%files_discovered > 0) then
            is_valid = .false.
        end if
    end function validate_basic_result_structure
    
    function validate_error_handling(result) result(is_valid)
        !! Validate that error scenarios are handled correctly
        type(gcov_result_t), intent(in) :: result
        logical :: is_valid
        
        is_valid = (.not. result%success) .and. (len_trim(result%error_message) > 0)
    end function validate_error_handling
    
    function validate_bulk_processing_results(result) result(is_valid)
        !! Validate bulk processing handles multiple files correctly
        type(gcov_result_t), intent(in) :: result
        logical :: is_valid
        
        is_valid = .true.
        
        ! Check that processing statistics are consistent
        if (result%files_processed < 0) is_valid = .false.
        if (result%files_discovered < result%files_processed) is_valid = .false.
        if ((result%successful_files + result%failed_files) /= result%files_processed) then
            is_valid = .false.
        end if
    end function validate_bulk_processing_results
    
    function validate_no_files_handling(result) result(is_valid)
        !! Validate handling when no files are found
        type(gcov_result_t), intent(in) :: result
        logical :: is_valid
        
        is_valid = (.not. result%success) .and. &
                  (result%files_discovered == 0) .and. &
                  (result%files_processed == 0) .and. &
                  (len_trim(result%error_message) > 0)
    end function validate_no_files_handling
    
    function validate_result_statistics(result) result(is_valid)
        !! Validate accuracy of result statistics
        type(gcov_result_t), intent(in) :: result
        logical :: is_valid
        
        integer :: i, calculated_lines
        
        is_valid = .true.
        
        ! Validate line count accuracy
        if (allocated(result%file_summaries)) then
            calculated_lines = 0
            do i = 1, size(result%file_summaries)
                if (result%file_summaries(i)%processed_successfully) then
                    calculated_lines = calculated_lines + &
                                     result%file_summaries(i)%lines_processed
                end if
            end do
            
            if (calculated_lines /= result%total_lines_processed) then
                is_valid = .false.
            end if
        end if
    end function validate_result_statistics
    
    subroutine print_result_summary(result)
        !! Print summary of processing results
        type(gcov_result_t), intent(in) :: result
        
        print *, "    Files discovered: ", result%files_discovered
        print *, "    Files processed: ", result%files_processed
        print *, "    Successful: ", result%successful_files
        print *, "    Failed: ", result%failed_files
        print *, "    Total lines: ", result%total_lines_processed
    end subroutine print_result_summary
    
    subroutine print_detailed_statistics(result)
        !! Print detailed processing statistics
        type(gcov_result_t), intent(in) :: result
        
        integer :: i
        
        call print_result_summary(result)
        
        if (allocated(result%file_summaries) .and. result%files_discovered <= 5) then
            print *, "    File details:"
            do i = 1, size(result%file_summaries)
                if (result%file_summaries(i)%processed_successfully) then
                    print *, "      ", trim(result%file_summaries(i)%filename), &
                           " - ", result%file_summaries(i)%lines_processed, " lines"
                else
                    print *, "      ", trim(result%file_summaries(i)%filename), " - FAILED"
                end if
            end do
        end if
    end subroutine print_detailed_statistics
    
    subroutine cleanup_test_directory(directory)
        !! Clean up test directory after tests
        character(len=*), intent(in) :: directory
        
        integer :: stat
        
        ! Simple cleanup using system command
        call execute_command_line("rmdir " // trim(directory) // " 2>/dev/null", &
                                 exitstat=stat)
    end subroutine cleanup_test_directory
    
end module test_gcov_auto_processing
