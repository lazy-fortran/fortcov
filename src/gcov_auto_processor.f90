module gcov_auto_processor
    !! GCov Auto-Processing Module
    !!
    !! Provides comprehensive automated gcov file discovery and processing
    !! functionality. Integrates with the secure file operations and error
    !! handling systems to safely discover and process .gcov files in bulk.
    !!
    !! Key Features:
    !! - Automated .gcov file discovery in directories
    !! - Bulk processing with error recovery
    !! - Comprehensive result reporting and statistics
    !! - Integration with existing coverage processing pipeline
    !! - Secure file operations with validation
    
    use error_handling, only: error_context_t, ERROR_SUCCESS, clear_error_context, &
                              safe_write_message, safe_write_suggestion, safe_write_context
    use file_finder, only: find_files_with_glob
    use gcov_file_processor, only: process_gcov_file
    use coverage_model, only: coverage_data_t
    use directory_operations, only: ensure_directory_safe
    use path_validation, only: validate_path_security
    implicit none
    private
    
    public :: gcov_result_t
    public :: auto_process_gcov_files
    public :: gcov_file_summary_t
    
    ! Result summary for individual file processing
    type :: gcov_file_summary_t
        character(len=256) :: filename = ''
        logical :: processed_successfully = .false.
        character(len=256) :: error_message = ''
        integer :: lines_processed = 0
    end type gcov_file_summary_t
    
    ! Comprehensive result type for gcov auto-processing
    type :: gcov_result_t
        logical :: success = .false.
        character(len=512) :: error_message = ''
        integer :: files_processed = 0
        integer :: files_discovered = 0
        integer :: successful_files = 0
        integer :: failed_files = 0
        integer :: total_lines_processed = 0
        type(gcov_file_summary_t), allocatable :: file_summaries(:)
    end type gcov_result_t
    
contains
    
    subroutine auto_process_gcov_files(directory, config, result)
        !! Main entry point for automated gcov file processing
        !!
        !! Discovers all .gcov files in the specified directory and processes
        !! them in bulk. Provides comprehensive error handling and detailed
        !! statistics about the processing results.
        !!
        !! Args:
        !!   directory: Target directory to search for .gcov files
        !!   config: Configuration object (polymorphic to avoid circular deps)
        !!   result: Comprehensive processing results and statistics
        
        character(len=*), intent(in) :: directory
        type(*), intent(in) :: config  ! Using assumed type to avoid circular dependency
        type(gcov_result_t), intent(out) :: result
        
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        
        call initialize_processing_result(result)
        
        ! Validate directory path
        call validate_target_directory(directory, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call handle_directory_validation_failure(result, error_ctx)
            return
        end if
        
        ! Discover .gcov files in directory
        call discover_gcov_files(directory, gcov_files, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call handle_file_discovery_failure(result, error_ctx)
            return
        end if
        
        result%files_discovered = size(gcov_files)
        
        if (result%files_discovered == 0) then
            call handle_no_files_discovered(result, directory)
            return
        end if
        
        ! Process discovered files
        call process_discovered_files(gcov_files, result)
        
        ! Finalize processing results
        call finalize_processing_results(result)
        
    end subroutine auto_process_gcov_files
    
    subroutine initialize_processing_result(result)
        !! Initialize result structure with default values
        type(gcov_result_t), intent(out) :: result
        
        result%success = .false.
        result%error_message = ''
        result%files_processed = 0
        result%files_discovered = 0
        result%successful_files = 0
        result%failed_files = 0
        result%total_lines_processed = 0
        
        if (allocated(result%file_summaries)) deallocate(result%file_summaries)
    end subroutine initialize_processing_result
    
    subroutine validate_target_directory(directory, error_ctx)
        !! Validate that the target directory exists and is accessible
        character(len=*), intent(in) :: directory
        type(error_context_t), intent(out) :: error_ctx
        
        logical :: dir_exists
        character(len=:), allocatable :: safe_path
        
        call clear_error_context(error_ctx)
        
        ! Validate directory path format
        call validate_path_security(directory, safe_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! Check if directory exists
        inquire(file=trim(safe_path), exist=dir_exists)
        if (.not. dir_exists) then
            call safe_write_message(error_ctx, &
                'Target directory does not exist: ' // trim(directory))
            call safe_write_suggestion(error_ctx, &
                'Verify the directory path or create the directory')
            call safe_write_context(error_ctx, 'gcov auto-processing directory validation')
            error_ctx%error_code = 1  ! Non-success error code
        end if
    end subroutine validate_target_directory
    
    subroutine discover_gcov_files(directory, gcov_files, error_ctx)
        !! Discover all .gcov files in the specified directory
        character(len=*), intent(in) :: directory
        character(len=:), allocatable, intent(out) :: gcov_files(:)
        type(error_context_t), intent(out) :: error_ctx
        
        call clear_error_context(error_ctx)
        
        ! Use file finder to discover .gcov files
        gcov_files = find_files_with_glob(directory, "*.gcov")
        
        ! Validate discovery results
        if (.not. allocated(gcov_files)) then
            call safe_write_message(error_ctx, &
                'Failed to search for gcov files in directory: ' // trim(directory))
            call safe_write_suggestion(error_ctx, &
                'Check directory permissions and file system access')
            call safe_write_context(error_ctx, 'gcov file discovery')
            error_ctx%error_code = 1
        end if
    end subroutine discover_gcov_files
    
    subroutine process_discovered_files(gcov_files, result)
        !! Process each discovered .gcov file and collect statistics
        character(len=*), intent(in) :: gcov_files(:)
        type(gcov_result_t), intent(inout) :: result
        
        integer :: i, file_count
        type(coverage_data_t) :: coverage_data
        logical :: error_flag
        
        file_count = size(gcov_files)
        allocate(result%file_summaries(file_count))
        
        ! Process each file
        do i = 1, file_count
            call process_single_gcov_file(gcov_files(i), result%file_summaries(i), &
                                        coverage_data, error_flag)
            
            result%files_processed = result%files_processed + 1
            
            if (.not. error_flag) then
                result%successful_files = result%successful_files + 1
                result%total_lines_processed = result%total_lines_processed + &
                                              result%file_summaries(i)%lines_processed
            else
                result%failed_files = result%failed_files + 1
            end if
        end do
    end subroutine process_discovered_files
    
    subroutine process_single_gcov_file(gcov_file, file_summary, coverage_data, error_flag)
        !! Process a single .gcov file and collect detailed statistics
        character(len=*), intent(in) :: gcov_file
        type(gcov_file_summary_t), intent(out) :: file_summary
        type(coverage_data_t), intent(out) :: coverage_data
        logical, intent(out) :: error_flag
        
        file_summary%filename = trim(gcov_file)
        file_summary%processed_successfully = .false.
        file_summary%error_message = ''
        file_summary%lines_processed = 0
        
        ! Process the gcov file using existing processor
        call process_gcov_file(gcov_file, coverage_data, error_flag)
        
        if (.not. error_flag) then
            file_summary%processed_successfully = .true.
            file_summary%lines_processed = count_coverage_lines(coverage_data)
        else
            file_summary%error_message = 'Failed to process gcov file: ' // trim(gcov_file)
        end if
    end subroutine process_single_gcov_file
    
    function count_coverage_lines(coverage_data) result(line_count)
        !! Count the total number of coverage lines in processed data
        type(coverage_data_t), intent(in) :: coverage_data
        integer :: line_count
        
        integer :: i
        
        line_count = 0
        
        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                if (allocated(coverage_data%files(i)%lines)) then
                    line_count = line_count + size(coverage_data%files(i)%lines)
                end if
            end do
        end if
    end function count_coverage_lines
    
    subroutine finalize_processing_results(result)
        !! Determine final success status and generate summary message
        type(gcov_result_t), intent(inout) :: result
        
        ! Determine overall success
        result%success = (result%files_discovered > 0) .and. &
                        (result%successful_files > 0) .and. &
                        (result%failed_files == 0)
        
        ! Generate summary message
        if (result%success) then
            write(result%error_message, '(A,I0,A,I0,A)') &
                'Successfully processed ', result%successful_files, &
                ' gcov files with ', result%total_lines_processed, ' coverage lines'
        else if (result%files_discovered == 0) then
            result%error_message = 'No .gcov files found in target directory'
        else if (result%failed_files > 0) then
            write(result%error_message, '(A,I0,A,I0,A)') &
                'Processing completed with ', result%failed_files, &
                ' failures out of ', result%files_processed, ' files'
        else
            result%error_message = 'GCov auto-processing completed with unknown status'
        end if
    end subroutine finalize_processing_results
    
    subroutine handle_directory_validation_failure(result, error_ctx)
        !! Handle directory validation failures
        type(gcov_result_t), intent(inout) :: result
        type(error_context_t), intent(in) :: error_ctx
        
        result%success = .false.
        result%error_message = 'Directory validation failed: ' // trim(error_ctx%message)
    end subroutine handle_directory_validation_failure
    
    subroutine handle_file_discovery_failure(result, error_ctx)
        !! Handle file discovery failures
        type(gcov_result_t), intent(inout) :: result
        type(error_context_t), intent(in) :: error_ctx
        
        result%success = .false.
        result%error_message = 'File discovery failed: ' // trim(error_ctx%message)
    end subroutine handle_file_discovery_failure
    
    subroutine handle_no_files_discovered(result, directory)
        !! Handle case where no .gcov files are found
        type(gcov_result_t), intent(inout) :: result
        character(len=*), intent(in) :: directory
        
        result%success = .false.
        result%error_message = 'No .gcov files found in directory: ' // trim(directory)
    end subroutine handle_no_files_discovered
    
end module gcov_auto_processor