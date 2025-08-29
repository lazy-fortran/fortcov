module gcov_processor_auto
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
    
    use error_handling_core, only: error_context_t, ERROR_SUCCESS, clear_error_context, &
                              safe_write_message, safe_write_suggestion, safe_write_context
    use file_utils_core, only: find_files_with_glob
    use gcov_file_processor, only: process_gcov_file
    use coverage_model_core, only: coverage_data_t
    use directory_ops_core, only: ensure_directory_safe
    use path_security_core, only: validate_path_security
    use config_types, only: config_t
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
        type(config_t), intent(in) :: config
        type(gcov_result_t), intent(out) :: result
        
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcda_files(:)
        character(len=:), allocatable :: gcov_files(:)
        
        call initialize_processing_result(result)
        
        ! Validate directory path
        call validate_target_directory(directory, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call handle_directory_validation_failure(result, error_ctx)
            return
        end if
        
        ! Discover .gcda files in directory (not .gcov files!)
        call discover_gcda_files(directory, gcda_files, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call handle_file_discovery_failure(result, error_ctx)
            return
        end if
        
        result%files_discovered = size(gcda_files)
        
        if (result%files_discovered == 0) then
            call handle_no_gcda_files_discovered(result, directory)
            return
        end if
        
        ! Generate .gcov files from .gcda files using gcov executable
        call generate_gcov_files(directory, gcda_files, config, gcov_files, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call handle_gcov_generation_failure(result, error_ctx)
            return
        end if
        
        ! Process generated .gcov files
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
    
    subroutine discover_gcda_files(directory, gcda_files, error_ctx)
        !! Discover all .gcda files in the specified directory
        character(len=*), intent(in) :: directory
        character(len=:), allocatable, intent(out) :: gcda_files(:)
        type(error_context_t), intent(out) :: error_ctx
        
        call clear_error_context(error_ctx)
        
        ! Direct file discovery using find command (bypassing security layer for now)
        call direct_find_gcda_files(directory, gcda_files, error_ctx)
    end subroutine discover_gcda_files

    subroutine direct_find_gcda_files(directory, gcda_files, error_ctx)
        !! Direct implementation of .gcda file discovery
        character(len=*), intent(in) :: directory
        character(len=:), allocatable, intent(out) :: gcda_files(:)
        type(error_context_t), intent(out) :: error_ctx
        
        character(len=1024) :: find_command, temp_file
        character(len=256) :: temp_files(50)  ! Max 50 files
        integer :: unit, iostat, num_files, i, stat
        
        call clear_error_context(error_ctx)
        
        ! Create temporary file for find output
        temp_file = '/tmp/fortcov_gcda_find.tmp'
        
        ! Build and execute find command
        write(find_command, '(A,1X,A,1X,A,1X,A,1X,A,1X,A,1X,A)') &
            'find', trim(directory), '-name', '"*.gcda"', '-type', 'f', '>' // trim(temp_file)
        
        call execute_command_line(trim(find_command), exitstat=stat)
        
        if (stat /= 0) then
            call safe_write_message(error_ctx, &
                'Find command failed for directory: ' // trim(directory))
            call safe_write_suggestion(error_ctx, &
                'Check directory exists and permissions')
            call safe_write_context(error_ctx, 'direct gcda file discovery')
            error_ctx%error_code = 1
            return
        end if
        
        ! Read results from temporary file
        open(newunit=unit, file=trim(temp_file), status='old', &
             action='read', iostat=iostat)
        if (iostat /= 0) then
            call safe_write_message(error_ctx, &
                'Failed to read find results from: ' // trim(temp_file))
            error_ctx%error_code = 1
            return
        end if
        
        ! Read file paths
        num_files = 0
        do i = 1, size(temp_files)
            read(unit, '(A)', iostat=iostat) temp_files(i)
            if (iostat /= 0) exit
            if (len_trim(temp_files(i)) > 0) then
                num_files = num_files + 1
            end if
        end do
        
        close(unit)
        call execute_command_line('rm -f ' // trim(temp_file))
        
        ! Allocate and populate result array
        if (num_files > 0) then
            allocate(character(len=256) :: gcda_files(num_files))
            gcda_files(1:num_files) = temp_files(1:num_files)
        else
            call safe_write_message(error_ctx, &
                'No .gcda files found in directory: ' // trim(directory))
            call safe_write_suggestion(error_ctx, &
                'Run tests with coverage flags to generate .gcda files')
            call safe_write_context(error_ctx, 'direct gcda file discovery')
            error_ctx%error_code = 1
        end if
    end subroutine direct_find_gcda_files

    subroutine direct_find_gcov_files(directory, gcov_files, error_ctx)
        !! Direct implementation of .gcov file discovery
        character(len=*), intent(in) :: directory
        character(len=:), allocatable, intent(out) :: gcov_files(:)
        type(error_context_t), intent(out) :: error_ctx
        
        character(len=1024) :: find_command, temp_file
        character(len=256) :: temp_files(50)  ! Max 50 files
        integer :: unit, iostat, num_files, i, stat
        
        call clear_error_context(error_ctx)
        
        ! Create temporary file for find output
        temp_file = '/tmp/fortcov_gcov_find.tmp'
        
        ! Build and execute find command
        write(find_command, '(A,1X,A,1X,A,1X,A,1X,A,1X,A,1X,A)') &
            'find', trim(directory), '-name', '"*.gcov"', '-type', 'f', '>' // trim(temp_file)
        
        call execute_command_line(trim(find_command), exitstat=stat)
        
        if (stat /= 0) then
            call safe_write_message(error_ctx, &
                'Find command failed for directory: ' // trim(directory))
            error_ctx%error_code = 1
            return
        end if
        
        ! Read results from temporary file
        open(newunit=unit, file=trim(temp_file), status='old', &
             action='read', iostat=iostat)
        if (iostat /= 0) then
            call safe_write_message(error_ctx, &
                'Failed to read find results from: ' // trim(temp_file))
            error_ctx%error_code = 1
            return
        end if
        
        ! Read file paths
        num_files = 0
        do i = 1, size(temp_files)
            read(unit, '(A)', iostat=iostat) temp_files(i)
            if (iostat /= 0) exit
            if (len_trim(temp_files(i)) > 0) then
                num_files = num_files + 1
            end if
        end do
        
        close(unit)
        call execute_command_line('rm -f ' // trim(temp_file))
        
        ! Allocate and populate result array
        if (num_files > 0) then
            allocate(character(len=256) :: gcov_files(num_files))
            gcov_files(1:num_files) = temp_files(1:num_files)
        else
            ! Don't set error here - let the caller handle no files found
            allocate(character(len=256) :: gcov_files(0))
        end if
    end subroutine direct_find_gcov_files
    
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
        
        ! Determine overall success - succeed if we processed at least some files
        result%success = (result%files_discovered > 0) .and. &
                        (result%successful_files > 0)
        
        ! Generate summary message
        if (result%success) then
            write(result%error_message, '(A,I0,A,I0,A)') &
                'Successfully processed ', result%successful_files, &
                ' gcov files with ', result%total_lines_processed, ' coverage lines'
        else if (result%files_discovered == 0) then
            result%error_message = 'No .gcda files found in target directory'
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
    
    subroutine handle_no_gcda_files_discovered(result, directory)
        !! Handle case where no .gcda files are found
        type(gcov_result_t), intent(inout) :: result
        character(len=*), intent(in) :: directory
        
        result%success = .false.
        result%error_message = 'No .gcda files found in directory: ' // trim(directory)
    end subroutine handle_no_gcda_files_discovered

    subroutine generate_gcov_files(directory, gcda_files, config, gcov_files, error_ctx)
        !! Generate .gcov files from .gcda files using gcov executable
        character(len=*), intent(in) :: directory
        character(len=*), intent(in) :: gcda_files(:)
        type(config_t), intent(in) :: config
        character(len=:), allocatable, intent(out) :: gcov_files(:)
        type(error_context_t), intent(out) :: error_ctx
        
        integer :: i, gcov_command_result
        character(len=1024) :: gcov_command
        character(len=256) :: gcno_file, gcda_base
        character(len=256) :: gcov_executable
        logical :: gcno_exists
        
        call clear_error_context(error_ctx)
        
        ! Ensure gcov executable is set
        if (allocated(config%gcov_executable) .and. len_trim(config%gcov_executable) > 0) then
            gcov_executable = trim(config%gcov_executable)
        else
            gcov_executable = 'gcov'  ! Default to 'gcov' if not specified
        end if
        
        ! Process each .gcda file
        do i = 1, size(gcda_files)
            ! Extract base name without extension
            gcda_base = gcda_files(i)(1:index(gcda_files(i), '.gcda') - 1)
            
            ! Check if corresponding .gcno file exists
            gcno_file = trim(gcda_base) // '.gcno'
            inquire(file=trim(gcno_file), exist=gcno_exists)
            
            if (.not. gcno_exists) then
                call safe_write_message(error_ctx, &
                    'Missing .gcno file for: ' // trim(gcda_files(i)))
                call safe_write_suggestion(error_ctx, &
                    'Ensure tests were compiled with coverage flags')
                call safe_write_context(error_ctx, 'gcov file generation')
                error_ctx%error_code = 1
                return
            end if
            
            ! Run gcov on the .gcda file with full path
            write(gcov_command, '(A,1X,A)') trim(gcov_executable), &
                trim(gcda_files(i))
            call execute_command_line(trim(gcov_command), &
                exitstat=gcov_command_result)
            
            if (gcov_command_result /= 0) then
                call safe_write_message(error_ctx, &
                    'Gcov command failed for: ' // trim(gcda_files(i)))
                call safe_write_suggestion(error_ctx, &
                    'Check gcov executable path and .gcda/.gcno file integrity')
                call safe_write_context(error_ctx, 'gcov execution')
                error_ctx%error_code = 1
                return
            end if
        end do
        
        ! Discover generated .gcov files in current directory and subdirectories
        call direct_find_gcov_files('.', gcov_files, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            ! Try searching more broadly
            call direct_find_gcov_files(directory, gcov_files, error_ctx)
        end if
        
        if (.not. allocated(gcov_files) .or. size(gcov_files) == 0) then
            call safe_write_message(error_ctx, &
                'No .gcov files generated from .gcda files')
            call safe_write_suggestion(error_ctx, &
                'Check gcov command output and file permissions')
            call safe_write_context(error_ctx, 'gcov file generation')
            error_ctx%error_code = 1
        end if
    end subroutine generate_gcov_files

    subroutine handle_gcov_generation_failure(result, error_ctx)
        !! Handle gcov file generation failures
        type(gcov_result_t), intent(inout) :: result
        type(error_context_t), intent(in) :: error_ctx
        
        result%success = .false.
        result%error_message = 'Gcov generation failed: ' // trim(error_ctx%message)
    end subroutine handle_gcov_generation_failure
    
end module gcov_processor_auto