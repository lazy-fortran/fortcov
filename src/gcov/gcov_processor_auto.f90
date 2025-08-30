module gcov_processor_auto
    !! GCov Auto-Processing Module
    !!
    !! Provides comprehensive automated gcov file discovery and processing
    !! functionality. Orchestrates the interaction between file discovery,
    !! gcov generation, and file processing modules.
    !!
    !! Key Features:
    !! - Orchestration of gcov auto-processing workflow
    !! - Integration with discovery, processing, and generation modules
    !! - Comprehensive result reporting and error handling
    !! - Clean separation of concerns with focused modules
    
    use error_handling_core, only: error_context_t, ERROR_SUCCESS, clear_error_context
    use config_types, only: config_t
    use gcov_file_discovery, only: discover_gcda_files, validate_target_directory
    use gcov_processing_core, only: gcov_result_t, gcov_file_summary_t, &
                                   initialize_processing_result, process_discovered_files, &
                                   finalize_processing_results
    use gcov_generation_utils, only: generate_gcov_files
    implicit none
    private
    
    public :: gcov_result_t
    public :: auto_process_gcov_files
    public :: gcov_file_summary_t
    
contains
    
    subroutine auto_process_gcov_files(directory, config, result)
        !! Main entry point for automated gcov file processing
        !!
        !! Orchestrates the complete workflow: validates directory, discovers
        !! .gcda files, generates .gcov files, processes them, and finalizes results.
        !!
        !! Args:
        !!   directory: Target directory to search for .gcov files
        !!   config: Configuration object with gcov executable settings
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
        
        ! Discover .gcda files in directory
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
    
    !! Error handling procedures for orchestration layer
    
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

    subroutine handle_gcov_generation_failure(result, error_ctx)
        !! Handle gcov file generation failures
        type(gcov_result_t), intent(inout) :: result
        type(error_context_t), intent(in) :: error_ctx
        
        result%success = .false.
        result%error_message = 'Gcov generation failed: ' // trim(error_ctx%message)
    end subroutine handle_gcov_generation_failure
    
end module gcov_processor_auto