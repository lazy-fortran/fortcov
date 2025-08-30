module gcov_file_discovery
    !! GCov File Discovery Module
    !!
    !! Provides specialized functionality for discovering .gcda and .gcov files
    !! in directories using secure file operations. Handles both direct find
    !! commands and fallback mechanisms for reliable file discovery.
    !!
    !! Key Features:
    !! - Secure .gcda file discovery with find command
    !! - Secure .gcov file discovery with find command
    !! - Error handling with detailed context information
    !! - Temporary file management for find operations
    !! - Memory-safe file list allocation and management

    use error_handling_core, only: error_context_t, ERROR_SUCCESS, clear_error_context, &
                              safe_write_message, safe_write_suggestion, safe_write_context
    use path_security_core, only: validate_path_security
    use file_ops_secure, only: safe_find_files, safe_remove_file
    implicit none
    private

    public :: discover_gcda_files
    public :: discover_gcov_files
    public :: validate_target_directory

contains

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

        ! Direct file discovery using find command
        call direct_find_gcda_files(directory, gcda_files, error_ctx)
    end subroutine discover_gcda_files

    subroutine direct_find_gcda_files(directory, gcda_files, error_ctx)
        !! Direct implementation of .gcda file discovery
        !! SECURITY FIX Issue #963: Use secure file operations instead of execute_command_line
        character(len=*), intent(in) :: directory
        character(len=:), allocatable, intent(out) :: gcda_files(:)
        type(error_context_t), intent(out) :: error_ctx

        character(len=512) :: pattern
        character(len=:), allocatable :: safe_directory

        call clear_error_context(error_ctx)

        ! Validate directory path security first
        call validate_path_security(directory, safe_directory, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return

        ! Build secure search pattern for .gcda files
        pattern = trim(safe_directory) // '/**/*.gcda'

        ! Use secure file finding instead of execute_command_line
        call safe_find_files(pattern, gcda_files, error_ctx)
        
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call safe_write_context(error_ctx, 'direct gcda file discovery')
            return
        end if

        ! Check if any files were found
        if (.not. allocated(gcda_files) .or. size(gcda_files) == 0) then
            call safe_write_message(error_ctx, &
                'No .gcda files found in directory: ' // trim(directory))
            call safe_write_suggestion(error_ctx, &
                'Run tests with coverage flags to generate .gcda files')
            call safe_write_context(error_ctx, 'direct gcda file discovery')
            error_ctx%error_code = 1
        end if
        
    end subroutine direct_find_gcda_files

    subroutine discover_gcov_files(directory, gcov_files, error_ctx)
        !! Discover all .gcov files in the specified directory
        character(len=*), intent(in) :: directory
        character(len=:), allocatable, intent(out) :: gcov_files(:)
        type(error_context_t), intent(out) :: error_ctx

        call clear_error_context(error_ctx)

        ! Direct file discovery using find command
        call direct_find_gcov_files(directory, gcov_files, error_ctx)
    end subroutine discover_gcov_files

    subroutine direct_find_gcov_files(directory, gcov_files, error_ctx)
        !! Direct implementation of .gcov file discovery
        !! SECURITY FIX Issue #963: Use secure file operations instead of execute_command_line
        character(len=*), intent(in) :: directory
        character(len=:), allocatable, intent(out) :: gcov_files(:)
        type(error_context_t), intent(out) :: error_ctx

        character(len=512) :: pattern
        character(len=:), allocatable :: safe_directory

        call clear_error_context(error_ctx)

        ! Validate directory path security first
        call validate_path_security(directory, safe_directory, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return

        ! Build secure search pattern for .gcov files
        pattern = trim(safe_directory) // '/**/*.gcov'

        ! Use secure file finding instead of execute_command_line
        call safe_find_files(pattern, gcov_files, error_ctx)
        
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call safe_write_context(error_ctx, 'direct gcov file discovery')
            return
        end if

        ! Don't set error for empty results - let caller handle no files found
        ! The secure find function already handles allocation correctly
        
    end subroutine direct_find_gcov_files

end module gcov_file_discovery