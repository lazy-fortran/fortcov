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
        character(len=*), intent(in) :: directory
        character(len=:), allocatable, intent(out) :: gcda_files(:)
        type(error_context_t), intent(out) :: error_ctx

        character(len=1024) :: find_command, temp_file
        character(len=256) :: temp_files(50)  ! Max 50 files
        integer :: unit, iostat, num_files, i, stat
        character(len=512) :: errmsg

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
            allocate(character(len=256) :: gcda_files(num_files), stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Failed to allocate gcda_files result: " // trim(errmsg)
                error_ctx%error_code = 1
                return
            end if
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
        character(len=*), intent(in) :: directory
        character(len=:), allocatable, intent(out) :: gcov_files(:)
        type(error_context_t), intent(out) :: error_ctx

        character(len=1024) :: find_command, temp_file
        character(len=256) :: temp_files(50)  ! Max 50 files
        integer :: unit, iostat, num_files, i, stat
        character(len=512) :: errmsg

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
            allocate(character(len=256) :: gcov_files(num_files), stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Failed to allocate gcov_files result: " // trim(errmsg)
                error_ctx%error_code = 1
                return
            end if
            gcov_files(1:num_files) = temp_files(1:num_files)
        else
            ! Don't set error here - let the caller handle no files found
            allocate(character(len=256) :: gcov_files(0), stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Failed to allocate empty gcov_files: " // trim(errmsg)
                error_ctx%error_code = 1
                return
            end if
        end if
    end subroutine direct_find_gcov_files

end module gcov_file_discovery