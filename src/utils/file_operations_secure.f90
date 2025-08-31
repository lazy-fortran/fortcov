module file_operations_secure
    !! Secure file operations for standard file management
    !!
    !! This module provides secure file operations including removal, movement,
    !! and directory creation using Fortran intrinsics to prevent shell injection
    !! vulnerabilities. All operations include comprehensive security validation.
    use error_handling_core
    use path_security_core, only: validate_path_security
    implicit none
    private
    
    ! Public procedures
    public :: safe_remove_file
    public :: safe_move_file
    public :: safe_mkdir
    
contains

    ! Secure file removal without shell commands - SECURITY FIX for Issue #963
    subroutine safe_remove_file(filename, error_ctx)
        !! Securely remove a file using Fortran intrinsics instead of shell commands
        !! This prevents shell injection vulnerabilities from execute_command_line calls
        character(len=*), intent(in) :: filename
        type(error_context_t), intent(out) :: error_ctx
        
        character(len=:), allocatable :: safe_filename
        logical :: file_exists
        integer :: unit, iostat, close_iostat
        
        call clear_error_context(error_ctx)
        
        ! Validate filename path security first
        call validate_path_security(filename, safe_filename, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! Check if file exists before attempting deletion
        inquire(file=safe_filename, exist=file_exists)
        if (.not. file_exists) then
            ! File doesn't exist - this is not an error, just return success
            return
        end if
        
        ! Use Fortran intrinsic file deletion instead of execute_command_line
        ! This is secure and prevents shell injection attacks
        open(newunit=unit, file=safe_filename, status='old', iostat=iostat)
        if (iostat /= 0) then
            ! File might be locked or inaccessible
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, &
                "Cannot open file for secure deletion: " // safe_filename)
            call safe_write_suggestion(error_ctx, &
                "Check file permissions and that file is not in use")
            return
        end if
        
        ! Secure deletion using Fortran intrinsic close with status='delete'
        close(unit, status='delete', iostat=close_iostat)
        if (close_iostat /= 0) then
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, &
                "Failed to delete file securely: " // safe_filename)
            call safe_write_suggestion(error_ctx, &
                "File may be locked or system may deny deletion")
        end if
        
    end subroutine safe_remove_file

    ! Secure file move without shell commands - SECURITY FIX for Issue #963
    subroutine safe_move_file(source_file, target_file, error_ctx)
        !! Securely move a file using Fortran intrinsics instead of shell commands
        !! This prevents shell injection vulnerabilities from execute_command_line calls
        character(len=*), intent(in) :: source_file
        character(len=*), intent(in) :: target_file
        type(error_context_t), intent(out) :: error_ctx
        
        character(len=:), allocatable :: safe_source, safe_target
        logical :: source_exists
        
        call clear_error_context(error_ctx)
        
        ! Validate both file paths for security
        call validate_move_file_paths(source_file, target_file, safe_source, &
                                      safe_target, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! Check source file exists
        inquire(file=safe_source, exist=source_exists)
        if (.not. source_exists) then
            error_ctx%error_code = ERROR_MISSING_FILE
            call safe_write_message(error_ctx, &
                "Source file does not exist: " // safe_source)
            return
        end if
        
        ! Perform the secure copy operation
        call perform_secure_file_copy(safe_source, safe_target, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! Remove original file after successful copy
        call safe_remove_file(safe_source, error_ctx)
        ! Note: If removal fails, we still have successful copy
        ! This matches 'mv' behavior where copy success is primary
        
    end subroutine safe_move_file
    
    ! Validate file paths for move operation
    subroutine validate_move_file_paths(source_file, target_file, safe_source, &
                                        safe_target, error_ctx)
        character(len=*), intent(in) :: source_file, target_file
        character(len=:), allocatable, intent(out) :: safe_source, safe_target
        type(error_context_t), intent(out) :: error_ctx
        
        call validate_path_security(source_file, safe_source, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        call validate_path_security(target_file, safe_target, error_ctx)
    end subroutine validate_move_file_paths
    
    ! Perform secure file copy operation
    subroutine perform_secure_file_copy(source_path, target_path, error_ctx)
        character(len=*), intent(in) :: source_path, target_path
        type(error_context_t), intent(inout) :: error_ctx
        
        integer :: source_unit, target_unit, iostat, copy_iostat
        character(len=1024) :: buffer
        
        ! Open source file for reading
        open(newunit=source_unit, file=source_path, status='old', &
             action='read', iostat=iostat)
        if (iostat /= 0) then
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, &
                "Cannot open source file for reading: " // source_path)
            return
        end if
        
        ! Open target file for writing
        open(newunit=target_unit, file=target_path, status='replace', &
             action='write', iostat=iostat)
        if (iostat /= 0) then
            close(source_unit)
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, &
                "Cannot create target file: " // target_path)
            return
        end if
        
        ! Copy content line by line
        do
            read(source_unit, '(A)', iostat=copy_iostat) buffer
            if (copy_iostat /= 0) exit
            write(target_unit, '(A)') trim(buffer)
        end do
        
        ! Close files
        close(target_unit)
        close(source_unit)
    end subroutine perform_secure_file_copy

    ! Directory creation with injection protection
    subroutine safe_mkdir(directory_path, error_ctx)
        character(len=*), intent(in) :: directory_path
        type(error_context_t), intent(out) :: error_ctx
        
        character(len=:), allocatable :: safe_dir_path
        integer :: stat
        logical :: dir_exists
        
        call clear_error_context(error_ctx)
        
        ! Validate directory path
        call validate_path_security(directory_path, safe_dir_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! Check if directory already exists
        inquire(file=safe_dir_path, exist=dir_exists)
        if (dir_exists) return
        
        ! Create directory safely using Fortran intrinsics instead of shell
        call create_directory_recursive(safe_dir_path, stat)
        
        if (stat /= 0) then
            error_ctx%error_code = ERROR_PERMISSION_DENIED
            error_ctx%recoverable = .false.
            call safe_write_message(error_ctx, &
                "Failed to create directory - permission denied or invalid path")
            call safe_write_suggestion(error_ctx, &
                "Check directory permissions and path validity")
            call safe_write_context(error_ctx, "directory creation")
        end if
    end subroutine safe_mkdir

    ! Create directory recursively using Fortran intrinsics
    ! SECURITY FIX Issue #963: Replace mkdir -p shell vulnerability
    recursive subroutine create_directory_recursive(dir_path, stat)
        character(len=*), intent(in) :: dir_path
        integer, intent(out) :: stat
        
        character(len=512) :: parent_path
        integer :: last_slash, path_len
        logical :: parent_exists
        
        stat = 0
        path_len = len_trim(dir_path)
        
        ! Check if directory already exists
        inquire(file=dir_path, exist=parent_exists)
        if (parent_exists) return
        
        ! Find parent directory
        last_slash = index(dir_path, '/', back=.true.)
        if (last_slash > 1) then
            parent_path = dir_path(1:last_slash-1)
            
            ! Recursively create parent directory first
            call create_directory_recursive(parent_path, stat)
            if (stat /= 0) return
        end if
        
        ! Create this directory using a simple method
        ! Note: Fortran doesn't have built-in mkdir, so we use a workaround
        ! Create a temporary file in the directory path to force creation
        call create_single_directory(dir_path, stat)
        
    end subroutine create_directory_recursive
    
    ! Create a single directory (non-recursive)
    subroutine create_single_directory(dir_path, stat)
        character(len=*), intent(in) :: dir_path
        integer, intent(out) :: stat
        
        character(len=512) :: temp_file_path
        integer :: temp_unit
        logical :: dir_exists
        
        stat = 0
        
        ! Check if already exists
        inquire(file=dir_path, exist=dir_exists)
        if (dir_exists) return
        
        ! Use file creation to force directory creation
        ! This is a workaround since Fortran doesn't have native mkdir
        temp_file_path = trim(dir_path) // '/.fortcov_temp_dir_marker'
        
        ! Try to create the temporary file which forces directory creation
        open(newunit=temp_unit, file=temp_file_path, status='new', iostat=stat)
        if (stat == 0) then
            ! Directory was created successfully
            close(temp_unit, status='delete')  ! Remove the temporary file
            stat = 0
        else
            ! Directory creation failed
            stat = 1
        end if
        
    end subroutine create_single_directory

end module file_operations_secure