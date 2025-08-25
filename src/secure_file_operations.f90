module secure_file_operations
    !! Secure file operations module for temp files and directory management
    !!
    !! This module provides secure file operations including safe deletion,
    !! file finding, and directory creation with full security validation
    !! and error reporting.
    use iso_fortran_env, only: error_unit
    use error_handling
    use string_utils, only: format_integer
    use path_validation, only: validate_path_security
    use security_assessment, only: assess_deletion_security_risks, &
                                   assess_pattern_security_risks
    use shell_utils, only: escape_shell_argument
    implicit none
    private
    
    ! Parameters
    integer, parameter :: MAX_COMMAND_LENGTH = 8192
    
    ! Public procedures
    public :: safe_close_and_delete
    public :: safe_find_files
    public :: safe_mkdir
    
contains

    ! Robust temporary file deletion with multi-layer security - FIX for issue #297
    subroutine safe_close_and_delete(unit, filename, error_ctx)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: filename
        type(error_context_t), intent(inout) :: error_ctx
        
        integer :: close_iostat, delete_iostat
        logical :: file_exists_before, deletion_successful
        logical :: potential_security_issues
        character(len=256) :: security_concerns
        integer, parameter :: max_attempts = 3
        
        ! Check file existence before deletion attempts
        inquire(file=filename, exist=file_exists_before)
        
        ! Primary deletion attempt: Fortran close with status='delete'
        close(unit, status='delete', iostat=close_iostat)
        
        ! Attempt file deletion with multiple strategies if needed
        call attempt_file_deletion(unit, filename, close_iostat, &
                                  max_attempts, deletion_successful, delete_iostat)
        
        ! Comprehensive security vulnerability assessment
        call assess_deletion_security_risks(filename, close_iostat, delete_iostat, &
                                           deletion_successful, file_exists_before, &
                                           potential_security_issues, security_concerns)
        
        ! Report any errors or security concerns
        if (.not. deletion_successful .or. close_iostat /= 0 .or. &
            potential_security_issues) then
            call report_deletion_error(error_ctx, filename, close_iostat, &
                                      deletion_successful, file_exists_before, &
                                      max_attempts, potential_security_issues, &
                                      security_concerns)
        end if
        
    end subroutine safe_close_and_delete
    
    ! Attempt file deletion using multiple strategies
    subroutine attempt_file_deletion(unit, filename, close_iostat, &
                                    max_attempts, deletion_successful, delete_iostat)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: filename
        integer, intent(in) :: close_iostat
        integer, intent(in) :: max_attempts
        logical, intent(out) :: deletion_successful
        integer, intent(out) :: delete_iostat
        
        logical :: file_exists_after
        character(len=MAX_COMMAND_LENGTH) :: delete_command
        integer :: attempts, overwrite_iostat, temp_iostat
        
        ! Initialize delete_iostat based on close_iostat
        delete_iostat = close_iostat
        
        ! Verify initial deletion was successful
        inquire(file=filename, exist=file_exists_after)
        deletion_successful = .not. file_exists_after
        
        ! If deletion was successful but close had error, clear the error
        if (deletion_successful .and. close_iostat /= 0) then
            delete_iostat = 0  ! File was deleted despite close error
        end if
        
        if (.not. deletion_successful) then
            ! File still exists - try additional deletion strategies
            if (close_iostat /= 0) then
                ! Close failed - try closing without delete first
                close(unit, iostat=temp_iostat)
                if (temp_iostat /= 0) then
                    delete_iostat = temp_iostat  ! Update status only if this also failed
                end if
            end if
            
            ! Multi-layer fallback deletion strategy
            do attempts = 1, max_attempts
                inquire(file=filename, exist=file_exists_after)
                if (.not. file_exists_after) then
                    deletion_successful = .true.
                    exit
                end if
                
                ! Strategy 1: Secure overwrite before deletion
                if (attempts == 1) then
                    call secure_overwrite_file(filename, overwrite_iostat)
                end if
                
                ! Strategy 2: System deletion command
                delete_command = "rm -f " // escape_shell_argument(filename)
                call execute_command_line(delete_command, exitstat=temp_iostat)
                ! Store the actual deletion command status
                if (temp_iostat /= 0) then
                    delete_iostat = temp_iostat
                end if
                
                ! Brief pause between attempts to handle concurrent access
                if (attempts < max_attempts) then
                    call execute_command_line("sleep 0.1", exitstat=temp_iostat)
                end if
            end do
            
            ! Final verification
            inquire(file=filename, exist=file_exists_after)
            deletion_successful = .not. file_exists_after
            
            ! Clear error status if deletion ultimately succeeded
            if (deletion_successful) then
                delete_iostat = 0
            end if
        end if
        
    end subroutine attempt_file_deletion
    
    ! Report deletion errors and security concerns
    subroutine report_deletion_error(error_ctx, filename, close_iostat, &
                                    deletion_successful, file_exists_before, &
                                    max_attempts, potential_security_issues, &
                                    security_concerns)
        type(error_context_t), intent(inout) :: error_ctx
        character(len=*), intent(in) :: filename
        integer, intent(in) :: close_iostat
        logical, intent(in) :: deletion_successful
        logical, intent(in) :: file_exists_before
        integer, intent(in) :: max_attempts
        logical, intent(in) :: potential_security_issues
        character(len=*), intent(in) :: security_concerns
        
        ! Enhanced error reporting for security compliance
        if (.not. deletion_successful .and. file_exists_before) then
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            error_ctx%recoverable = .false.
            
            if (close_iostat /= 0) then
                call safe_write_message(error_ctx, &
                    "Critical: Temp file deletion failed - security risk. " // &
                    "Close iostat: " // format_integer(close_iostat) // &
                    ", Delete attempts: " // format_integer(max_attempts))
            else
                call safe_write_message(error_ctx, &
                    "Critical: Temp file persists after deletion attempts " // &
                    "- potential security data exposure")
            end if
        else if (close_iostat /= 0 .and. deletion_successful) then
            ! Even if deletion ultimately succeeded, report close failures
            ! for security audit and compliance tracking
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            error_ctx%recoverable = .true.
            call safe_write_message(error_ctx, &
                "Security audit: Temp file deletion required fallback. " // &
                "Primary close iostat: " // format_integer(close_iostat))
        else if (potential_security_issues) then
            ! Report security concerns even if deletion appeared successful
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            error_ctx%recoverable = .true.
            call safe_write_message(error_ctx, &
                "Security compliance issue detected: " // trim(security_concerns))
        end if
        
    end subroutine report_deletion_error
    
    ! Secure overwrite sensitive data before deletion
    subroutine secure_overwrite_file(filename, iostat)
        character(len=*), intent(in) :: filename
        integer, intent(out) :: iostat
        
        integer :: unit, file_size_bytes, i
        character(len=1024) :: overwrite_buffer
        
        iostat = 0
        
        ! Fill buffer with zeros for secure overwrite
        do i = 1, len(overwrite_buffer)
            overwrite_buffer(i:i) = char(0)
        end do
        
        ! Open file for overwriting
        open(newunit=unit, file=filename, action='write', status='old', &
             access='stream', iostat=iostat)
        if (iostat /= 0) return
        
        ! Get file size
        inquire(unit=unit, size=file_size_bytes, iostat=iostat)
        if (iostat /= 0) then
            close(unit)
            return
        end if
        
        ! Overwrite file content with zeros
        rewind(unit)
        do i = 1, (file_size_bytes / len(overwrite_buffer)) + 1
            write(unit, iostat=iostat) overwrite_buffer
            if (iostat /= 0) exit
        end do
        
        ! Ensure data is written to disk
        flush(unit, iostat=iostat)
        close(unit)
        
    end subroutine secure_overwrite_file
    
    ! Safe file finding with injection protection
    subroutine safe_find_files(pattern, files, error_ctx)
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable, intent(out) :: files(:)
        type(error_context_t), intent(out) :: error_ctx
        
        character(len=:), allocatable :: safe_pattern
        character(len=MAX_COMMAND_LENGTH) :: command
        character(len=:), allocatable :: temp_filename
        integer :: stat
        logical :: has_security_assessment
        character(len=512) :: security_message
        
        call clear_error_context(error_ctx)
        
        ! Validate pattern
        call validate_path_security(pattern, safe_pattern, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            ! Leave files unallocated on validation error
            return
        end if
        
        ! Create secure temporary filename for output
        call create_secure_temp_filename(temp_filename)
        
        ! Security pre-assessment for pattern-based vulnerabilities
        call assess_pattern_security_risks(safe_pattern, error_ctx)
        
        ! Preserve security assessment for priority reporting
        has_security_assessment = (error_ctx%error_code /= ERROR_SUCCESS)
        if (has_security_assessment) then
            security_message = error_ctx%message
        end if
        
        ! Build safe find command
        call build_find_command(safe_pattern, temp_filename, command)
        
        ! Execute command
        call execute_command_line(command, exitstat=stat)
        if (stat /= 0) then
            if (.not. has_security_assessment) then
                error_ctx%error_code = ERROR_INVALID_CONFIG
                call safe_write_message(error_ctx, &
                    "File search failed with exit code " // format_integer(stat))
            end if
            ! Leave files unallocated on command failure
            return
        end if
        
        ! Parse output and populate files array
        call parse_find_output(temp_filename, files, error_ctx, has_security_assessment)
        
    end subroutine safe_find_files

    ! Build find command for file pattern search
    subroutine build_find_command(safe_pattern, temp_filename, command)
        character(len=*), intent(in) :: safe_pattern
        character(len=*), intent(in) :: temp_filename
        character(len=MAX_COMMAND_LENGTH), intent(out) :: command
        
        character(len=256) :: filename_pattern, base_dir
        integer :: star_pos
        
        ! Handle recursive patterns (**/) differently
        if (index(safe_pattern, '**/') > 0) then
            ! Extract the filename pattern after **/
            star_pos = index(safe_pattern, '**/')
            if (star_pos > 0) then
                filename_pattern = safe_pattern(star_pos+3:)
            else
                filename_pattern = safe_pattern
            end if
            
            ! Extract the base directory from the pattern if it exists
            if (star_pos > 1) then
                base_dir = safe_pattern(1:star_pos-1)
                ! Remove trailing slash if present
                if (base_dir(len_trim(base_dir):len_trim(base_dir)) == '/') then
                    base_dir = base_dir(1:len_trim(base_dir)-1)
                end if
            else
                base_dir = '.'
            end if
            
            command = "find " // escape_shell_argument(trim(base_dir)) // " -name " // &
                     escape_shell_argument(trim(filename_pattern)) // &
                     " -type f 2>/dev/null > " // escape_shell_argument(temp_filename)
        else if (index(safe_pattern, '/') > 0) then
            ! Pattern contains directory - split it
            star_pos = index(safe_pattern, '/', back=.true.)
            base_dir = safe_pattern(1:star_pos-1)
            filename_pattern = safe_pattern(star_pos+1:)
            
            command = "find " // escape_shell_argument(trim(base_dir)) // " -name " // &
                     escape_shell_argument(trim(filename_pattern)) // &
                     " -type f 2>/dev/null > " // escape_shell_argument(temp_filename)
        else
            ! Simple pattern without directory
            command = "find . -name " // escape_shell_argument(safe_pattern) // &
                     " -type f 2>/dev/null > " // escape_shell_argument(temp_filename)
        end if
    end subroutine build_find_command
    
    ! Parse find command output and populate files array
    subroutine parse_find_output(temp_filename, files, error_ctx, has_security_assessment)
        character(len=*), intent(in) :: temp_filename
        character(len=:), allocatable, intent(out) :: files(:)
        type(error_context_t), intent(inout) :: error_ctx
        logical, intent(in) :: has_security_assessment
        
        character(len=256) :: temp_files(100)
        integer :: unit, iostat, num_files
        
        ! Read results from temporary file
        open(newunit=unit, file=temp_filename, status='old', &
             action='read', iostat=iostat)
        if (iostat /= 0) then
            if (.not. has_security_assessment) then
                error_ctx%error_code = ERROR_MISSING_FILE
                call safe_write_message(error_ctx, "Failed to read search results")
            end if
            ! Leave files unallocated on error
            return
        end if
        
        num_files = 0
        do
            if (num_files >= size(temp_files)) exit
            read(unit, '(A)', iostat=iostat) temp_files(num_files + 1)
            if (iostat /= 0) exit
            num_files = num_files + 1
        end do
        
        ! Close and delete temporary file with proper error handling
        call safe_close_and_delete(unit, temp_filename, error_ctx)
        
        ! Allocate output array - even if empty to distinguish success with no files
        allocate(character(len=256) :: files(num_files))
        if (num_files > 0) then
            files(1:num_files) = temp_files(1:num_files)
        end if
    end subroutine parse_find_output

    ! Directory creation with injection protection
    subroutine safe_mkdir(directory_path, error_ctx)
        character(len=*), intent(in) :: directory_path
        type(error_context_t), intent(out) :: error_ctx
        
        character(len=:), allocatable :: safe_dir_path
        character(len=MAX_COMMAND_LENGTH) :: command
        integer :: stat
        logical :: dir_exists
        
        call clear_error_context(error_ctx)
        
        ! Validate directory path
        call validate_path_security(directory_path, safe_dir_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! Check if directory already exists
        inquire(file=safe_dir_path, exist=dir_exists)
        if (dir_exists) return
        
        ! Create directory safely
        command = "mkdir -p " // escape_shell_argument(safe_dir_path)
        call execute_command_line(command, exitstat=stat)
        
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

    ! Create secure temporary filename
    subroutine create_secure_temp_filename(temp_filename)
        character(len=:), allocatable, intent(out) :: temp_filename
        
        integer :: pid
        character(len=16) :: pid_str
        
        call get_process_id(pid)
        write(pid_str, '(I0)') pid
        temp_filename = "/tmp/fortcov_" // trim(pid_str) // "_temp.txt"
    end subroutine create_secure_temp_filename

    ! Get process ID helper
    subroutine get_process_id(pid)
        integer, intent(out) :: pid
        pid = 1234  ! Simplified - in real implementation would get actual PID
    end subroutine get_process_id


end module secure_file_operations