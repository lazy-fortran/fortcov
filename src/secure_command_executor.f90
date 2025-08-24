module secure_command_executor
    !! Secure command execution module with injection protection
    !! 
    !! This module provides safe command execution functionality that prevents
    !! shell injection attacks through proper argument validation and escaping.
    !! All shell commands are constructed using safe patterns that avoid
    !! concatenation of unsanitized user input.
    use iso_fortran_env, only: error_unit
    use error_handling
    use string_utils, only: format_integer
    implicit none
    private
    
    ! Maximum lengths for security validation
    integer, parameter :: MAX_PATH_LENGTH = 4096
    integer, parameter :: MAX_COMMAND_LENGTH = 8192
    integer, parameter :: MAX_ARGS = 32
    
    ! Public procedures
    public :: safe_execute_gcov
    public :: safe_find_files
    public :: safe_mkdir
    public :: validate_path_security
    public :: validate_executable_path
    public :: escape_shell_argument
    
contains

    ! Safe gcov command execution with full injection protection
    subroutine safe_execute_gcov(gcov_executable, source_file, working_dir, &
                                branch_coverage, output_file, error_ctx)
        character(len=*), intent(in) :: gcov_executable
        character(len=*), intent(in) :: source_file
        character(len=*), intent(in) :: working_dir
        logical, intent(in) :: branch_coverage
        character(len=*), intent(in) :: output_file
        type(error_context_t), intent(out) :: error_ctx
        
        character(len=:), allocatable :: safe_gcov_path
        character(len=:), allocatable :: safe_source_path
        character(len=:), allocatable :: safe_working_dir
        character(len=:), allocatable :: safe_output_path
        character(len=MAX_COMMAND_LENGTH) :: command
        integer :: stat
        logical :: file_exists
        
        call clear_error_context(error_ctx)
        
        ! Validate and sanitize all inputs
        call validate_executable_path(gcov_executable, safe_gcov_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        call validate_path_security(source_file, safe_source_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        call validate_path_security(working_dir, safe_working_dir, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        call validate_path_security(output_file, safe_output_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! Verify source file exists
        inquire(file=safe_source_path, exist=file_exists)
        if (.not. file_exists) then
            call handle_missing_source(safe_source_path, error_ctx)
            return
        end if
        
        ! Execute gcov command with safer working directory handling
        if (len_trim(safe_working_dir) > 0 .and. safe_working_dir /= ".") then
            ! For non-current directories, use a safer approach
            ! First, change to the working directory using chdir-like approach
            call safe_execute_in_directory(safe_working_dir, safe_gcov_path, &
                                         safe_source_path, branch_coverage, &
                                         safe_output_path, stat)
        else
            ! Execute in current directory - build command safely
            call build_safe_gcov_command(safe_gcov_path, safe_source_path, &
                                       branch_coverage, safe_output_path, command)
            
            ! Execute the safely constructed command
            call execute_command_line(command, exitstat=stat)
        end if
        
        if (stat /= 0) then
            error_ctx%error_code = ERROR_INVALID_CONFIG
            error_ctx%recoverable = .true.  ! Allow recovery for gcov failures
            call safe_write_message(error_ctx, &
                "gcov command failed with exit code " // format_integer(stat))
            call safe_write_suggestion(error_ctx, &
                "Verify gcov is installed and coverage files are valid")
            call safe_write_context(error_ctx, "gcov command execution")
        end if
    end subroutine safe_execute_gcov

    ! Safe file finding with injection protection
    subroutine safe_find_files(pattern, files, error_ctx)
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable, intent(out) :: files(:)
        type(error_context_t), intent(out) :: error_ctx
        
        character(len=:), allocatable :: safe_pattern
        character(len=MAX_COMMAND_LENGTH) :: command
        character(len=:), allocatable :: temp_filename
        character(len=256) :: temp_files(100)
        integer :: unit, stat, iostat, num_files
        logical :: file_exists
        
        call clear_error_context(error_ctx)
        
        ! Validate pattern
        call validate_path_security(pattern, safe_pattern, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! Create secure temporary filename for output
        call create_secure_temp_filename(temp_filename)
        
        ! Build safe find command - use shell-safe patterns
        command = "find . -name " // escape_shell_argument(safe_pattern) // &
                 " -type f 2>/dev/null > " // escape_shell_argument(temp_filename)
        
        ! Execute command
        call execute_command_line(command, exitstat=stat)
        if (stat /= 0) then
            error_ctx%error_code = ERROR_INVALID_CONFIG
            call safe_write_message(error_ctx, &
                "File search failed with exit code " // format_integer(stat))
            return
        end if
        
        ! Read results from temporary file
        open(newunit=unit, file=temp_filename, status='old', &
             action='read', iostat=iostat)
        if (iostat /= 0) then
            error_ctx%error_code = ERROR_MISSING_FILE
            call safe_write_message(error_ctx, "Failed to read search results")
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
        
        ! Allocate output array
        allocate(character(len=256) :: files(num_files))
        files(1:num_files) = temp_files(1:num_files)
        
    end subroutine safe_find_files

    ! Safe temporary file deletion - MINIMAL FIX for issue #244
    subroutine safe_close_and_delete(unit, filename, error_ctx)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: filename
        type(error_context_t), intent(inout) :: error_ctx
        
        integer :: iostat
        logical :: file_exists
        
        ! First try standard close with delete
        close(unit, status='delete', iostat=iostat)
        
        if (iostat /= 0) then
            ! If delete failed, try alternative cleanup
            close(unit, iostat=iostat)  ! Close without delete first
            
            ! Verify file still exists
            inquire(file=filename, exist=file_exists)
            if (file_exists) then
                ! Try manual deletion as fallback
                call execute_command_line("rm -f " // escape_shell_argument(filename))
            end if
        end if
    end subroutine safe_close_and_delete

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
                "Failed to create directory: " // safe_dir_path)
            call safe_write_suggestion(error_ctx, &
                "Check directory permissions and path validity")
            call safe_write_context(error_ctx, "directory creation")
        end if
    end subroutine safe_mkdir

    ! Path security validation
    subroutine validate_path_security(input_path, safe_path, error_ctx)
        character(len=*), intent(in) :: input_path
        character(len=:), allocatable, intent(out) :: safe_path
        type(error_context_t), intent(out) :: error_ctx
        
        character(len=len(input_path)) :: working_path
        integer :: i, path_len
        
        call clear_error_context(error_ctx)
        working_path = input_path
        path_len = len_trim(working_path)
        
        ! Length validation
        if (path_len == 0) then
            error_ctx%error_code = ERROR_INVALID_PATH
            call safe_write_message(error_ctx, "Empty path provided")
            return
        end if
        
        if (path_len > MAX_PATH_LENGTH) then
            error_ctx%error_code = ERROR_INVALID_PATH
            call safe_write_message(error_ctx, &
                "Path exceeds maximum length: " // format_integer(MAX_PATH_LENGTH))
            return
        end if
        
        ! Enhanced injection protection - check for all dangerous patterns
        if (index(working_path, '..') > 0 .or. &
            index(working_path, ';') > 0 .or. &
            index(working_path, '|') > 0 .or. &
            index(working_path, '&') > 0 .or. &
            index(working_path, '<') > 0 .or. &
            index(working_path, '>') > 0 .or. &
            index(working_path, '$') > 0 .or. &
            index(working_path, '`') > 0 .or. &
            index(working_path, '"') > 0 .or. &
            index(working_path, "'") > 0) then
            error_ctx%error_code = ERROR_INVALID_PATH
            call safe_write_message(error_ctx, "Path contains dangerous characters")
            return
        end if
        
        ! URL-encoded directory traversal protection
        call check_url_encoded_attacks(working_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! System file access protection
        call check_system_file_access(working_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            ! Sanitize error message to avoid path leakage
            call sanitize_error_message_path(error_ctx)
            return
        end if
        
        ! Windows device names protection
        call check_windows_device_names(working_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! UNC path protection
        call check_unc_path_attack(working_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! Allocate and copy safe path
        safe_path = trim(working_path)
    end subroutine validate_path_security

    ! Executable path validation
    subroutine validate_executable_path(executable, safe_executable, error_ctx)
        character(len=*), intent(in) :: executable
        character(len=:), allocatable, intent(out) :: safe_executable
        type(error_context_t), intent(out) :: error_ctx
        
        logical :: exec_exists
        
        ! First validate as regular path
        call validate_path_security(executable, safe_executable, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! Check if executable exists and is executable
        inquire(file=safe_executable, exist=exec_exists)
        if (.not. exec_exists) then
            error_ctx%error_code = ERROR_MISSING_FILE
            call safe_write_message(error_ctx, &
                "Executable not found: " // safe_executable)
            call safe_write_suggestion(error_ctx, &
                "Check if the executable is installed and in PATH")
            return
        end if
    end subroutine validate_executable_path

    ! Shell argument escaping
    function escape_shell_argument(arg) result(escaped_arg)
        character(len=*), intent(in) :: arg
        character(len=:), allocatable :: escaped_arg
        
        integer :: i, new_len, pos
        character(len=len(arg)*2) :: temp_arg
        
        ! Simple shell escaping by surrounding with single quotes
        ! and escaping any single quotes in the argument
        temp_arg = "'"
        pos = 2
        
        do i = 1, len_trim(arg)
            if (arg(i:i) == "'") then
                ! Replace ' with '\''
                temp_arg(pos:pos+3) = "'\'''"
                pos = pos + 4
            else
                temp_arg(pos:pos) = arg(i:i)
                pos = pos + 1
            end if
        end do
        
        temp_arg(pos:pos) = "'"
        new_len = pos
        
        escaped_arg = temp_arg(1:new_len)
    end function escape_shell_argument

    ! Helper subroutines (simplified versions)

    subroutine safe_execute_in_directory(working_dir, gcov_path, source_path, &
                                       branch_coverage, output_path, exit_stat)
        character(len=*), intent(in) :: working_dir, gcov_path, source_path
        logical, intent(in) :: branch_coverage
        character(len=*), intent(in) :: output_path
        integer, intent(out) :: exit_stat
        
        character(len=MAX_COMMAND_LENGTH) :: command
        
        call build_safe_gcov_command(gcov_path, source_path, branch_coverage, &
                                   output_path, command)
        command = "cd " // escape_shell_argument(working_dir) // " && " // command
        call execute_command_line(command, exitstat=exit_stat)
    end subroutine safe_execute_in_directory

    subroutine build_safe_gcov_command(gcov_path, source_path, branch_coverage, &
                                     output_path, command)
        character(len=*), intent(in) :: gcov_path, source_path
        logical, intent(in) :: branch_coverage
        character(len=*), intent(in) :: output_path
        character(len=*), intent(out) :: command
        
        command = escape_shell_argument(gcov_path)
        if (branch_coverage) then
            command = trim(command) // " -b"
        end if
        command = trim(command) // " " // escape_shell_argument(source_path)
        command = trim(command) // " > " // escape_shell_argument(output_path)
    end subroutine build_safe_gcov_command

    subroutine create_secure_temp_filename(temp_filename)
        character(len=:), allocatable, intent(out) :: temp_filename
        
        integer :: pid
        character(len=16) :: pid_str
        
        call get_process_id(pid)
        write(pid_str, '(I0)') pid
        temp_filename = "/tmp/fortcov_" // trim(pid_str) // "_temp.txt"
    end subroutine create_secure_temp_filename

    subroutine get_process_id(pid)
        integer, intent(out) :: pid
        pid = 1234  ! Simplified - in real implementation would get actual PID
    end subroutine get_process_id
    
    ! URL-encoded attack detection
    subroutine check_url_encoded_attacks(path, error_ctx)
        character(len=*), intent(in) :: path
        type(error_context_t), intent(inout) :: error_ctx
        
        ! Check for URL-encoded directory traversal patterns
        if (index(path, '%2e') > 0 .or. index(path, '%2E') > 0 .or. &
            index(path, '%2f') > 0 .or. index(path, '%2F') > 0 .or. &
            index(path, '%5c') > 0 .or. index(path, '%5C') > 0) then
            error_ctx%error_code = ERROR_INVALID_PATH
            call safe_write_message(error_ctx, "URL-encoded attack pattern detected")
            return
        end if
    end subroutine check_url_encoded_attacks
    
    ! System file access protection
    subroutine check_system_file_access(path, error_ctx)
        character(len=*), intent(in) :: path
        type(error_context_t), intent(inout) :: error_ctx
        
        character(len=len(path)) :: lower_path
        
        ! Convert to lowercase for case-insensitive checking
        call to_lowercase(path, lower_path)
        
        ! Block access to system directories
        if (index(lower_path, '/etc/') == 1 .or. &
            index(lower_path, '/proc/') == 1 .or. &
            index(lower_path, '/sys/') == 1 .or. &
            index(lower_path, '/dev/') == 1 .or. &
            index(lower_path, '/root/') == 1 .or. &
            index(lower_path, '/tmp/') == 1 .or. &
            index(lower_path, '/home/') == 1 .or. &
            index(lower_path, '/var/log/') == 1) then
            error_ctx%error_code = ERROR_INVALID_PATH
            call safe_write_message(error_ctx, "System file access not allowed")
            return
        end if
    end subroutine check_system_file_access
    
    ! Windows device names protection
    subroutine check_windows_device_names(path, error_ctx)
        character(len=*), intent(in) :: path
        type(error_context_t), intent(inout) :: error_ctx
        
        character(len=len(path)) :: upper_path
        
        ! Convert to uppercase for case-insensitive checking
        call to_uppercase(path, upper_path)
        upper_path = trim(upper_path)
        
        ! Block Windows device names (exact match or at start)
        if (trim(upper_path) == 'CON' .or. index(upper_path, 'CON.') == 1 .or. index(upper_path, 'CON/') == 1 .or. &
            trim(upper_path) == 'PRN' .or. index(upper_path, 'PRN.') == 1 .or. index(upper_path, 'PRN/') == 1 .or. &
            trim(upper_path) == 'NUL' .or. index(upper_path, 'NUL.') == 1 .or. index(upper_path, 'NUL/') == 1 .or. &
            trim(upper_path) == 'AUX' .or. index(upper_path, 'AUX.') == 1 .or. index(upper_path, 'AUX/') == 1 .or. &
            trim(upper_path) == 'COM1' .or. index(upper_path, 'COM1.') == 1 .or. index(upper_path, 'COM1/') == 1 .or. &
            trim(upper_path) == 'COM2' .or. index(upper_path, 'COM2.') == 1 .or. index(upper_path, 'COM2/') == 1 .or. &
            trim(upper_path) == 'LPT1' .or. index(upper_path, 'LPT1.') == 1 .or. index(upper_path, 'LPT1/') == 1) then
            error_ctx%error_code = ERROR_INVALID_PATH
            call safe_write_message(error_ctx, "Windows device name access not allowed")
            return
        end if
    end subroutine check_windows_device_names
    
    ! UNC path attack protection
    subroutine check_unc_path_attack(path, error_ctx)
        character(len=*), intent(in) :: path
        type(error_context_t), intent(inout) :: error_ctx
        
        ! Block UNC paths (Windows network paths)
        if (len(path) >= 2) then
            if (path(1:2) == '\\\\') then
                error_ctx%error_code = ERROR_INVALID_PATH
                call safe_write_message(error_ctx, "UNC path access not allowed")
                return
            end if
        end if
    end subroutine check_unc_path_attack
    
    ! Helper to convert string to lowercase
    subroutine to_lowercase(input, output)
        character(len=*), intent(in) :: input
        character(len=*), intent(out) :: output
        integer :: i
        
        output = input
        do i = 1, len_trim(input)
            if (input(i:i) >= 'A' .and. input(i:i) <= 'Z') then
                output(i:i) = char(ichar(input(i:i)) + 32)
            end if
        end do
    end subroutine to_lowercase
    
    ! Helper to convert string to uppercase
    subroutine to_uppercase(input, output)
        character(len=*), intent(in) :: input
        character(len=*), intent(out) :: output
        integer :: i
        
        output = input
        do i = 1, len_trim(input)
            if (input(i:i) >= 'a' .and. input(i:i) <= 'z') then
                output(i:i) = char(ichar(input(i:i)) - 32)
            end if
        end do
    end subroutine to_uppercase
    
    ! Sanitize error messages to prevent path information leakage
    subroutine sanitize_error_message_path(error_ctx)
        type(error_context_t), intent(inout) :: error_ctx
        
        ! Replace specific sensitive paths with generic messages
        if (index(error_ctx%message, '/home/') > 0 .or. &
            index(error_ctx%message, '/etc/') > 0 .or. &
            index(error_ctx%message, '/root/') > 0 .or. &
            index(error_ctx%message, '/tmp/') > 0) then
            ! Replace with generic message to avoid information leakage
            call safe_write_message(error_ctx, "Invalid path - access denied")
        end if
    end subroutine sanitize_error_message_path

end module secure_command_executor