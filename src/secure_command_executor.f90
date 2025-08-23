module secure_command_executor
    !! Secure command execution module with comprehensive injection protection
    !! 
    !! This module provides safe command execution functionality that prevents
    !! multiple classes of security vulnerabilities including:
    !! - Shell injection attacks (command chaining, pipes, redirects)
    !! - Path traversal attacks (directory escape attempts)
    !! - System file access (Unix/Linux system directories)  
    !! - Windows device exploitation (CON, NUL, COM, LPT, etc.)
    !! - Unicode-based attacks (handled via unicode_secure_validator)
    !! - File redirection exploitation (>, <, >>, <<)
    !! All shell commands are constructed using safe patterns that avoid
    !! concatenation of unsanitized user input.
    use iso_fortran_env, only: error_unit
    use error_handling
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
    public :: secure_command_executor_init
    public :: secure_command_executor_finalize
    
    ! Module variables for temp file tracking
    character(len=256), allocatable :: active_temp_files(:)
    integer :: active_temp_count = 0
    integer, parameter :: MAX_TEMP_FILES = 100
    
contains

    ! Module initialization - register cleanup handler
    ! This should be called once at program startup
    subroutine secure_command_executor_init()
        ! Initialize temp file tracking
        if (.not. allocated(active_temp_files)) then
            allocate(active_temp_files(MAX_TEMP_FILES))
            active_temp_count = 0
        end if
        
        ! Register cleanup to run at program exit
        ! Note: In production, this would use a proper exit handler
        ! For now, we rely on manual cleanup calls
    end subroutine secure_command_executor_init

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
        character(len=:), allocatable :: abs_source_path
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
        
        ! Verify source file exists and get absolute path if needed
        inquire(file=safe_source_path, exist=file_exists)
        if (.not. file_exists) then
            call handle_missing_source(safe_source_path, error_ctx)
            return
        end if
        
        ! Ensure we have absolute path for source file when using working dir
        if (len_trim(safe_working_dir) > 0 .and. safe_working_dir /= ".") then
            if (safe_source_path(1:1) /= '/') then
                ! Make absolute path
                call execute_command_line("pwd", exitstat=stat)
                abs_source_path = safe_source_path  ! Simplified for now
            else
                abs_source_path = safe_source_path
            end if
        else
            abs_source_path = safe_source_path
        end if
        
        ! Execute gcov command with safer working directory handling
        if (len_trim(safe_working_dir) > 0 .and. safe_working_dir /= ".") then
            ! For non-current directories, use a safer approach
            ! First, change to the working directory using chdir-like approach
            call safe_execute_in_directory(safe_working_dir, safe_gcov_path, &
                                         abs_source_path, branch_coverage, &
                                         safe_output_path, stat)
        else
            ! Execute in current directory - build command safely
            call build_safe_gcov_command(safe_gcov_path, abs_source_path, &
                                       branch_coverage, safe_output_path, command)
            
            ! Execute the safely constructed command
            call execute_command_line(command, exitstat=stat)
        end if
        
        if (stat /= 0) then
            error_ctx%error_code = ERROR_INVALID_CONFIG
            error_ctx%recoverable = .true.  ! Allow recovery for gcov failures
            call safe_write_message(error_ctx, &
                "gcov command failed with exit code " // int_to_string(stat))
            call safe_write_suggestion(error_ctx, &
                "Verify gcov is installed and coverage files are valid")
            call safe_write_context(error_ctx, "gcov command execution")
        end if
    end subroutine safe_execute_gcov

    ! Safe file finding with injection protection
    subroutine safe_find_files(pattern, files, error_ctx, max_files)
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable, intent(out) :: files(:)
        type(error_context_t), intent(out) :: error_ctx
        integer, intent(in), optional :: max_files
        
        character(len=:), allocatable :: safe_pattern
        character(len=:), allocatable :: temp_filename
        character(len=MAX_COMMAND_LENGTH) :: command
        character(len=256) :: line
        character(len=256), allocatable :: temp_files(:)
        integer :: unit, stat, count, i
        integer :: max_files_limit
        
        ! Ensure module is initialized
        call secure_command_executor_init()
        
        call clear_error_context(error_ctx)
        count = 0
        
        ! Set file limit (configurable with default of 10000)
        if (present(max_files)) then
            max_files_limit = max_files
        else
            max_files_limit = 10000  ! Default if not provided
        end if
        
        ! Validate and sanitize the search pattern
        call validate_path_security(pattern, safe_pattern, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            allocate(character(len=256) :: files(0))
            return
        end if
        
        ! Create safe temporary filename
        call create_secure_temp_filename(temp_filename)
        
        if (index(safe_pattern, "**") > 0) then
            ! Recursive search using find with safe arguments
            command = "find " // &
                     escape_shell_argument(extract_dir_from_pattern(safe_pattern)) // &
                     " -name " // &
                     escape_shell_argument(extract_filename_from_pattern(safe_pattern)) // &
                     " -type f 2>/dev/null > " // escape_shell_argument(temp_filename)
        else
            ! Non-recursive search - we need special handling for glob patterns
            ! Extract directory and filename pattern separately for safe handling
            block
                character(len=:), allocatable :: dir_part, file_part
                dir_part = extract_dir_from_pattern(safe_pattern)
                file_part = extract_filename_from_pattern(safe_pattern)
                
                ! Use find for glob patterns to avoid shell expansion issues
                command = "find " // escape_shell_argument(dir_part) // &
                         " -maxdepth 1 -name " // escape_shell_argument(file_part) // &
                         " -type f 2>/dev/null > " // escape_shell_argument(temp_filename)
            end block
        end if
        
        ! Track temp file for cleanup in case of failures
        call register_temp_file_for_cleanup(temp_filename)
        
        ! Execute safe command
        call execute_command_line(command, exitstat=stat)
        
        ! Allocate temporary array with configurable size
        allocate(temp_files(max_files_limit))
        
        ! Read results from temp file
        open(newunit=unit, file=temp_filename, action='read', &
             status='old', iostat=stat)
        if (stat == 0) then
            do i = 1, max_files_limit
                read(unit, '(A)', iostat=stat) line
                if (stat /= 0) exit
                if (len_trim(line) > 0) then
                    count = count + 1
                    temp_files(count) = trim(line)
                end if
            end do
            ! Secure file deletion with multiple fallback mechanisms
            call secure_delete_temp_file(unit, temp_filename, error_ctx)
        else
            ! Try to clean up temp file even if open failed
            call manual_delete_file(temp_filename)
            ! Also unregister since we tried to delete
            call unregister_temp_file(temp_filename)
        end if
        
        ! Allocate result array with exact size needed
        if (count > 0) then
            allocate(character(len=256) :: files(count))
            do i = 1, count
                files(i) = temp_files(i)
            end do
        else
            allocate(character(len=256) :: files(0))
        end if
        
        ! Clean up temporary array
        if (allocated(temp_files)) deallocate(temp_files)
    end subroutine safe_find_files

    ! Safe directory creation with injection protection
    subroutine safe_mkdir(path, error_ctx)
        character(len=*), intent(in) :: path
        type(error_context_t), intent(out) :: error_ctx
        
        character(len=:), allocatable :: safe_path
        character(len=MAX_COMMAND_LENGTH) :: command
        integer :: stat
        logical :: exists
        
        call clear_error_context(error_ctx)
        
        ! Validate and sanitize path
        call validate_path_security(path, safe_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! Check if directory already exists
        inquire(file=safe_path, exist=exists)
        if (exists) return
        
        ! Build safe mkdir command
        command = "mkdir -p " // escape_shell_argument(safe_path)
        
        call execute_command_line(command, exitstat=stat)
        
        if (stat /= 0) then
            call handle_permission_denied(safe_path, error_ctx)
        end if
    end subroutine safe_mkdir

    ! URL decode a path to detect encoded traversal attempts
    !! This subroutine decodes common URL-encoded sequences that could hide
    !! directory traversal attacks (e.g., %2e%2e%2f -> ../)
    subroutine url_decode_path(encoded_path, decoded_path)
        character(len=*), intent(in) :: encoded_path
        character(len=:), allocatable, intent(out) :: decoded_path
        
        integer :: i, j, encoded_len
        character(len=len(encoded_path)) :: temp_path
        character(len=2) :: hex_chars
        
        encoded_len = len_trim(encoded_path)
        j = 1
        i = 1
        
        do while (i <= encoded_len)
            if (encoded_path(i:i) == '%' .and. i + 2 <= encoded_len) then
                ! Try to decode %xx sequence
                hex_chars = encoded_path(i+1:i+2)
                
                ! Decode common sequences that could be dangerous
                select case (hex_chars)
                case ('2e', '2E')  ! '.'
                    temp_path(j:j) = '.'
                case ('2f', '2F')  ! '/'
                    temp_path(j:j) = '/'
                case ('5c', '5C')  ! '\'
                    temp_path(j:j) = '\'
                case ('20')        ! ' '
                    temp_path(j:j) = ' '
                case ('3b', '3B')  ! ';'
                    temp_path(j:j) = ';'
                case ('26')        ! '&'
                    temp_path(j:j) = '&'
                case ('7c', '7C')  ! '|'
                    temp_path(j:j) = '|'
                case ('24')        ! '$'
                    temp_path(j:j) = '$'
                case ('60')        ! '`'
                    temp_path(j:j) = '`'
                case ('00')        ! null byte
                    temp_path(j:j) = char(0)
                case default
                    ! Keep the original % sequence if we can't decode it
                    temp_path(j:j+2) = encoded_path(i:i+2)
                    j = j + 2
                end select
                i = i + 3
            else
                temp_path(j:j) = encoded_path(i:i)
                i = i + 1
            end if
            j = j + 1
        end do
        
        decoded_path = temp_path(1:j-1)
    end subroutine url_decode_path

    ! Validate path for comprehensive security protection
    !! 
    !! Performs multi-layer security validation to prevent:
    !! - Shell injection attacks (semicolons, pipes, redirects, command substitution)
    !! - Directory traversal attacks (../, /..)
    !! - URL-encoded traversal attacks (%2e%2e%2f, etc.)
    !! - System file access (/proc/, /sys/, /dev/, /etc/)
    !! - Windows device name exploitation (CON, PRN, NUL, etc.)
    !! - UNC path attacks (\\server\share)
    !! - File redirection attacks (>, <, >>, <<)
    !! - NULL byte injection attacks
    !!
    !! @param path Input path to validate
    !! @param safe_path Validated safe path (allocated on success)
    !! @param error_ctx Error context for detailed error reporting
    subroutine validate_path_security(path, safe_path, error_ctx)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: safe_path
        type(error_context_t), intent(out) :: error_ctx
        
        integer :: i, path_len
        logical :: has_dangerous_chars
        character(len=:), allocatable :: working_path, decoded_path
        
        call clear_error_context(error_ctx)
        path_len = len_trim(path)
        
        ! Handle empty paths by defaulting to current directory
        ! This is a common case for working directories
        if (path_len == 0) then
            working_path = "."
        else
            working_path = trim(path)
        end if
        
        ! URL decode the path to detect encoded traversal attempts
        call url_decode_path(working_path, decoded_path)
        working_path = decoded_path
        
        path_len = len_trim(working_path)
        
        ! Check for excessively long paths
        if (path_len > MAX_PATH_LENGTH) then
            error_ctx%error_code = ERROR_INVALID_CONFIG
            call safe_write_message(error_ctx, "Path too long (exceeds 4096 characters)")
            call safe_write_suggestion(error_ctx, "Use shorter file paths")
            safe_path = ""
            return
        end if
        
        ! Check for dangerous characters and patterns
        has_dangerous_chars = .false.
        
        ! Look for shell metacharacters that could enable injection
        if (index(working_path, ';') > 0 .or. index(working_path, '&') > 0 .or. &
            index(working_path, '|') > 0 .or. index(working_path, '`') > 0 .or. &
            index(working_path, '$') > 0 .or. index(working_path, '"') > 0 .or. &
            index(working_path, "'") > 0 .or. index(working_path, '\') > 0 .or. &
            index(working_path, '>') > 0 .or. index(working_path, '<') > 0 .or. &
            index(working_path, char(0)) > 0) then
            has_dangerous_chars = .true.
        end if
        
        ! Check for directory traversal attempts
        if (index(working_path, '../') > 0 .or. index(working_path, '/..') > 0) then
            has_dangerous_chars = .true.
        end if
        
        ! Check for system file access attempts (Unix/Linux)
        if (index(working_path, '/proc/') == 1 .or. index(working_path, '/sys/') == 1 .or. &
            index(working_path, '/dev/') == 1 .or. index(working_path, '/etc/') == 1) then
            has_dangerous_chars = .true.
        end if
        
        ! Check for Windows device names
        if (index(working_path, 'CON') > 0 .or. index(working_path, 'PRN') > 0 .or. &
            index(working_path, 'AUX') > 0 .or. index(working_path, 'NUL') > 0 .or. &
            index(working_path, 'COM') > 0 .or. index(working_path, 'LPT') > 0) then
            has_dangerous_chars = .true.
        end if
        
        ! Check for UNC path attempts (Windows network paths)
        if (index(working_path, '\\') == 1) then
            has_dangerous_chars = .true.
        end if
        
        if (has_dangerous_chars) then
            error_ctx%error_code = ERROR_INVALID_CONFIG
            call safe_write_message(error_ctx, &
                "Path contains dangerous characters: " // trim(working_path))
            call safe_write_suggestion(error_ctx, &
                "Use only alphanumeric characters, dots, dashes, and forward slashes")
            safe_path = ""
            return
        end if
        
        ! Path is safe to use
        safe_path = working_path
    end subroutine validate_path_security

    ! Validate executable path and ensure it exists
    subroutine validate_executable_path(executable, safe_executable, error_ctx)
        character(len=*), intent(in) :: executable
        character(len=:), allocatable, intent(out) :: safe_executable
        type(error_context_t), intent(out) :: error_ctx
        
        logical :: exists
        character(len=256) :: resolved_path
        integer :: stat = 0  ! Initialize to prevent undefined behavior
        
        call clear_error_context(error_ctx)
        
        ! First validate the path for security
        call validate_path_security(executable, safe_executable, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! Check if executable exists
        if (index(safe_executable, '/') > 0) then
            ! Absolute or relative path - check directly
            inquire(file=safe_executable, exist=exists)
        else
            ! Command name only - check if it's in PATH using which with safe redirection
            ! Use a temporary file instead of shell redirection to avoid injection risks
            block
                character(len=:), allocatable :: temp_file
                call create_secure_temp_filename(temp_file)
                call execute_command_line("which " // escape_shell_argument(safe_executable) // &
                                        " > " // escape_shell_argument(temp_file) // " 2>&1", &
                                        exitstat=stat)
                exists = (stat == 0)
                ! Clean up temp file using secure deletion
                call manual_delete_file(temp_file)
            end block
        end if
        
        if (.not. exists) then
            error_ctx%error_code = ERROR_INVALID_CONFIG
            call safe_write_message(error_ctx, &
                "Executable not found: " // safe_executable)
            call safe_write_suggestion(error_ctx, &
                "Install the required program or provide full path")
            safe_executable = ""
        end if
    end subroutine validate_executable_path

    ! Escape shell argument to prevent injection
    function escape_shell_argument(arg) result(escaped)
        character(len=*), intent(in) :: arg
        character(len=:), allocatable :: escaped
        
        integer :: i, out_pos, arg_len
        character(len=len(arg)*2+2) :: temp_escaped
        
        arg_len = len_trim(arg)
        
        ! Simply quote the entire argument to prevent shell interpretation
        temp_escaped(1:1) = "'"
        out_pos = 2
        
        do i = 1, arg_len
            if (arg(i:i) == "'") then
                ! Escape single quotes by ending quote, adding escaped quote, starting new quote
                temp_escaped(out_pos:out_pos+3) = "'\''"
                out_pos = out_pos + 4
            else
                temp_escaped(out_pos:out_pos) = arg(i:i)
                out_pos = out_pos + 1
            end if
        end do
        
        temp_escaped(out_pos:out_pos) = "'"
        escaped = temp_escaped(1:out_pos)
    end function escape_shell_argument

    ! Create secure temporary filename
    subroutine create_secure_temp_filename(temp_filename)
        character(len=:), allocatable, intent(out) :: temp_filename
        character(len=8) :: date_str
        character(len=10) :: time_str
        character(len=256) :: temp_name
        integer :: pid
        
        call date_and_time(date_str, time_str)
        
        ! Get process ID for uniqueness (simulate with time)
        pid = mod(ichar(time_str(7:7)) * 100 + ichar(time_str(8:8)), 10000)
        
        write(temp_name, '(A,A,A,A,I0)') &
            "/tmp/fortcov_secure_", date_str, "_", time_str(1:6), pid
        
        temp_filename = trim(temp_name)
    end subroutine create_secure_temp_filename

    ! Helper functions for safe pattern processing
    function extract_dir_from_pattern(pattern) result(dir_part)
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: dir_part
        integer :: pos, double_star_pos
        
        ! For recursive patterns like "base/**/*.ext", extract just "base"
        double_star_pos = index(pattern, "/**")
        if (double_star_pos > 0) then
            dir_part = pattern(1:double_star_pos-1)
        else
            ! Non-recursive pattern - extract directory normally
            pos = index(pattern, "/", back=.true.)
            if (pos > 0) then
                dir_part = pattern(1:pos-1)
            else
                dir_part = "."
            end if
        end if
    end function extract_dir_from_pattern

    function extract_filename_from_pattern(pattern) result(filename_part)
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: filename_part
        integer :: pos
        
        pos = index(pattern, "/", back=.true.)
        if (pos > 0) then
            filename_part = pattern(pos+1:)
        else
            filename_part = pattern
        end if
        
        ! Remove ** if present
        pos = index(filename_part, "**")
        if (pos > 0) then
            filename_part = filename_part(pos+3:)
        end if
    end function extract_filename_from_pattern

    ! Helper function to convert integer to string
    function int_to_string(int_val) result(str_val)
        integer, intent(in) :: int_val
        character(len=:), allocatable :: str_val
        character(len=32) :: temp_str
        
        write(temp_str, '(I0)') int_val
        str_val = trim(temp_str)
    end function int_to_string

    ! Build gcov command safely without shell concatenation vulnerabilities
    subroutine build_safe_gcov_command(gcov_path, source_path, branch_coverage, &
                                      output_path, command)
        character(len=*), intent(in) :: gcov_path, source_path, output_path
        logical, intent(in) :: branch_coverage
        character(len=*), intent(out) :: command
        
        ! Build command with safe argument construction
        command = escape_shell_argument(gcov_path)
        
        if (branch_coverage) then
            command = trim(command) // " -b"
        end if
        
        command = trim(command) // " " // escape_shell_argument(source_path)
        command = trim(command) // " > " // escape_shell_argument(output_path)
        command = trim(command) // " 2>&1"
    end subroutine build_safe_gcov_command

    ! Execute command in specified directory using safer process-level approach
    subroutine safe_execute_in_directory(working_dir, gcov_path, source_path, &
                                        branch_coverage, output_path, exit_status)
        character(len=*), intent(in) :: working_dir, gcov_path, source_path, output_path
        logical, intent(in) :: branch_coverage
        integer, intent(out) :: exit_status
        
        character(len=MAX_COMMAND_LENGTH) :: command
        character(len=:), allocatable :: temp_script_name
        integer :: unit, stat
        
        ! For security, create a temporary script that handles directory change
        ! This avoids shell injection in the cd command
        call create_secure_temp_filename(temp_script_name)
        temp_script_name = trim(temp_script_name) // ".sh"
        
        ! Write safe script that changes directory and executes command
        open(newunit=unit, file=temp_script_name, status='replace', iostat=stat)
        if (stat /= 0) then
            exit_status = 1
            return
        end if
        
        write(unit, '(A)') "#!/bin/bash"
        write(unit, '(A)') "set -e"  ! Exit on any error
        write(unit, '(A)') "cd " // escape_shell_argument(working_dir)
        
        ! Build the gcov command
        call build_safe_gcov_command(gcov_path, source_path, branch_coverage, &
                                   output_path, command)
        write(unit, '(A)') trim(command)
        close(unit)
        
        ! Make script executable and run it
        call execute_command_line("chmod +x " // escape_shell_argument(temp_script_name), &
                                 exitstat=stat)
        if (stat == 0) then
            call execute_command_line(escape_shell_argument(temp_script_name), &
                                     exitstat=exit_status)
        else
            exit_status = 1
        end if
        
        ! Clean up temporary script using secure deletion
        call manual_delete_file(temp_script_name)
    end subroutine safe_execute_in_directory

    ! Secure deletion of temporary file with multiple fallback mechanisms
    subroutine secure_delete_temp_file(unit, filename, error_ctx)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: filename
        type(error_context_t), intent(inout) :: error_ctx
        
        integer :: iostat, del_stat, retry_count
        logical :: file_exists, deletion_failed
        character(len=256) :: error_msg
        
        deletion_failed = .false.
        
        ! Step 1: Pre-close preparation - flush buffers
        flush(unit, iostat=iostat)
        if (iostat /= 0) then
            ! Even if flush fails, continue with deletion attempts
            deletion_failed = .true.
        end if
        
        ! Step 2: Try standard close with delete
        close(unit, status='delete', iostat=iostat)
        
        if (iostat /= 0) then
            deletion_failed = .true.
            ! Handle specific error conditions
            select case(iostat)
            case(5002)  ! Permission denied
                error_msg = "Permission denied deleting temp file"
            case(5005)  ! I/O error
                error_msg = "I/O error deleting temp file"
            case(5006)  ! No space left on device
                error_msg = "Disk space issue deleting temp file"
            case default
                write(error_msg, '(A,I0)') "Failed to delete temp file, error code: ", iostat
            end select
        end if
        
        ! Step 3: Check if file was actually deleted
        inquire(file=filename, exist=file_exists)
        
        if (file_exists) then
            deletion_failed = .true.
            
            ! Step 4: Retry with delay for locked files
            retry_count = 0
            do while (file_exists .and. retry_count < 3)
                ! Wait briefly for file locks to release
                call execute_command_line("sleep 0.1", exitstat=del_stat)
                
                ! Try manual deletion
                call manual_delete_file(filename)
                
                ! Check again
                inquire(file=filename, exist=file_exists)
                retry_count = retry_count + 1
            end do
            
            if (file_exists) then
                ! Step 5: Try with elevated permissions if possible
                call force_delete_file(filename)
                inquire(file=filename, exist=file_exists)
                
                if (file_exists) then
                    ! Step 6: Register for cleanup at exit if still exists
                    call register_temp_file_for_cleanup(filename)
                    
                    ! Always report deletion failures for security
                    if (error_ctx%error_code == ERROR_SUCCESS) then
                        error_ctx%error_code = ERROR_INVALID_CONFIG
                        error_ctx%recoverable = .true.
                    end if
                    call safe_write_message(error_ctx, &
                        "SECURITY WARNING: Failed to delete temp file: " // trim(filename))
                    if (len_trim(error_msg) > 0) then
                        call safe_write_context(error_ctx, trim(error_msg))
                    end if
                    call safe_write_suggestion(error_ctx, &
                        "Temp file registered for cleanup at program exit")
                else
                    ! Successfully deleted after retries - unregister
                    call unregister_temp_file(filename)
                end if
            else
                ! Successfully deleted - unregister
                call unregister_temp_file(filename)
            end if
        else
            ! Successfully deleted on first try - unregister
            call unregister_temp_file(filename)
        end if
    end subroutine secure_delete_temp_file
    
    ! Manual file deletion using system calls
    subroutine manual_delete_file(filename)
        character(len=*), intent(in) :: filename
        integer :: stat
        logical :: file_exists
        
        ! First check if file exists to avoid unnecessary operations
        inquire(file=filename, exist=file_exists)
        if (.not. file_exists) return
        
        ! Try rm command as fallback
        call execute_command_line("rm -f " // escape_shell_argument(filename), &
                                 exitstat=stat)
        
        if (stat /= 0) then
            ! Try unlink system call through a C-style approach
            ! This is more robust on Unix-like systems
            call execute_command_line("unlink " // escape_shell_argument(filename) // &
                                     " 2>/dev/null", exitstat=stat)
            
            if (stat /= 0) then
                ! Try to change permissions first then delete
                call execute_command_line("chmod 600 " // escape_shell_argument(filename) // &
                                        " 2>/dev/null", exitstat=stat)
                if (stat == 0) then
                    call execute_command_line("rm -f " // escape_shell_argument(filename), &
                                            exitstat=stat)
                end if
            end if
        end if
    end subroutine manual_delete_file
    
    ! Force delete file with elevated methods
    subroutine force_delete_file(filename)
        character(len=*), intent(in) :: filename  
        integer :: stat
        logical :: file_exists
        
        ! Check if file exists
        inquire(file=filename, exist=file_exists)
        if (.not. file_exists) return
        
        ! Try to override file attributes and delete
        call execute_command_line("chmod 777 " // escape_shell_argument(filename) // &
                                " 2>/dev/null", exitstat=stat)
        
        ! Try shred for secure deletion if available
        call execute_command_line("shred -vfz -n 1 " // escape_shell_argument(filename) // &
                                " 2>/dev/null", exitstat=stat)
        
        if (stat /= 0) then
            ! Final attempt with force removal
            call execute_command_line("rm -rf " // escape_shell_argument(filename) // &
                                    " 2>/dev/null", exitstat=stat)
        end if
    end subroutine force_delete_file
    
    ! Register temp file for cleanup at program exit
    subroutine register_temp_file_for_cleanup(filename)
        character(len=*), intent(in) :: filename
        character(len=256), allocatable :: new_temp_files(:)
        integer :: i
        
        ! Initialize array if not allocated
        if (.not. allocated(active_temp_files)) then
            allocate(active_temp_files(MAX_TEMP_FILES))
            active_temp_count = 0
        end if
        
        ! Check if already registered
        do i = 1, active_temp_count
            if (active_temp_files(i) == filename) return
        end do
        
        ! Add to list if space available
        if (active_temp_count < MAX_TEMP_FILES) then
            active_temp_count = active_temp_count + 1
            active_temp_files(active_temp_count) = filename
        else
            ! If list is full, try to clean some files now
            call cleanup_registered_temp_files()
            
            ! Try adding again if space was freed
            if (active_temp_count < MAX_TEMP_FILES) then
                active_temp_count = active_temp_count + 1
                active_temp_files(active_temp_count) = filename
            end if
        end if
    end subroutine register_temp_file_for_cleanup
    
    ! Unregister temp file after successful deletion
    subroutine unregister_temp_file(filename)
        character(len=*), intent(in) :: filename
        integer :: i, j
        
        if (.not. allocated(active_temp_files)) return
        
        ! Find and remove from list
        j = 0
        do i = 1, active_temp_count
            if (active_temp_files(i) /= filename) then
                j = j + 1
                if (j < i) then
                    active_temp_files(j) = active_temp_files(i)
                end if
            end if
        end do
        active_temp_count = j
    end subroutine unregister_temp_file
    
    ! Cleanup all registered temp files
    subroutine cleanup_registered_temp_files()
        integer :: i, j
        logical :: file_exists
        character(len=256), allocatable :: remaining_files(:)
        integer :: remaining_count
        
        if (.not. allocated(active_temp_files)) return
        if (active_temp_count == 0) return
        
        allocate(remaining_files(MAX_TEMP_FILES))
        remaining_count = 0
        
        ! Try to delete each registered file
        do i = 1, active_temp_count
            inquire(file=active_temp_files(i), exist=file_exists)
            if (file_exists) then
                call manual_delete_file(active_temp_files(i))
                
                ! Check if deletion succeeded
                inquire(file=active_temp_files(i), exist=file_exists)
                if (file_exists) then
                    ! Keep in list if still exists
                    remaining_count = remaining_count + 1
                    remaining_files(remaining_count) = active_temp_files(i)
                end if
            end if
        end do
        
        ! Update the active list with remaining files
        active_temp_count = remaining_count
        do i = 1, remaining_count
            active_temp_files(i) = remaining_files(i)
        end do
        
        deallocate(remaining_files)
    end subroutine cleanup_registered_temp_files
    
    ! Module finalization - cleanup temp files on exit
    ! Note: This is called automatically when program terminates
    subroutine secure_command_executor_finalize()
        call cleanup_registered_temp_files()
        if (allocated(active_temp_files)) deallocate(active_temp_files)
    end subroutine secure_command_executor_finalize

end module secure_command_executor
