module secure_command_executor
    !! Secure command execution module with injection protection
    !! 
    !! This module provides safe command execution functionality that prevents
    !! shell injection attacks through proper argument validation and escaping.
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
                "gcov command failed with exit code " // int_to_string(stat))
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
        character(len=:), allocatable :: temp_filename
        character(len=MAX_COMMAND_LENGTH) :: command
        character(len=256) :: line
        character(len=256) :: temp_files(100)
        integer :: unit, stat, count, i
        
        call clear_error_context(error_ctx)
        count = 0
        
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
        
        ! Execute safe command
        call execute_command_line(command, exitstat=stat)
        
        ! Read results from temp file
        open(newunit=unit, file=temp_filename, action='read', &
             status='old', iostat=stat)
        if (stat == 0) then
            do i = 1, 100
                read(unit, '(A)', iostat=stat) line
                if (stat /= 0) exit
                if (len_trim(line) > 0) then
                    count = count + 1
                    temp_files(count) = trim(line)
                end if
            end do
            close(unit, status='delete')
        end if
        
        ! Allocate result array
        if (count > 0) then
            allocate(character(len=256) :: files(count))
            do i = 1, count
                files(i) = temp_files(i)
            end do
        else
            allocate(character(len=256) :: files(0))
        end if
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

    ! Validate path for security - prevent directory traversal and injection
    subroutine validate_path_security(path, safe_path, error_ctx)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: safe_path
        type(error_context_t), intent(out) :: error_ctx
        
        integer :: i, path_len
        logical :: has_dangerous_chars
        character(len=:), allocatable :: working_path
        
        call clear_error_context(error_ctx)
        path_len = len_trim(path)
        
        ! Handle empty paths by defaulting to current directory
        ! This is a common case for working directories
        if (path_len == 0) then
            working_path = "."
        else
            working_path = trim(path)
        end if
        
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
            index(working_path, char(0)) > 0) then
            has_dangerous_chars = .true.
        end if
        
        ! Check for directory traversal attempts
        if (index(working_path, '../') > 0 .or. index(working_path, '/..') > 0) then
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
            ! Command name only - check if it's in PATH using which
            call execute_command_line("which " // escape_shell_argument(safe_executable) // &
                                    " >/dev/null 2>&1", exitstat=stat)
            exists = (stat == 0)
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
        
        ! Clean up temporary script
        call execute_command_line("rm -f " // escape_shell_argument(temp_script_name), &
                                 exitstat=stat)
    end subroutine safe_execute_in_directory

end module secure_command_executor
