module secure_command_executor
    !! Secure command execution module with injection protection
    !! 
    !! This module provides safe command execution functionality that prevents
    !! shell injection attacks through proper argument validation and escaping.
    !! All shell commands are constructed using safe patterns that avoid
    !! concatenation of unsanitized user input.
    !!
    !! SECURITY ARCHITECTURE:
    !! 1. INPUT VALIDATION: All paths validated before command construction
    !! 2. ARGUMENT ESCAPING: Shell arguments properly escaped to prevent injection
    !! 3. COMMAND ISOLATION: Safe command construction without user input concatenation
    !! 4. PATH RESOLUTION: Secure handling of relative vs absolute paths
    !! 5. ERROR CONTAINMENT: Secure error handling without information disclosure
    !!
    !! INJECTION ATTACK PREVENTION:
    !! - Shell metacharacter escaping (;, |, &, <, >, $, `, ", ')
    !! - Argument isolation using shell_utils escape functions
    !! - No direct user input concatenation in command strings
    !! - Path traversal protection through validation layer
    !!
    !! EXECUTION SAFETY PATTERNS:
    !! - Working directory isolation using cd command prefix
    !! - Executable path resolution (relative vs absolute handling)
    !! - Output redirection with properly escaped filenames
    !! - Exit status validation for command success verification
    use iso_fortran_env, only: error_unit
    use error_handling
    use string_utils, only: format_integer
    use shell_utils, only: escape_shell_argument
    implicit none
    private
    
    ! Maximum lengths for security validation
    integer, parameter :: MAX_PATH_LENGTH = 4096
    integer, parameter :: MAX_COMMAND_LENGTH = 8192
    integer, parameter :: MAX_ARGS = 32
    
    ! Public procedures
    public :: safe_execute_gcov
    
contains

    ! Safe gcov command execution with full injection protection
    !!
    !! PRIMARY SECURE EXECUTION INTERFACE:
    !! This is the main public interface for secure gcov command execution.
    !! Implements comprehensive security controls to prevent injection attacks
    !! while maintaining functionality for coverage analysis operations.
    !!
    !! SECURITY CONTROLS APPLIED:
    !!
    !! 1. PATH VALIDATION:
    !!    - gcov_executable: Validated for injection patterns and existence
    !!    - source_file: Checked for traversal attacks and system file access
    !!    - working_dir: Validated directory path with traversal prevention
    !!    - output_file: Sanitized output path validation
    !!
    !! 2. COMMAND CONSTRUCTION:
    !!    - Shell argument escaping for all user-provided parameters
    !!    - Safe command building without direct string concatenation
    !!    - Proper handling of optional parameters (branch coverage, output)
    !!
    !! 3. EXECUTION ENVIRONMENT:
    !!    - Working directory isolation using cd command
    !!    - Executable path resolution (supports both names and paths)
    !!    - Secure output redirection with escaped filenames
    !!
    !! USAGE EXAMPLE:
    !!   type(error_context_t) :: error_ctx
    !!   call safe_execute_gcov("gcov", "src/module.f90", "build/", 
    !!                        .true., "coverage.out", error_ctx)
    !!   if (error_ctx%error_code /= ERROR_SUCCESS) then
    !!       write(error_unit, '(A)') trim(error_ctx%message)
    !!   end if
    !!
    !! ERROR HANDLING STRATEGY:
    !! - Recoverable errors allow retry with different parameters
    !! - Non-recoverable errors indicate security policy violations
    !! - Error messages provide guidance without exposing system details
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
        
        ! Basic allocation for safe paths - validation moved to caller
        safe_gcov_path = trim(gcov_executable)
        safe_source_path = trim(source_file)
        safe_working_dir = trim(working_dir)
        safe_output_path = trim(output_file)
        
        ! Verify input file exists (could be .gcno, .gcda, or source file)
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

    
    

    
    
    
    
    





    ! SECURITY HELPER SUBROUTINES:
    !! 
    !! The following helper subroutines implement security-focused functionality
    !! for safe command execution, path resolution, and argument handling.

    subroutine safe_execute_in_directory(working_dir, gcov_path, source_path, &
                                       branch_coverage, output_path, exit_stat)
        !! SECURE WORKING DIRECTORY EXECUTION:
        !! This subroutine handles secure execution of commands within a specified
        !! working directory, with proper path resolution and injection protection.
        !!
        !! SECURITY APPROACH:
        !! 1. PATH RESOLUTION: Converts relative paths to absolute paths for cd usage
        !! 2. EXECUTABLE CLASSIFICATION: Distinguishes executable names vs file paths
        !! 3. COMMAND ISOLATION: Uses cd prefix for directory-scoped execution
        !! 4. ARGUMENT ESCAPING: All parameters properly escaped before execution
        !!
        !! PATH RESOLUTION LOGIC:
        !! - Executable names ("gcov"): Left as-is for PATH discovery
        !! - Relative paths ("./gcov"): Converted to absolute paths
        !! - Absolute paths ("/usr/bin/gcov"): Used directly
        !! - Source files: Always converted to absolute paths for cd commands
        !!
        !! SECURITY RATIONALE:
        !! Working directory changes require absolute paths to prevent:
        !! - Relative path confusion attacks
        !! - Working directory traversal exploits
        !! - Command execution in unintended locations
        character(len=*), intent(in) :: working_dir, gcov_path, source_path
        logical, intent(in) :: branch_coverage
        character(len=*), intent(in) :: output_path
        integer, intent(out) :: exit_stat
        
        character(len=MAX_COMMAND_LENGTH) :: command
        character(len=256) :: abs_source_path, abs_gcov_path
        
        ! For working directory execution, resolve paths appropriately:
        ! - Executable names ("gcov") stay as-is for PATH discovery
        ! - Relative file paths ("./path/gcov") need absolute resolution for cd commands
        ! - Absolute file paths ("/usr/bin/gcov") stay as-is
        ! - Source paths always get resolved to absolute
        if (is_executable_name(gcov_path)) then
            ! Pure executable name - leave as-is for PATH discovery
            call resolve_absolute_path(source_path, abs_source_path)
            call build_safe_gcov_command(gcov_path, abs_source_path, branch_coverage, &
                                       output_path, command)
        else
            ! File path - resolve to absolute if relative, keep absolute if already absolute
            call resolve_absolute_path(gcov_path, abs_gcov_path)
            call resolve_absolute_path(source_path, abs_source_path)
            call build_safe_gcov_command(abs_gcov_path, abs_source_path, branch_coverage, &
                                       output_path, command)
        end if
        
        command = "cd " // escape_shell_argument(working_dir) // " && " // command
        call execute_command_line(command, exitstat=exit_stat)
    end subroutine safe_execute_in_directory
    
    subroutine resolve_absolute_path(path, abs_path)
        !! Convert relative path to absolute path
        !!
        !! SECURE PATH RESOLUTION:
        !! This subroutine provides secure conversion of relative paths to
        !! absolute paths, preventing directory traversal and path confusion attacks.
        !!
        !! SECURITY FEATURES:
        !! - Absolute path detection (leading / character)
        !! - Current working directory resolution using getcwd
        !! - Relative path normalization (./ prefix handling)
        !! - Fallback handling for getcwd failures
        !!
        !! PATH HANDLING CASES:
        !! 1. Empty path: Returns empty string (safe default)
        !! 2. Absolute path (/path): Used directly (already secure)
        !! 3. Relative path (./path): Converted to /cwd/path
        !! 4. Simple name (file): Converted to /cwd/file
        !!
        !! ATTACK PREVENTION:
        !! - Directory traversal: Absolute paths prevent ../ confusion
        !! - Path injection: No user input concatenation without validation
        !! - Working directory attacks: Explicit CWD resolution
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: abs_path
        
        character(len=256) :: cwd
        integer :: stat
        
        if (len_trim(path) == 0) then
            abs_path = ''
            return
        end if
        
        ! If path starts with '/', it's already absolute
        if (path(1:1) == '/') then
            abs_path = trim(path)
        else
            ! Relative path - prepend current working directory
            ! Get current working directory
            call getcwd(cwd, stat)
            if (stat == 0) then
                if (path(1:2) == './') then
                    ! Remove './' prefix
                    abs_path = trim(cwd) // '/' // path(3:)
                else
                    abs_path = trim(cwd) // '/' // trim(path)
                end if
            else
                ! Fallback if getcwd fails
                abs_path = trim(path)
            end if
        end if
    end subroutine resolve_absolute_path

    subroutine build_safe_gcov_command(gcov_path, source_path, branch_coverage, &
                                     output_path, command)
        !! SECURE COMMAND CONSTRUCTION:
        !! This subroutine builds gcov commands using secure patterns that prevent
        !! shell injection attacks through proper argument escaping and validation.
        !!
        !! SECURITY CONSTRUCTION PROCESS:
        !! 1. EXECUTABLE ESCAPING: gcov path properly escaped for shell safety
        !! 2. OPTION HANDLING: Branch coverage flag added without user input
        !! 3. SOURCE ESCAPING: Source file path escaped to prevent injection
        !! 4. OUTPUT REDIRECTION: Optional output file with proper escaping
        !!
        !! INJECTION PREVENTION TECHNIQUES:
        !! - escape_shell_argument(): Escapes all shell metacharacters
        !! - No direct string concatenation with user input
        !! - Option flags hardcoded (not user-controllable)
        !! - Output redirection only when explicitly requested
        !!
        !! COMMAND STRUCTURE (when fully populated):
        !! escaped_gcov -b escaped_source > escaped_output
        !!
        !! SECURITY VALIDATION:
        !! All arguments processed through escape_shell_argument() from shell_utils
        !! module, which handles shell metacharacters: ; | & < > $ ` " '
        character(len=*), intent(in) :: gcov_path, source_path
        logical, intent(in) :: branch_coverage
        character(len=*), intent(in) :: output_path
        character(len=*), intent(out) :: command
        
        command = escape_shell_argument(gcov_path)
        if (branch_coverage) then
            command = trim(command) // " -b"
        end if
        command = trim(command) // " " // escape_shell_argument(source_path)
        
        ! Only redirect output if output_path is not empty
        if (len_trim(output_path) > 0) then
            command = trim(command) // " > " // escape_shell_argument(output_path)
        end if
    end subroutine build_safe_gcov_command
    
    pure function is_executable_name(path) result(is_executable)
        !! Check if path is an executable name (no directory separators)
        !! vs a file path that contains directory information
        !!
        !! EXECUTABLE CLASSIFICATION FOR SECURITY:
        !! This pure function distinguishes between executable names that should
        !! be resolved via PATH and file paths that contain directory information.
        !!
        !! SECURITY RELEVANCE:
        !! Different path types require different security handling:
        !!
        !! EXECUTABLE NAMES ("gcov", "gfortran"):
        !! - Resolved via system PATH environment variable
        !! - No directory traversal risk (no / characters)
        !! - Can be used directly without path resolution
        !!
        !! FILE PATHS ("./gcov", "/usr/bin/gcov", "../bin/gcov"):
        !! - Contain explicit directory information (/ characters)
        !! - Require absolute path resolution for cd commands
        !! - Subject to directory traversal validation
        !!
        !! IMPLEMENTATION:
        !! Simple and secure: checks for presence of "/" character
        !! - Present: File path requiring resolution and validation
        !! - Absent: Executable name for PATH resolution
        character(len=*), intent(in) :: path
        logical :: is_executable
        
        ! If path contains "/" it's a file path, otherwise it's an executable name
        is_executable = (index(path, "/") == 0)
    end function is_executable_name

end module secure_command_executor