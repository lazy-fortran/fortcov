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

    
    

    
    
    
    
    





    ! Helper subroutines (simplified versions)

    subroutine safe_execute_in_directory(working_dir, gcov_path, source_path, &
                                       branch_coverage, output_path, exit_stat)
        character(len=*), intent(in) :: working_dir, gcov_path, source_path
        logical, intent(in) :: branch_coverage
        character(len=*), intent(in) :: output_path
        integer, intent(out) :: exit_stat
        
        character(len=MAX_COMMAND_LENGTH) :: command, gcov_command
        character(len=512) :: gcov_command_path
        
        ! Handle relative gcov paths by using shell variable for original directory
        if (index(gcov_path, './') == 1 .or. &
            (gcov_path(1:1) /= '/' .and. index(gcov_path, '/') > 0)) then
            ! Relative path - preserve it relative to original directory
            gcov_command_path = '"$ORIG_DIR"/' // trim(gcov_path)
            call build_safe_gcov_command(gcov_command_path, source_path, branch_coverage, &
                                       output_path, gcov_command)
            command = "ORIG_DIR=$(pwd) && cd " // escape_shell_argument(working_dir) // &
                     " && " // trim(gcov_command)
        else
            ! Absolute path or command name - use as is
            call build_safe_gcov_command(gcov_path, source_path, branch_coverage, &
                                       output_path, gcov_command)
            command = "cd " // escape_shell_argument(working_dir) // " && " // trim(gcov_command)
        end if
        
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


    
    
    
    
    
    
    
    

end module secure_command_executor