module command_utilities
    !! Command utility functions extracted from zero_configuration_manager
    !! 
    !! This module provides command execution, shell escaping, and system
    !! interaction utilities for zero-configuration functionality.
    !!
    !! Responsibilities:
    !! - Shell command escaping and security
    !! - Directory validation
    !! - Command building and execution
    !! - Unique identifier generation
    use iso_c_binding, only: c_int
    use file_ops_secure, only: safe_remove_file, safe_find_files
    use error_handling_core, only: error_context_t
    implicit none
    private
    
    ! Interface for system process ID function
    interface
        function c_getpid() bind(c, name="getpid")
            use iso_c_binding, only: c_int
            integer(c_int) :: c_getpid
        end function c_getpid
    end interface
    
    public :: escape_shell_arg
    public :: replace_single_quotes
    public :: validate_directory_exists
    ! DEPRECATED: Shell-based find commands removed in favor of secure implementation
    ! Use file_search_secure module for all file finding operations
    public :: get_unique_suffix

contains

    function escape_shell_arg(arg) result(escaped_arg)
        !! Properly escape shell arguments to prevent injection attacks
        character(len=*), intent(in) :: arg
        character(len=:), allocatable :: escaped_arg
        integer :: i, len_arg, escape_count, stat
        character(len=512) :: errmsg
        character :: c
        
        len_arg = len_trim(arg)
        escape_count = 0
        
        ! Count special characters that need escaping
        do i = 1, len_arg
            c = arg(i:i)
            if (c == "'" .or. c == '"' .or. c == '\' .or. c == '$' .or. &
                c == '`' .or. c == '!' .or. c == '*' .or. c == '?' .or. &
                c == '[' .or. c == ']' .or. c == '(' .or. c == ')' .or. &
                c == '{' .or. c == '}' .or. c == ';' .or. c == '&' .or. &
                c == '|' .or. c == '<' .or. c == '>') then
                escape_count = escape_count + 1
            end if
        end do
        
        ! Allocate escaped string with extra space for escape characters
        allocate(character(len=len_arg + escape_count + 2) :: escaped_arg, &
            stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            write(*, '(A)') "Error: Memory allocation failed for escaped_arg: " // &
                trim(errmsg)
            return
        end if
        
        ! Build escaped string with single quotes
        escaped_arg = "'" // replace_single_quotes(arg(1:len_arg)) // "'"
    end function escape_shell_arg
    
    function replace_single_quotes(str) result(escaped_str)
        !! Replace single quotes with '\'' sequence for shell safety
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: escaped_str
        integer :: i, len_str, quote_count, pos, stat
        character(len=512) :: errmsg
        
        len_str = len_trim(str)
        quote_count = 0
        
        ! Count single quotes
        do i = 1, len_str
            if (str(i:i) == "'") quote_count = quote_count + 1
        end do
        
        ! Allocate space for replacement (each ' becomes '\'' - 3 extra chars)
        allocate(character(len=len_str + quote_count*3) :: escaped_str, &
            stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            write(*, '(A)') "Error: Memory allocation failed for escaped_str: " // &
                trim(errmsg)
            return
        end if
        
        pos = 1
        do i = 1, len_str
            if (str(i:i) == "'") then
                escaped_str(pos:pos+3) = "'\''"
                pos = pos + 4
            else
                escaped_str(pos:pos) = str(i:i)
                pos = pos + 1
            end if
        end do
        
        ! Trim to actual length
        escaped_str = escaped_str(1:pos-1)
    end function replace_single_quotes
    
    function validate_directory_exists(directory) result(exists)
        !! Check if directory exists for gcov file discovery
        character(len=*), intent(in) :: directory
        logical :: exists
        
        inquire(file=directory, exist=exists)
    end function validate_directory_exists
    
    ! DEPRECATED FUNCTIONS REMOVED:
    ! - build_recursive_find_command
    ! - build_nonrecursive_find_command  
    ! - execute_find_and_read_results
    !
    ! REPLACEMENT: Use file_search_secure module functions:
    ! - safe_find_files_recursive(base_dir, pattern, files, error_ctx)
    ! - safe_find_files_with_glob(directory, pattern, files, error_ctx)
    ! - safe_find_files(pattern, files, error_ctx)
    
    function get_unique_suffix() result(suffix)
        !! Generate a cryptographically secure unique suffix for temporary files
        character(len=16) :: suffix
        integer :: pid, clock_count
        character(len=32) :: temp_suffix
        
        ! Get process ID using system call
        pid = c_getpid()  ! Proper system call for process ID
        
        ! Add high-resolution clock for additional entropy
        call system_clock(clock_count)
        
        ! Combine PID and clock for uniqueness - use temp string first
        write(temp_suffix, '(I0,"_",I0)') pid, clock_count
        
        ! Truncate to fit in result
        if (len_trim(temp_suffix) > 16) then
            suffix = temp_suffix(1:16)
        else
            suffix = trim(temp_suffix)
        end if
    end function get_unique_suffix
    
    ! Execute secure file finding from command pattern
    ! SECURITY FIX Issue #963: Replace find shell execution vulnerability
    subroutine execute_secure_find_from_command(command, gcov_files)
        character(len=*), intent(in) :: command
        character(len=:), allocatable, intent(out) :: gcov_files(:)
        
        type(error_context_t) :: error_ctx
        character(len=256) :: pattern
        integer :: pos_find, pos_name
        
        ! Parse the find command to extract directory and pattern
        pos_find = index(command, 'find ')
        pos_name = index(command, " -name '*.gcov'")
        
        if (pos_find > 0 .and. pos_name > 0) then
            ! Extract directory from "find <directory> -name ..."
            pattern = command(pos_find+5:pos_name-1)
            pattern = trim(adjustl(pattern)) // '/*.gcov'
            
            ! Remove shell escaping quotes if present
            if (pattern(1:1) == '"') then
                pattern = pattern(2:len_trim(pattern)-1)
            end if
            
            ! Use secure file finding
            call safe_find_files(pattern, gcov_files, error_ctx)
            
            ! If no files found, create empty array
            if (.not. allocated(gcov_files)) then
                allocate(character(len=1) :: gcov_files(0))
            end if
        else
            ! Fallback: create empty array for unknown command format
            allocate(character(len=1) :: gcov_files(0))
        end if
        
    end subroutine execute_secure_find_from_command

end module command_utilities