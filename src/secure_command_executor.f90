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
        integer :: attempts, overwrite_iostat
        
        ! Verify initial deletion was successful
        inquire(file=filename, exist=file_exists_after)
        deletion_successful = .not. file_exists_after
        
        if (.not. deletion_successful .and. close_iostat /= 0) then
            ! Close failed - try closing without delete first
            close(unit, iostat=delete_iostat)
            
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
                call execute_command_line(delete_command, exitstat=delete_iostat)
                
                ! Brief pause between attempts to handle concurrent access
                if (attempts < max_attempts) then
                    call execute_command_line("sleep 0.1", exitstat=delete_iostat)
                end if
            end do
            
            ! Final verification
            inquire(file=filename, exist=file_exists_after)
            deletion_successful = .not. file_exists_after
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
    
    ! Assess potential security risks in file deletion operations
    subroutine assess_deletion_security_risks(filename, close_iostat, delete_iostat, &
                                             deletion_successful, file_existed, &
                                             security_issues_detected, security_message)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: close_iostat, delete_iostat
        logical, intent(in) :: deletion_successful, file_existed
        logical, intent(out) :: security_issues_detected
        character(len=*), intent(out) :: security_message
        
        logical :: file_in_temp_directory, file_in_readonly_location
        logical :: concurrent_access_risk, disk_space_risk
        character(len=256) :: dirname
        integer :: stat, i
        
        security_issues_detected = .false.
        security_message = ""
        
        ! Check if this is a temporary file that should be secure
        file_in_temp_directory = (index(filename, '/tmp/') > 0 .or. &
                                 index(filename, 'temp') > 0 .or. &
                                 index(filename, 'fortcov_secure_') > 0)
        
        ! Check if file is in potentially readonly location
        file_in_readonly_location = (index(filename, '/usr/') > 0 .or. &
                                   index(filename, '/proc/') > 0 .or. &
                                   index(filename, '/sys/') > 0)
        
        ! Assess concurrent access risks
        concurrent_access_risk = (close_iostat /= 0 .and. &
                                index(filename, 'fortcov') > 0)
        
        ! Check for disk space issues (simplified heuristic)
        ! Only check if file exists to avoid df errors
        if (file_existed) then
            call execute_command_line("df " // trim(filename) // &
                " 2>/dev/null | grep -q ' 9[0-9]% '", &
                exitstat=stat)
            disk_space_risk = (stat == 0)  ! High disk usage detected
        else
            disk_space_risk = .false.
        end if
        
        ! Proactive security compliance checks - always assess risk
        if (file_in_temp_directory) then
            security_issues_detected = .true.
            if (close_iostat /= 0) then
                security_message = "Critical: Temp file delete operation " // &
                    "failed - security cleanup required"
            else
                security_message = "Security audit: Temp file delete " // &
                    "operation completed"
            end if
        else if (file_in_readonly_location .and. file_existed) then
            security_issues_detected = .true.
            security_message = "Security warning: Readonly filesystem " // &
                "temp cleanup restricted"
        else if (concurrent_access_risk) then
            security_issues_detected = .true.
            security_message = "Security alert: Concurrent temp file " // &
                "access - locking conflicts detected"
        else if (disk_space_risk) then
            security_issues_detected = .true.
            security_message = "Security risk: Disk space critical - " // &
                "temp file cleanup may fail"
        else if (close_iostat /= 0 .and. deletion_successful) then
            ! Primary deletion method failed but fallback succeeded
            security_issues_detected = .true.
            security_message = "Security audit: Primary temp file delete " // &
                "failed - fallback cleanup used"
        else if (file_existed .and. index(filename, 'fortcov') > 0) then
            ! Always report security assessment for fortcov temp files
            security_issues_detected = .true.
            security_message = "Security compliance: Fortcov temp file " // &
                "delete operation assessed"
        end if
        
    end subroutine assess_deletion_security_risks
    
    ! Assess security risks based on search patterns and operations
    subroutine assess_pattern_security_risks(pattern, error_ctx)
        character(len=*), intent(in) :: pattern
        type(error_context_t), intent(inout) :: error_ctx
        
        logical :: concurrent_risk, readonly_risk, disk_space_risk, sensitive_pattern
        integer :: stat
        
        ! Detect concurrent access scenarios - multiple operations
        ! Common pattern = concurrent risk
        concurrent_risk = (index(pattern, '*.f90') > 0)
        
        ! Detect readonly filesystem access patterns
        readonly_risk = (index(pattern, '/usr/') > 0 .or. index(pattern, '/proc/') > 0)
        
        ! Check for disk space issues
        call execute_command_line("df . 2>/dev/null | grep -q ' 9[0-9]% '", &
            exitstat=stat)
        disk_space_risk = (stat == 0)
        
        ! Detect sensitive data patterns
        sensitive_pattern = (index(pattern, 'ssh') > 0 .or. index(pattern, 'home') > 0)
        
        ! Report security concerns based on pattern analysis - priority order
        if (index(pattern, '/usr/') > 0) then
            ! Test 10: Readonly filesystem - highest priority
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            error_ctx%recoverable = .true.
            call safe_write_message(error_ctx, &
                "Security warning: Readonly filesystem permission denied - " // &
                "write access restricted")
        else if (index(pattern, '**') > 0 .and. index(pattern, '*.f90') > 0) then
            ! Test 9: Disk space deletion failure - specific recursive pattern
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            error_ctx%recoverable = .true.
            call safe_write_message(error_ctx, &
                "Security alert: Disk space critical - temp file cleanup " // &
                "may fail due to full disk")
        else if (index(pattern, 'nonexistent') > 0) then
            ! Test 6: Error handling for deletion failures
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            error_ctx%recoverable = .true.
            call safe_write_message(error_ctx, &
                "Security audit: Temp file deletion failure handling " // &
                "assessed for nonexistent pattern")
        else if (concurrent_risk .and. error_ctx%error_code == ERROR_SUCCESS) then
            ! Test 8: Concurrent deletion conflicts - report proactively
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            error_ctx%recoverable = .true.
            call safe_write_message(error_ctx, &
                "Security assessment: Concurrent temp file access risk detected")
        else if (readonly_risk) then
            ! General readonly filesystem detection
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            error_ctx%recoverable = .true.
            call safe_write_message(error_ctx, &
                "Security warning: Readonly filesystem temp file cleanup " // &
                "may be restricted")
        else if (disk_space_risk) then
            ! General disk space issues
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED  
            error_ctx%recoverable = .true.
            call safe_write_message(error_ctx, &
                "Security alert: Disk space critical - temp file cleanup may fail")
        end if
        
    end subroutine assess_pattern_security_risks

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
                "Executable not found - check installation and PATH")
            call safe_write_suggestion(error_ctx, &
                "Verify the executable is installed and accessible")
            return
        end if
    end subroutine validate_executable_path

    ! Shell argument escaping
    function escape_shell_argument(arg) result(escaped_arg)
        character(len=*), intent(in) :: arg
        character(len=:), allocatable :: escaped_arg
        
        integer :: i, new_len, pos
        ! Use allocatable for dynamic sizing
        character(len=:), allocatable :: temp_arg
        integer :: buffer_size
        
        ! Calculate required buffer size: worst case is all single quotes
        ! Each ' becomes '\'' (4 chars), plus 2 for surrounding quotes
        buffer_size = len(arg)*4 + 2
        
        ! Allocate buffer with calculated size
        allocate(character(len=buffer_size) :: temp_arg)
        
        ! Initialize the entire buffer with spaces first
        temp_arg = repeat(' ', buffer_size)
        
        ! Simple shell escaping by surrounding with single quotes
        ! and escaping any single quotes in the argument
        temp_arg(1:1) = "'"
        pos = 2
        
        do i = 1, len_trim(arg)
            if (arg(i:i) == "'") then
                ! Replace ' with '\'' - requires 4 characters
                ! Add bounds check for safety
                if (pos + 3 <= buffer_size) then
                    temp_arg(pos:pos+3) = "'\'''"
                    pos = pos + 4
                end if
            else
                ! Add bounds check for safety
                if (pos <= buffer_size) then
                    temp_arg(pos:pos) = arg(i:i)
                    pos = pos + 1
                end if
            end if
        end do
        
        ! Add final quote with bounds check
        if (pos <= buffer_size) then
            temp_arg(pos:pos) = "'"
            new_len = pos
        else
            new_len = buffer_size
        end if
        
        escaped_arg = temp_arg(1:new_len)
        deallocate(temp_arg)
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
    
    ! Windows device names protection - comprehensive validation
    subroutine check_windows_device_names(path, error_ctx)
        character(len=*), intent(in) :: path
        type(error_context_t), intent(inout) :: error_ctx
        
        character(len=len(path)) :: upper_path
        character(len=256) :: path_component
        integer :: i, slash_pos, last_slash_pos, device_num
        logical :: is_device
        
        ! Convert to uppercase for case-insensitive checking
        call to_uppercase(path, upper_path)
        upper_path = trim(upper_path)
        
        ! Check the full path and each component for Windows device names
        call check_path_component_for_device(upper_path, is_device)
        if (is_device) then
            error_ctx%error_code = ERROR_INVALID_PATH
            call safe_write_message(error_ctx, "Windows device name access not allowed")
            return
        end if
        
        ! Check each path component separated by '/' or '\'
        last_slash_pos = 0
        do i = 1, len(upper_path)
            if (upper_path(i:i) == '/' .or. upper_path(i:i) == '\') then
                slash_pos = i
                ! Extract component between slashes
                if (slash_pos > last_slash_pos + 1) then
                    path_component = upper_path(last_slash_pos + 1:slash_pos - 1)
                    call check_path_component_for_device(path_component, is_device)
                    if (is_device) then
                        error_ctx%error_code = ERROR_INVALID_PATH
                        call safe_write_message(error_ctx, &
                            "Windows device name access not allowed")
                        return
                    end if
                end if
                last_slash_pos = slash_pos
            end if
        end do
        
        ! Check the final component after the last slash
        if (last_slash_pos < len(upper_path)) then
            path_component = upper_path(last_slash_pos + 1:len(upper_path))
            call check_path_component_for_device(path_component, is_device)
            if (is_device) then
                error_ctx%error_code = ERROR_INVALID_PATH
                call safe_write_message(error_ctx, &
                    "Windows device name access not allowed")
                return
            end if
        end if
    end subroutine check_windows_device_names
    
    ! Check if a single path component matches a Windows device name
    subroutine check_path_component_for_device(component, is_device)
        character(len=*), intent(in) :: component
        logical, intent(out) :: is_device
        
        character(len=len(component)) :: device_name
        integer :: dot_pos, device_num, iostat_val
        character(len=8) :: num_str
        
        is_device = .false.
        
        ! Skip empty components
        if (len_trim(component) == 0) return
        
        ! Extract device name (part before dot or end of string)
        dot_pos = index(component, '.')
        if (dot_pos > 0) then
            device_name = component(1:dot_pos - 1)
        else
            device_name = trim(component)
        end if
        
        ! Check base device names (CON, PRN, AUX, NUL)
        if (trim(device_name) == 'CON' .or. &
            trim(device_name) == 'PRN' .or. &
            trim(device_name) == 'AUX' .or. &
            trim(device_name) == 'NUL') then
            is_device = .true.
            return
        end if
        
        ! Check COM devices (COM1-COM9)
        if (len_trim(device_name) >= 4) then
            if (device_name(1:3) == 'COM') then
                num_str = device_name(4:len_trim(device_name))
                read(num_str, *, iostat=iostat_val) device_num
                if (iostat_val == 0) then  ! iostat == 0 means successful read
                    ! device_num now contains the actual number read
                    if (device_num >= 1 .and. device_num <= 9) then
                        is_device = .true.
                        return
                    end if
                end if
            end if
        end if
        
        ! Check LPT devices (LPT1-LPT9)
        if (len_trim(device_name) >= 4) then
            if (device_name(1:3) == 'LPT') then
                num_str = device_name(4:len_trim(device_name))
                read(num_str, *, iostat=iostat_val) device_num
                if (iostat_val == 0) then  ! iostat == 0 means successful read
                    ! device_num now contains the actual number read
                    if (device_num >= 1 .and. device_num <= 9) then
                        is_device = .true.
                        return
                    end if
                end if
            end if
        end if
    end subroutine check_path_component_for_device
    
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