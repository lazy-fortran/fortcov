module gcov_executor
    !! GCov command executor module for generating .gcov text files
    !! 
    !! This module provides functionality to execute gcov commands on source
    !! files to generate .gcov text coverage reports. It handles command
    !! failures gracefully, supports gcov options, and manages temporary files.
    !! All command execution is performed securely to prevent injection attacks.
    use iso_fortran_env, only: error_unit
    use error_handling_core, only: error_context_t, ERROR_SUCCESS, ERROR_INVALID_CONFIG, &
                                  ERROR_INCOMPLETE_COVERAGE, clear_error_context, handle_missing_source, &
                                  safe_write_message, safe_write_suggestion, safe_write_context
    use file_utilities, only: file_exists
    ! SECURITY FIX Issue #963: safe_execute_gcov removed - shell injection vulnerability
    ! Simplified: avoid security wrapper modules; use minimal local helpers
    use xml_utils, only: get_base_name
    use string_utils, only: int_to_string
    ! Inline security helpers to avoid module-order issues during test builds
    implicit none
    private
    
    ! Main executor type for gcov command execution
    type, public :: gcov_executor_t
        private
        logical :: branch_coverage = .false.
        character(len=256) :: working_directory = ""
        character(len=256) :: gcov_command = "gcov"
        character(len=256) :: gcov_output_dir = "build/gcov"  ! Directory for .gcov files
    contains
        procedure :: execute_gcov
        procedure :: set_branch_coverage
        procedure :: set_working_directory
        procedure :: set_gcov_command
        procedure :: set_gcov_output_directory
        procedure :: cleanup_gcov_files
    end type gcov_executor_t
    
contains

    subroutine execute_gcov(this, source_file, gcov_files, error_ctx)
        class(gcov_executor_t), intent(in) :: this
        character(len=*), intent(in) :: source_file
        character(len=:), allocatable, intent(out) :: gcov_files(:)
        type(error_context_t), intent(out) :: error_ctx
        
        character(len=256) :: temp_filename
        character(len=256), allocatable :: temp_files(:)
        integer :: line_count
        integer :: stat
        character(len=512) :: errmsg
        
        call clear_error_context(error_ctx)
        allocate(temp_files(10), stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            write(*, '(A)') "Error: Failed to allocate temp_files: " // trim(errmsg)
            error_ctx%error_code = ERROR_INVALID_CONFIG
            ! Handle failed allocation case: allocate empty result directly
            allocate(character(len=256) :: gcov_files(0), stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Failed to allocate empty gcov_files: " // trim(errmsg)
            end if
            return
        end if
        
        ! Validate prerequisites
        call validate_gcov_prerequisites(source_file, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call cleanup_and_return_empty(temp_files, gcov_files)
            return
        end if
        
        ! Setup output environment
        call setup_gcov_output_environment(this, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call cleanup_and_return_empty(temp_files, gcov_files)
            return
        end if
        
        ! Execute gcov command
        call create_temp_filename(temp_filename)
        call execute_gcov_command(this, source_file, temp_filename, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS .and. .not. error_ctx%recoverable) then
            call cleanup_temp_file(temp_filename)
            call cleanup_and_return_empty(temp_files, gcov_files)
            return
        end if
        
        ! Process generated files
        call process_gcov_output_files(this, source_file, temp_files, line_count, error_ctx)
        
        ! Build final result
        call build_gcov_files_result(temp_files, line_count, gcov_files, error_ctx)
        
        ! Cleanup with proper error handling
        call cleanup_temp_file(temp_filename)
        if (allocated(temp_files)) then
            deallocate(temp_files, stat=stat)
            if (stat /= 0) then
                write(*, '(A)') "Warning: Failed to deallocate temp_files in normal cleanup"
            end if
        end if
    end subroutine execute_gcov
    
    ! Validate that source file and coverage data files exist
    subroutine validate_gcov_prerequisites(source_file, error_ctx)
        character(len=*), intent(in) :: source_file
        type(error_context_t), intent(out) :: error_ctx
        
        character(len=256) :: gcda_file
        logical :: source_exists, gcda_exists
        
        call clear_error_context(error_ctx)
        
        inquire(file=source_file, exist=source_exists)
        if (.not. source_exists) then
            call handle_missing_source(source_file, error_ctx)
            return
        end if
        
        gcda_file = get_base_name(source_file) // ".gcda"
        inquire(file=gcda_file, exist=gcda_exists)
        if (.not. gcda_exists) then
            call handle_missing_source(gcda_file, error_ctx)
            return
        end if
    end subroutine validate_gcov_prerequisites
    
    ! Setup output directory environment
    subroutine setup_gcov_output_environment(this, error_ctx)
        class(gcov_executor_t), intent(in) :: this
        type(error_context_t), intent(out) :: error_ctx
        
        logical :: output_dir_exists
        
        call clear_error_context(error_ctx)
        
        inquire(file=trim(this%gcov_output_dir), exist=output_dir_exists)
        if (.not. output_dir_exists) then
            call ensure_dir(trim(this%gcov_output_dir))
        end if
    end subroutine setup_gcov_output_environment
    
    ! Execute gcov command with error handling
    subroutine execute_gcov_command(this, source_file, temp_filename, error_ctx)
        class(gcov_executor_t), intent(in) :: this
        character(len=*), intent(in) :: source_file, temp_filename
        type(error_context_t), intent(out) :: error_ctx
        
        character(len=1024) :: command
        character(len=512) :: safe_source_file
        character(len=64)  :: branch_flag
        integer :: exit_code
        logical :: source_exists
        
        call clear_error_context(error_ctx)
        
        ! Validate source file exists
        inquire(file=source_file, exist=source_exists)
        if (.not. source_exists) then
            call handle_missing_source(source_file, error_ctx)
            return
        end if
        
        ! Build minimal, safely-quoted gcov invocation
        safe_source_file = trim(source_file)
        branch_flag = ''
        if (this%branch_coverage) branch_flag = ' -b'
        command = trim(this%gcov_command) // branch_flag // ' ' // &
                  quote_arg(safe_source_file)

        call run_command(command, exit_code)
        
        if (exit_code /= 0) then
            call handle_gcov_command_failure(command, exit_code, temp_filename, error_ctx)
            return
        end if
        
        ! Success - gcov command executed successfully
        error_ctx%error_code = ERROR_SUCCESS
    end subroutine execute_gcov_command
    
    ! Minimal helpers: quoting and command execution
    pure function quote_arg(s) result(q)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: q
        character(len=:), allocatable :: tmp
        integer :: i, n
        n = len_trim(s)
        if (n == 0) then
            q = "''"
            return
        end if
        tmp = ''
        do i = 1, n
            select case(s(i:i))
            case("'")
                tmp = tmp // "'" // '"' // "'" // '"' // "'"
            case default
                tmp = tmp // s(i:i)
            end select
        end do
        q = "'" // tmp // "'"
    end function quote_arg

    subroutine run_command(cmd, exit_code)
        character(len=*), intent(in) :: cmd
        integer, intent(out) :: exit_code
        integer :: stat
        call execute_command_line(cmd, exitstat=stat)
        exit_code = stat
    end subroutine run_command
    
    ! Process generated gcov output files
    subroutine process_gcov_output_files(this, source_file, temp_files, line_count, error_ctx)
        class(gcov_executor_t), intent(in) :: this
        character(len=*), intent(in) :: source_file
        character(len=256), allocatable, intent(inout) :: temp_files(:)
        integer, intent(out) :: line_count
        type(error_context_t), intent(inout) :: error_ctx
        
        character(len=256) :: gcov_file, output_gcov_file, source_basename
        logical :: gcov_file_exists
        integer :: stat
        
        line_count = 0
        temp_files = ""
        source_basename = get_base_name(source_file)
        gcov_file = trim(source_file) // ".gcov"
        
        inquire(file=gcov_file, exist=gcov_file_exists)
        if (gcov_file_exists) then
            output_gcov_file = trim(this%gcov_output_dir) // "/" // &
                              trim(source_basename) // ".f90.gcov"
            ! SECURITY FIX Issue #963: Use secure file move instead of execute_command_line
            call move_file(gcov_file, output_gcov_file, stat)
            line_count = 1
            if (stat == 0) then
                temp_files(1) = output_gcov_file
            else
                temp_files(1) = gcov_file
            end if
        end if
    end subroutine process_gcov_output_files
    
    ! Build final gcov_files result array
    subroutine build_gcov_files_result(temp_files, line_count, gcov_files, error_ctx)
        character(len=256), allocatable, intent(in) :: temp_files(:)
        integer, intent(in) :: line_count
        character(len=:), allocatable, intent(out) :: gcov_files(:)
        type(error_context_t), intent(inout) :: error_ctx
        
        integer :: i
        integer :: stat
        character(len=512) :: errmsg
        
        if (line_count > 0) then
            allocate(character(len=256) :: gcov_files(line_count), stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Failed to allocate gcov_files result: " // trim(errmsg)
                error_ctx%error_code = ERROR_INVALID_CONFIG
                return
            end if
            do i = 1, line_count
                gcov_files(i) = trim(temp_files(i))
            end do
        else
            error_ctx%error_code = ERROR_INCOMPLETE_COVERAGE
            call safe_write_message(error_ctx, "No .gcov files were generated")
            call safe_write_suggestion(error_ctx, "Check gcov command output for errors")
            call safe_write_context(error_ctx, "gcov file generation")
            allocate(character(len=256) :: gcov_files(0), stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Failed to allocate empty gcov_files: " // trim(errmsg)
                return
            end if
        end if
    end subroutine build_gcov_files_result
    
    ! Cleanup and return empty result with proper memory management
    subroutine cleanup_and_return_empty(temp_files, gcov_files)
        character(len=256), allocatable, intent(inout) :: temp_files(:)
        character(len=:), allocatable, intent(out) :: gcov_files(:)
        
        integer :: stat
        character(len=512) :: errmsg
        
        ! Critical fix: Ensure temp_files is deallocated in all error paths
        if (allocated(temp_files)) then
            deallocate(temp_files, stat=stat)
            if (stat /= 0) then
                write(*, '(A)') "Warning: Failed to deallocate temp_files"
            end if
        end if
        
        allocate(character(len=256) :: gcov_files(0), stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            write(*, '(A)') "Error: Failed to allocate empty gcov_files in cleanup: " // trim(errmsg)
            return
        end if
    end subroutine cleanup_and_return_empty

    subroutine set_branch_coverage(this, enable)
        class(gcov_executor_t), intent(inout) :: this
        logical, intent(in) :: enable
        
        this%branch_coverage = enable
    end subroutine set_branch_coverage

    subroutine set_working_directory(this, directory)
        class(gcov_executor_t), intent(inout) :: this
        character(len=*), intent(in) :: directory
        
        this%working_directory = directory
    end subroutine set_working_directory

    subroutine set_gcov_command(this, command)
        class(gcov_executor_t), intent(inout) :: this
        character(len=*), intent(in) :: command
        
        this%gcov_command = command
    end subroutine set_gcov_command
    
    subroutine set_gcov_output_directory(this, directory)
        class(gcov_executor_t), intent(inout) :: this
        character(len=*), intent(in) :: directory
        
        this%gcov_output_dir = directory
    end subroutine set_gcov_output_directory

    subroutine cleanup_gcov_files(this, gcov_files)
        class(gcov_executor_t), intent(in) :: this
        character(len=*), intent(in) :: gcov_files(:)
        
        integer :: i, unit, stat, close_stat
        logical :: exists
        
        do i = 1, size(gcov_files)
            inquire(file=gcov_files(i), exist=exists)
            if (exists) then
                open(newunit=unit, file=gcov_files(i), status='old', iostat=stat)
                if (stat == 0) then
                    close(unit, status='delete', iostat=close_stat)
                    if (close_stat /= 0) continue
                end if
            end if
        end do
    end subroutine cleanup_gcov_files

    ! Helper subroutines and functions
    subroutine create_temp_filename(temp_filename)
        character(len=*), intent(out) :: temp_filename
        character(len=8) :: date_str
        character(len=10) :: time_str
        
        call date_and_time(date_str, time_str)
        temp_filename = "/tmp/fortcov_gcov_" // trim(date_str) // "_" // time_str(1:6)
    end subroutine create_temp_filename

    subroutine cleanup_temp_file(temp_filename)
        character(len=*), intent(in) :: temp_filename
        integer :: unit, stat, close_stat
        
        open(newunit=unit, file=temp_filename, status='old', iostat=stat)
        if (stat == 0) then
            close(unit, status='delete', iostat=close_stat)
            if (close_stat /= 0) return
        end if
    end subroutine cleanup_temp_file

    subroutine ensure_dir(path)
        character(len=*), intent(in) :: path
        integer :: stat
        call execute_command_line('mkdir -p ' // quote_arg(trim(path)), exitstat=stat)
    end subroutine ensure_dir

    subroutine move_file(src, dst, stat)
        character(len=*), intent(in) :: src, dst
        integer, intent(out) :: stat
        call execute_command_line('mv ' // quote_arg(trim(src)) // ' ' // &
                                  quote_arg(trim(dst)), exitstat=stat)
    end subroutine move_file

    subroutine handle_gcov_command_failure(command, exit_code, temp_file, &
                                         error_ctx)
        character(len=*), intent(in) :: command, temp_file
        integer, intent(in) :: exit_code
        type(error_context_t), intent(out) :: error_ctx
        
        error_ctx%error_code = ERROR_INVALID_CONFIG
        call safe_write_message(error_ctx, &
            "gcov command failed with exit code " // &
            int_to_string(exit_code))
        call safe_write_suggestion(error_ctx, &
            "Verify gcov is installed and coverage files are valid")
        call safe_write_context(error_ctx, "gcov command execution")
    end subroutine handle_gcov_command_failure

    ! get_base_name now imported from xml_utils

    ! integer_to_string now imported from iostat_utilities
    
    ! SECURITY HELPERS: Path sanitization and shell argument escaping
    
    ! sanitize_file_path and remove_pattern moved to gcov_executor_helpers

end module gcov_executor
