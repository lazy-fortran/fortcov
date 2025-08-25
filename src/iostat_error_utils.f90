module iostat_error_utils
    !! IOSTAT Error Interpretation and Handling Module
    !! 
    !! Provides granular error handling for Fortran iostat operations
    !! with meaningful error messages and actionable suggestions.
    use iso_fortran_env, only: error_unit
    use error_handling
    implicit none
    private
    
    ! Common iostat error codes (compiler-specific but widely used)
    integer, parameter :: IOSTAT_SUCCESS = 0
    integer, parameter :: IOSTAT_EOR = -1           ! End-of-record
    integer, parameter :: IOSTAT_EOF = -2           ! End-of-file
    integer, parameter :: IOSTAT_FORMAT_ERROR = 5010
    integer, parameter :: IOSTAT_NO_SUCH_FILE = 29
    integer, parameter :: IOSTAT_PERMISSION_DENIED = 13
    integer, parameter :: IOSTAT_FILE_EXISTS = 17
    integer, parameter :: IOSTAT_NO_SPACE = 28
    integer, parameter :: IOSTAT_BAD_FILE_DESCRIPTOR = 9
    integer, parameter :: IOSTAT_INVALID_ARGUMENT = 22
    integer, parameter :: IOSTAT_TOO_MANY_FILES = 24
    
    ! Public procedures
    public :: interpret_iostat_open_error
    public :: interpret_iostat_read_error  
    public :: interpret_iostat_write_error
    public :: interpret_iostat_close_error
    public :: get_iostat_error_message
    public :: handle_file_operation_error
    
contains

    subroutine interpret_iostat_open_error(iostat_code, filename, error_ctx)
        !! Interpret iostat errors from file open operations
        integer, intent(in) :: iostat_code
        character(len=*), intent(in) :: filename
        type(error_context_t), intent(out) :: error_ctx
        
        call clear_error_context(error_ctx)
        
        if (iostat_code == IOSTAT_SUCCESS) return
        
        select case (iostat_code)
        case (IOSTAT_NO_SUCH_FILE)
            error_ctx%error_code = ERROR_MISSING_FILE
            call safe_write_message(error_ctx, &
                "File not found: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "1. Check file path and spelling" // char(10) // &
                "2. Verify file exists: ls -la " // trim(filename) // char(10) // &
                "3. Check current directory: pwd" // char(10) // &
                "4. Use absolute path if needed")
            call safe_write_context(error_ctx, "File open operation")
            
        case (IOSTAT_PERMISSION_DENIED)
            error_ctx%error_code = ERROR_PERMISSION_DENIED
            call safe_write_message(error_ctx, &
                "Permission denied accessing: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "1. Check file permissions: ls -la " // trim(filename) // char(10) // &
                "2. Make file readable: chmod 644 " // trim(filename) // char(10) // &
                "3. Check directory permissions for parent directory" // char(10) // &
                "4. Run with appropriate user privileges")
            call safe_write_context(error_ctx, "File open operation")
            
        case (IOSTAT_NO_SPACE)
            error_ctx%error_code = ERROR_OUT_OF_MEMORY
            call safe_write_message(error_ctx, &
                "No space left on device when opening: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "1. Free up disk space: df -h" // char(10) // &
                "2. Remove temporary files: rm -f /tmp/*" // char(10) // &
                "3. Choose different output location" // char(10) // &
                "4. Check available space: du -sh .")
            call safe_write_context(error_ctx, "File open operation")
            
        case (IOSTAT_TOO_MANY_FILES)
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, &
                "Too many files open simultaneously")
            call safe_write_suggestion(error_ctx, &
                "1. Close unused files in your program" // char(10) // &
                "2. Check system limits: ulimit -n" // char(10) // &
                "3. Increase file descriptor limit" // char(10) // &
                "4. Process files in smaller batches")
            call safe_write_context(error_ctx, "File open operation")
            
        case (IOSTAT_FILE_EXISTS)
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, &
                "File already exists: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "1. Remove existing file: rm " // trim(filename) // char(10) // &
                "2. Use different filename" // char(10) // &
                "3. Use status='replace' to overwrite" // char(10) // &
                "4. Backup existing file first")
            call safe_write_context(error_ctx, "File open operation")
            
        case default
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, &
                "File open failed with iostat=" // integer_to_string(iostat_code) // &
                " for file: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "1. Check system error messages" // char(10) // &
                "2. Verify file accessibility" // char(10) // &
                "3. Check system resources" // char(10) // &
                "4. Try with different file location")
            call safe_write_context(error_ctx, "File open operation")
        end select
        
        error_ctx%recoverable = .false.
    end subroutine interpret_iostat_open_error
    
    subroutine interpret_iostat_read_error(iostat_code, filename, error_ctx)
        !! Interpret iostat errors from file read operations
        integer, intent(in) :: iostat_code
        character(len=*), intent(in) :: filename
        type(error_context_t), intent(out) :: error_ctx
        
        call clear_error_context(error_ctx)
        
        if (iostat_code == IOSTAT_SUCCESS) return
        
        select case (iostat_code)
        case (IOSTAT_EOF)
            error_ctx%error_code = ERROR_EMPTY_FILE
            call safe_write_message(error_ctx, &
                "Unexpected end-of-file reading: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "1. Check file content: cat " // trim(filename) // char(10) // &
                "2. Verify file is complete and not truncated" // char(10) // &
                "3. Check if file was generated properly" // char(10) // &
                "4. Try regenerating the source file")
            call safe_write_context(error_ctx, "File read operation")
            error_ctx%recoverable = .true.
            
        case (IOSTAT_EOR)
            error_ctx%error_code = ERROR_INVALID_DATA
            call safe_write_message(error_ctx, &
                "Unexpected end-of-record reading: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "1. Check file format and structure" // char(10) // &
                "2. Verify data matches expected format" // char(10) // &
                "3. Check for missing line terminators" // char(10) // &
                "4. Review read format specifier")
            call safe_write_context(error_ctx, "File read operation")
            error_ctx%recoverable = .false.
            
        case (IOSTAT_FORMAT_ERROR)
            error_ctx%error_code = ERROR_INVALID_DATA
            call safe_write_message(error_ctx, &
                "Format error reading: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "1. Check data format matches read statement" // char(10) // &
                "2. Verify numeric data is properly formatted" // char(10) // &
                "3. Check for invalid characters in data" // char(10) // &
                "4. Use list-directed I/O for flexible parsing")
            call safe_write_context(error_ctx, "File read operation")
            error_ctx%recoverable = .false.
            
        case (IOSTAT_BAD_FILE_DESCRIPTOR)
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, &
                "Bad file descriptor reading: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "1. Check if file was properly opened" // char(10) // &
                "2. Verify file unit is valid" // char(10) // &
                "3. Check if file was already closed" // char(10) // &
                "4. Reopen file before reading")
            call safe_write_context(error_ctx, "File read operation")
            error_ctx%recoverable = .false.
            
        case default
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, &
                "File read failed with iostat=" // integer_to_string(iostat_code) // &
                " for file: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "1. Check file integrity and accessibility" // char(10) // &
                "2. Verify file is not locked by another process" // char(10) // &
                "3. Check available memory for read operation" // char(10) // &
                "4. Try reading file in smaller chunks")
            call safe_write_context(error_ctx, "File read operation")
        end select
        
        error_ctx%recoverable = (iostat_code == IOSTAT_EOF)
    end subroutine interpret_iostat_read_error
    
    subroutine interpret_iostat_write_error(iostat_code, filename, error_ctx)
        !! Interpret iostat errors from file write operations
        integer, intent(in) :: iostat_code
        character(len=*), intent(in) :: filename
        type(error_context_t), intent(out) :: error_ctx
        
        call clear_error_context(error_ctx)
        
        if (iostat_code == IOSTAT_SUCCESS) return
        
        select case (iostat_code)
        case (IOSTAT_NO_SPACE)
            error_ctx%error_code = ERROR_OUT_OF_MEMORY
            call safe_write_message(error_ctx, &
                "No space left on device writing: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "1. Free up disk space: df -h" // char(10) // &
                "2. Remove temporary files" // char(10) // &
                "3. Choose different output location" // char(10) // &
                "4. Use compression if possible")
            call safe_write_context(error_ctx, "File write operation")
            
        case (IOSTAT_PERMISSION_DENIED)
            error_ctx%error_code = ERROR_PERMISSION_DENIED
            call safe_write_message(error_ctx, &
                "Permission denied writing: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "1. Check directory write permissions" // char(10) // &
                "2. Make directory writable: chmod 755 $(dirname " // &
                trim(filename) // ")" // char(10) // &
                "3. Choose different output directory" // char(10) // &
                "4. Run with appropriate user privileges")
            call safe_write_context(error_ctx, "File write operation")
            
        case (IOSTAT_BAD_FILE_DESCRIPTOR)
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, &
                "Bad file descriptor writing: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "1. Check if file was properly opened for writing" // char(10) // &
                "2. Verify file unit is valid" // char(10) // &
                "3. Check if file was closed prematurely" // char(10) // &
                "4. Reopen file with write access")
            call safe_write_context(error_ctx, "File write operation")
            
        case (IOSTAT_FORMAT_ERROR)
            error_ctx%error_code = ERROR_INVALID_DATA
            call safe_write_message(error_ctx, &
                "Format error writing: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "1. Check write format specifier" // char(10) // &
                "2. Verify data types match format" // char(10) // &
                "3. Check for invalid characters in output" // char(10) // &
                "4. Use list-directed output for flexibility")
            call safe_write_context(error_ctx, "File write operation")
            
        case default
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, &
                "File write failed with iostat=" // integer_to_string(iostat_code) // &
                " for file: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "1. Check available disk space" // char(10) // &
                "2. Verify write permissions" // char(10) // &
                "3. Check if file is locked" // char(10) // &
                "4. Try different output location")
            call safe_write_context(error_ctx, "File write operation")
        end select
        
        error_ctx%recoverable = .false.
    end subroutine interpret_iostat_write_error
    
    subroutine interpret_iostat_close_error(iostat_code, filename, error_ctx)
        !! Interpret iostat errors from file close operations
        integer, intent(in) :: iostat_code
        character(len=*), intent(in) :: filename
        type(error_context_t), intent(out) :: error_ctx
        
        call clear_error_context(error_ctx)
        
        if (iostat_code == IOSTAT_SUCCESS) return
        
        select case (iostat_code)
        case (IOSTAT_BAD_FILE_DESCRIPTOR)
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, &
                "Bad file descriptor closing: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "1. Check if file was already closed" // char(10) // &
                "2. Verify file unit is valid" // char(10) // &
                "3. Check for double close operations" // char(10) // &
                "4. This may not affect program execution")
            call safe_write_context(error_ctx, "File close operation")
            error_ctx%recoverable = .true.
            
        case (IOSTAT_NO_SPACE)
            error_ctx%error_code = ERROR_OUT_OF_MEMORY
            call safe_write_message(error_ctx, &
                "No space left on device closing: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "1. File buffer flush failed - check disk space" // char(10) // &
                "2. File may not be completely written" // char(10) // &
                "3. Verify file content after close" // char(10) // &
                "4. Free up disk space before retry")
            call safe_write_context(error_ctx, "File close operation")
            
        case default
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, &
                "File close failed with iostat=" // integer_to_string(iostat_code) // &
                " for file: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "1. Check system error messages" // char(10) // &
                "2. Verify file operations completed" // char(10) // &
                "3. Check file integrity" // char(10) // &
                "4. This may indicate system issues")
            call safe_write_context(error_ctx, "File close operation")
        end select
        
        error_ctx%recoverable = (iostat_code == IOSTAT_BAD_FILE_DESCRIPTOR)
    end subroutine interpret_iostat_close_error
    
    function get_iostat_error_message(iostat_code) result(message)
        !! Get brief error message for iostat code
        integer, intent(in) :: iostat_code
        character(len=:), allocatable :: message
        
        select case (iostat_code)
        case (IOSTAT_SUCCESS)
            message = "Success"
        case (IOSTAT_EOF)
            message = "End of file"
        case (IOSTAT_EOR)
            message = "End of record"
        case (IOSTAT_FORMAT_ERROR)
            message = "Format error"
        case (IOSTAT_NO_SUCH_FILE)
            message = "File not found"
        case (IOSTAT_PERMISSION_DENIED)
            message = "Permission denied"
        case (IOSTAT_FILE_EXISTS)
            message = "File already exists"
        case (IOSTAT_NO_SPACE)
            message = "No space left on device"
        case (IOSTAT_BAD_FILE_DESCRIPTOR)
            message = "Bad file descriptor"
        case (IOSTAT_INVALID_ARGUMENT)
            message = "Invalid argument"
        case (IOSTAT_TOO_MANY_FILES)
            message = "Too many open files"
        case default
            message = "Unknown iostat error"
        end select
    end function get_iostat_error_message
    
    subroutine handle_file_operation_error(operation, iostat_code, filename, &
                                         error_ctx)
        !! Generic file operation error handler
        character(len=*), intent(in) :: operation
        integer, intent(in) :: iostat_code
        character(len=*), intent(in) :: filename
        type(error_context_t), intent(out) :: error_ctx
        
        select case (trim(operation))
        case ("open", "OPEN")
            call interpret_iostat_open_error(iostat_code, filename, error_ctx)
        case ("read", "READ")
            call interpret_iostat_read_error(iostat_code, filename, error_ctx)
        case ("write", "WRITE")
            call interpret_iostat_write_error(iostat_code, filename, error_ctx)
        case ("close", "CLOSE")
            call interpret_iostat_close_error(iostat_code, filename, error_ctx)
        case default
            call clear_error_context(error_ctx)
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, &
                "File " // trim(operation) // " operation failed with iostat=" // &
                integer_to_string(iostat_code) // " for file: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "Check file accessibility and system resources")
            call safe_write_context(error_ctx, "File operation")
        end select
    end subroutine handle_file_operation_error
    
    ! Helper function to convert integer to string
    function integer_to_string(int_val) result(str)
        integer, intent(in) :: int_val
        character(len=:), allocatable :: str
        character(len=32) :: temp_str
        
        write(temp_str, '(I0)') int_val
        str = trim(temp_str)
    end function integer_to_string

end module iostat_error_utils