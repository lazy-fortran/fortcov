module atomic_temp_file_error_handler
    !! Error handling for atomic temporary file operations
    !! 
    !! This module provides error mapping and recovery checking
    !! for atomic temporary file operations.
    
    use iso_c_binding
    use error_handling
    implicit none
    private
    
    ! Internal error codes for temp file operations
    integer, parameter :: ERROR_CODE_STATE_ERROR = 2001
    integer, parameter :: ERROR_CODE_SECURITY_VIOLATION = 2002
    integer, parameter :: ERROR_CODE_IO_ERROR = 2003
    integer, parameter :: ERROR_CODE_FILE_NOT_FOUND = 2004
    integer, parameter :: ERROR_CODE_UNKNOWN = 2999
    integer, parameter :: ERROR_CODE_INVALID_ARGUMENT = 2005
    
    public :: map_c_error_to_context
    public :: is_temp_file_error_recoverable
    public :: ERROR_CODE_STATE_ERROR
    public :: ERROR_CODE_INVALID_ARGUMENT
    
contains
    
    subroutine map_c_error_to_context(c_error_code, error_ctx)
        !! Map C error codes to error context messages
        integer(c_int), intent(in) :: c_error_code
        type(error_context_t), intent(inout) :: error_ctx
        
        select case (c_error_code)
        case (0)
            ! Success - no error
            return
        case (1)
            error_ctx%error_code = ERROR_PERMISSION_DENIED
            error_ctx%message = "Permission denied creating secure temp file"
            error_ctx%recoverable = .false.
        case (2)
            error_ctx%error_code = ERROR_MISSING_FILE
            error_ctx%message = "Temporary directory does not exist or inaccessible"
            error_ctx%recoverable = .false.
        case (3)
            error_ctx%error_code = ERROR_CODE_SECURITY_VIOLATION
            error_ctx%message = "Secure temp file creation failed - security check failed"
            error_ctx%recoverable = .false.
        case (4)
            error_ctx%error_code = ERROR_OUT_OF_MEMORY
            error_ctx%message = "Out of memory allocating temp file state"
            error_ctx%recoverable = .true.
        case (5)
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            error_ctx%message = "I/O error during temp file operation"
            error_ctx%recoverable = .true.
        case default
            error_ctx%error_code = ERROR_FATAL
            error_ctx%message = "Unknown error in secure temp file operation"
            error_ctx%recoverable = .false.
        end select
    end subroutine map_c_error_to_context
    
    function is_temp_file_error_recoverable(error_code) result(recoverable)
        !! Check if a temp file error is recoverable
        integer, intent(in) :: error_code
        logical :: recoverable
        
        recoverable = .false.
        
        select case (error_code)
        case (ERROR_OUT_OF_MEMORY, ERROR_FILE_OPERATION_FAILED)
            ! These might be transient
            recoverable = .true.
        case default
            recoverable = .false.
        end select
    end function is_temp_file_error_recoverable
    
end module atomic_temp_file_error_handler