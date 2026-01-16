module iostat_utilities
    use error_handling_core, only: clear_error_context, error_context_t, &
                                   safe_write_context, safe_write_message, &
                                   ERROR_EMPTY_FILE, ERROR_FILE_OPERATION_FAILED, &
                                   ERROR_INVALID_DATA, ERROR_MISSING_FILE, &
                                   ERROR_OUT_OF_MEMORY, ERROR_PERMISSION_DENIED
    use string_utils, only: int_to_string
    implicit none
    private

    integer, parameter :: IOSTAT_SUCCESS = 0
    integer, parameter :: IOSTAT_EOR = -1
    integer, parameter :: IOSTAT_EOF = -2
    integer, parameter :: IOSTAT_FORMAT_ERROR = 5010
    integer, parameter :: IOSTAT_NO_SUCH_FILE = 29
    integer, parameter :: IOSTAT_PERMISSION_DENIED = 13
    integer, parameter :: IOSTAT_FILE_EXISTS = 17
    integer, parameter :: IOSTAT_NO_SPACE = 28
    integer, parameter :: IOSTAT_BAD_FILE_DESCRIPTOR = 9
    integer, parameter :: IOSTAT_INVALID_ARGUMENT = 22
    integer, parameter :: IOSTAT_TOO_MANY_FILES = 24

    public :: interpret_iostat_open_error
    public :: interpret_iostat_read_error
    public :: interpret_iostat_write_error
    public :: interpret_iostat_close_error
    public :: get_iostat_error_message
    public :: handle_file_operation_error

contains

    subroutine interpret_iostat_open_error(iostat_code, filename, error_ctx)
        integer, intent(in) :: iostat_code
        character(len=*), intent(in) :: filename
        type(error_context_t), intent(out) :: error_ctx

        call clear_error_context(error_ctx)
        if (iostat_code == IOSTAT_SUCCESS) return

        error_ctx%recoverable = .false.
        call safe_write_context(error_ctx, "File open")

        select case (iostat_code)
        case (IOSTAT_NO_SUCH_FILE)
            error_ctx%error_code = ERROR_MISSING_FILE
            call safe_write_message(error_ctx, "Could not open file: "// &
                                    trim(filename))
        case (IOSTAT_PERMISSION_DENIED)
            error_ctx%error_code = ERROR_PERMISSION_DENIED
            call safe_write_message(error_ctx, "Permission denied opening: "// &
                                    trim(filename))
        case (IOSTAT_NO_SPACE)
            error_ctx%error_code = ERROR_OUT_OF_MEMORY
            call safe_write_message(error_ctx, "No space left opening: "// &
                                    trim(filename))
        case (IOSTAT_TOO_MANY_FILES)
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, "Too many open files")
        case (IOSTAT_FILE_EXISTS)
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, "File already exists: "// &
                                    trim(filename))
        case default
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, "Could not open file: "// &
                                    trim(filename)//" (iostat="// &
                                    int_to_string(iostat_code)//")")
        end select
    end subroutine interpret_iostat_open_error

    subroutine interpret_iostat_read_error(iostat_code, filename, error_ctx)
        integer, intent(in) :: iostat_code
        character(len=*), intent(in) :: filename
        type(error_context_t), intent(out) :: error_ctx

        call clear_error_context(error_ctx)
        if (iostat_code == IOSTAT_SUCCESS) return

        error_ctx%recoverable = (iostat_code == IOSTAT_EOF)
        call safe_write_context(error_ctx, "File read")

        select case (iostat_code)
        case (IOSTAT_EOF)
            error_ctx%error_code = ERROR_EMPTY_FILE
            call safe_write_message(error_ctx, "Unexpected end-of-file reading: "// &
                                    trim(filename))
        case (IOSTAT_EOR)
            error_ctx%error_code = ERROR_INVALID_DATA
            call safe_write_message(error_ctx, "Unexpected end-of-record reading: "// &
                                    trim(filename))
        case (IOSTAT_FORMAT_ERROR)
            error_ctx%error_code = ERROR_INVALID_DATA
            call safe_write_message(error_ctx, "Format error reading: "// &
                                    trim(filename))
        case (IOSTAT_BAD_FILE_DESCRIPTOR)
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, "Bad file descriptor reading: "// &
                                    trim(filename))
        case default
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, "File read failed: "// &
                                    trim(filename)//" (iostat="// &
                                    int_to_string(iostat_code)//")")
        end select
    end subroutine interpret_iostat_read_error

    subroutine interpret_iostat_write_error(iostat_code, filename, error_ctx)
        integer, intent(in) :: iostat_code
        character(len=*), intent(in) :: filename
        type(error_context_t), intent(out) :: error_ctx

        call clear_error_context(error_ctx)
        if (iostat_code == IOSTAT_SUCCESS) return

        error_ctx%recoverable = .false.
        call safe_write_context(error_ctx, "File write")

        select case (iostat_code)
        case (IOSTAT_NO_SPACE)
            error_ctx%error_code = ERROR_OUT_OF_MEMORY
            call safe_write_message(error_ctx, "No space left writing: "// &
                                    trim(filename))
        case (IOSTAT_PERMISSION_DENIED)
            error_ctx%error_code = ERROR_PERMISSION_DENIED
            call safe_write_message(error_ctx, "Permission denied writing: "// &
                                    trim(filename))
        case (IOSTAT_BAD_FILE_DESCRIPTOR)
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, "Bad file descriptor writing: "// &
                                    trim(filename))
        case (IOSTAT_FORMAT_ERROR)
            error_ctx%error_code = ERROR_INVALID_DATA
            call safe_write_message(error_ctx, "Format error writing: "// &
                                    trim(filename))
        case default
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, "File write failed: "// &
                                    trim(filename)//" (iostat="// &
                                    int_to_string(iostat_code)//")")
        end select
    end subroutine interpret_iostat_write_error

    subroutine interpret_iostat_close_error(iostat_code, filename, error_ctx)
        integer, intent(in) :: iostat_code
        character(len=*), intent(in) :: filename
        type(error_context_t), intent(out) :: error_ctx

        call clear_error_context(error_ctx)
        if (iostat_code == IOSTAT_SUCCESS) return

        error_ctx%recoverable = (iostat_code == IOSTAT_BAD_FILE_DESCRIPTOR)
        call safe_write_context(error_ctx, "File close")

        select case (iostat_code)
        case (IOSTAT_BAD_FILE_DESCRIPTOR)
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, "Bad file descriptor closing: "// &
                                    trim(filename))
        case (IOSTAT_NO_SPACE)
            error_ctx%error_code = ERROR_OUT_OF_MEMORY
            call safe_write_message(error_ctx, "No space left closing: "// &
                                    trim(filename))
        case default
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            call safe_write_message(error_ctx, "File close failed: "// &
                                    trim(filename)//" (iostat="// &
                                    int_to_string(iostat_code)//")")
        end select
    end subroutine interpret_iostat_close_error

    function get_iostat_error_message(iostat_code) result(message)
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
            call safe_write_context(error_ctx, "File operation")
            call safe_write_message(error_ctx, "File operation failed: "// &
                                    trim(operation)//" (iostat="// &
                                    int_to_string(iostat_code)//") for "// &
                                    trim(filename))
            error_ctx%recoverable = .false.
        end select
    end subroutine handle_file_operation_error

end module iostat_utilities
