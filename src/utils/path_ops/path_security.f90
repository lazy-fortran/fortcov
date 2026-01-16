module path_security
    use error_handling_core, only: &
        error_context_t, &
        ERROR_SUCCESS, &
        ERROR_INVALID_PATH, &
        ERROR_MISSING_FILE, &
        clear_error_context, &
        safe_write_message, &
        safe_write_suggestion
    use string_utils, only: int_to_string
    implicit none
    private

    ! Public procedures
    public :: validate_path_security
    public :: validate_executable_path

    ! Maximum path length for basic validation
    integer, parameter :: MAX_PATH_LENGTH = 4096

    character(len=1), parameter :: BACKSLASH = achar(92)

contains

    subroutine validate_path_security(input_path, safe_path, error_ctx)
        character(len=*), intent(in) :: input_path
        character(len=:), allocatable, intent(out) :: safe_path
        type(error_context_t), intent(out) :: error_ctx

        character(len=len(input_path)) :: working_path
        integer :: path_len

        call clear_error_context(error_ctx)
        working_path = input_path
        path_len = len_trim(working_path)

        if (path_len == 0) then
            error_ctx%error_code = ERROR_INVALID_PATH
            call safe_write_message(error_ctx, "Empty path provided")
            return
        end if

        if (path_len > MAX_PATH_LENGTH) then
            error_ctx%error_code = ERROR_INVALID_PATH
            call safe_write_message( &
                error_ctx, &
                "Path exceeds maximum length: "//int_to_string(MAX_PATH_LENGTH))
            return
        end if

        if (index(working_path(1:path_len), char(0)) > 0) then
            error_ctx%error_code = ERROR_INVALID_PATH
            call safe_write_message(error_ctx, "Null byte in path")
            return
        end if

        safe_path = trim(working_path)
    end subroutine validate_path_security

    subroutine validate_executable_path(executable, safe_executable, error_ctx)
        character(len=*), intent(in) :: executable
        character(len=:), allocatable, intent(out) :: safe_executable
        type(error_context_t), intent(out) :: error_ctx

        logical :: exec_exists
        logical :: has_drive_letter, is_path_like

        call clear_error_context(error_ctx)

        has_drive_letter = (len_trim(executable) >= 2 .and. executable(2:2) == ':')
        is_path_like = (index(executable, '/') > 0 .or. &
                        index(executable, BACKSLASH) > 0 .or. has_drive_letter)

        if (is_path_like) then
            call validate_path_security(executable, safe_executable, error_ctx)
            if (error_ctx%error_code /= ERROR_SUCCESS) return

            inquire (file=safe_executable, exist=exec_exists)
            if (.not. exec_exists) then
                error_ctx%error_code = ERROR_MISSING_FILE
                call safe_write_message( &
                    error_ctx, &
                    "Executable not found - check installation and PATH")
                call safe_write_suggestion( &
                    error_ctx, &
                    "Verify the executable is installed and accessible")
                return
            end if
        else
            safe_executable = trim(executable)
        end if
    end subroutine validate_executable_path

end module path_security
