module temp_file_error_handling
    !! Temporary File Error Handling
    !!
    !! Handles C error code mapping and error recovery logic.
    !! Extracted from atomic_temp_file_manager_impl.f90 for SRP compliance.
    use iso_c_binding
    implicit none
    private

    ! Public procedures
    public :: map_c_error_to_context
    public :: is_temp_file_error_recoverable

contains

    ! Map C error codes to Fortran error context
    subroutine map_c_error_to_context(c_error_code, error_ctx)
        integer(c_int), intent(in) :: c_error_code
        type(*), intent(inout) :: error_ctx

        ! This is a simplified mapping - actual implementation would depend
        ! on the specific error context type being used
        select case (c_error_code)
        case (0)
            ! Success - no error context update needed
            continue
        case (1)
            ! File creation failed
            ! error_ctx%set_error("Failed to create secure temporary file")
        case (2)
            ! Write operation failed
            ! error_ctx%set_error("Failed to write to temporary file")
        case (3)
            ! Read operation failed
            ! error_ctx%set_error("Failed to read from temporary file")
        case (4)
            ! Move operation failed
            ! error_ctx%set_error("Failed to move temporary file")
        case (5)
            ! Cleanup operation failed
            ! error_ctx%set_error("Failed to cleanup temporary file")
        case (10)
            ! Security validation failed
            ! error_ctx%set_error("Security validation failed")
        case (11)
            ! Symlink attack detected
            ! error_ctx%set_error("Potential symlink attack detected")
        case (12)
            ! Insufficient entropy
            ! error_ctx%set_error("Insufficient entropy for secure operations")
        case (20)
            ! Platform-specific error
            ! error_ctx%set_error("Platform-specific security error")
        case (99)
            ! Simulated error for testing
            ! error_ctx%set_error("Simulated error condition")
        case default
            ! Unknown error
            ! error_ctx%set_error("Unknown temporary file error")
        end select
    end subroutine map_c_error_to_context

    ! Determine if a temporary file error is recoverable
    function is_temp_file_error_recoverable(error_code) result(recoverable)
        integer, intent(in) :: error_code
        logical :: recoverable

        select case (error_code)
        case (0)
            ! No error
            recoverable = .true.
        case (1, 2, 3, 4)
            ! File operations - potentially recoverable with retry
            recoverable = .true.
        case (5)
            ! Cleanup errors - usually not critical for continuation
            recoverable = .true.
        case (10, 11, 12, 20)
            ! Security errors - not recoverable
            recoverable = .false.
        case (99)
            ! Simulated error - recoverable for testing
            recoverable = .true.
        case default
            ! Unknown errors - assume not recoverable
            recoverable = .false.
        end select
    end function is_temp_file_error_recoverable

end module temp_file_error_handling