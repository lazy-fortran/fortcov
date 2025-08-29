module error_utils
    !! Error utility functions extracted from error_handling_core
    !! 
    !! Focused on error logging, formatting, and context manipulation.
    !! Provides support functions for error handling operations.
    use error_types
    use iso_fortran_env, only: error_unit
    implicit none
    private
    
    ! Public procedures
    public :: log_error
    public :: format_error_message
    public :: is_recoverable_error
    public :: clear_error_context
    public :: safe_write_message
    public :: safe_write_suggestion
    public :: safe_write_context

contains

    ! Log error to file or stderr
    subroutine log_error(error_ctx, log_file)
        type(error_context_t), intent(inout) :: error_ctx
        character(len=*), intent(in), optional :: log_file
        
        integer :: unit
        character(len=32) :: timestamp
        
        ! Set logged flag immediately to prevent race condition
        if (error_ctx%logged) return
        error_ctx%logged = .true.
        
        call get_timestamp(timestamp)
        
        if (present(log_file)) then
            open(newunit=unit, file=log_file, position='append', &
                 status='unknown')
            write(unit, '(A,A,A,I0,A,A)') &
                "[", trim(timestamp), "] ERROR ", error_ctx%error_code, &
                ": ", trim(error_ctx%message)
            if (len_trim(error_ctx%suggestion) > 0) then
                write(unit, '(A,A)') "SUGGESTION: ", trim(error_ctx%suggestion)
            end if
            close(unit)
        else
            write(error_unit, '(A,I0,A,A)') &
                "ERROR ", error_ctx%error_code, ": ", trim(error_ctx%message)
            if (len_trim(error_ctx%suggestion) > 0) then
                write(error_unit, '(A,A)') "SUGGESTION: ", &
                    trim(error_ctx%suggestion)
            end if
        end if
    end subroutine log_error

    ! Format error message for user display
    function format_error_message(error_ctx) result(formatted_msg)
        type(error_context_t), intent(in) :: error_ctx
        character(len=:), allocatable :: formatted_msg
        
        character(len=1024) :: temp_msg
        
        write(temp_msg, '(A,I0,A,A)') &
            "Error ", error_ctx%error_code, ": ", trim(error_ctx%message)
        
        if (len_trim(error_ctx%suggestion) > 0) then
            temp_msg = trim(temp_msg) // char(10) // "Suggestion: " // &
                      trim(error_ctx%suggestion)
        end if
        
        formatted_msg = trim(temp_msg)
    end function format_error_message

    ! Check if error is recoverable
    function is_recoverable_error(error_ctx) result(recoverable)
        type(error_context_t), intent(in) :: error_ctx
        logical :: recoverable
        
        recoverable = error_ctx%recoverable
    end function is_recoverable_error

    ! Clear error context
    subroutine clear_error_context(error_ctx)
        type(error_context_t), intent(out) :: error_ctx
        
        error_ctx%error_code = ERROR_SUCCESS
        error_ctx%message = ""
        error_ctx%suggestion = ""
        error_ctx%context = ""
        error_ctx%stack_trace = ""
        error_ctx%recoverable = .false.
        error_ctx%logged = .false.
    end subroutine clear_error_context

    ! Safe message writing with bounds checking
    subroutine safe_write_message(error_ctx, text)
        type(error_context_t), intent(inout) :: error_ctx
        character(len=*), intent(in) :: text
        
        if (len(text) <= MAX_MESSAGE_LEN) then
            error_ctx%message = text
        else
            error_ctx%message = text(1:MAX_MESSAGE_LEN-3) // "..."
        end if
    end subroutine safe_write_message

    ! Safe suggestion writing with bounds checking
    subroutine safe_write_suggestion(error_ctx, text)
        type(error_context_t), intent(inout) :: error_ctx
        character(len=*), intent(in) :: text
        
        if (len(text) <= MAX_SUGGESTION_LEN) then
            error_ctx%suggestion = text
        else
            error_ctx%suggestion = text(1:MAX_SUGGESTION_LEN-3) // "..."
        end if
    end subroutine safe_write_suggestion

    ! Safe context writing with bounds checking
    subroutine safe_write_context(error_ctx, text)
        type(error_context_t), intent(inout) :: error_ctx
        character(len=*), intent(in) :: text
        
        if (len(text) <= MAX_CONTEXT_LEN) then
            error_ctx%context = text
        else
            error_ctx%context = text(1:MAX_CONTEXT_LEN-3) // "..."
        end if
    end subroutine safe_write_context

    ! Helper function to get timestamp
    subroutine get_timestamp(timestamp)
        character(len=*), intent(out) :: timestamp
        
        character(len=8) :: date_str
        character(len=10) :: time_str
        
        call date_and_time(date_str, time_str)
        
        ! Format as YYYY-MM-DD HH:MM:SS
        write(timestamp, '(A4,A1,A2,A1,A2,A1,A2,A1,A2,A1,A2)') &
            date_str(1:4), '-', date_str(5:6), '-', date_str(7:8), ' ', &
            time_str(1:2), ':', time_str(3:4), ':', time_str(5:6)
    end subroutine get_timestamp

end module error_utils