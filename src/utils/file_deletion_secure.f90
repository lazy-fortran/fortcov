module file_deletion_secure
    !! Secure file deletion operations with multi-layer security
    !!
    !! This module provides secure file deletion including safe cleanup,
    !! file overwriting, and comprehensive security validation for temporary
    !! files and sensitive data protection.
    use iso_fortran_env, only: error_unit
    use error_handling_core
    use string_utils, only: int_to_string
    use security_assessment_core, only: assess_deletion_security_risks
    implicit none
    private
    
    ! Parameters
    integer, parameter :: MAX_DELETION_ATTEMPTS = 3
    
    ! Public procedures
    public :: safe_close_and_delete
    public :: secure_overwrite_file
    
contains

    ! Robust temporary file deletion with multi-layer security - FIX for issue #297
    subroutine safe_close_and_delete(unit, filename, error_ctx)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: filename
        type(error_context_t), intent(inout) :: error_ctx
        
        integer :: close_iostat, delete_iostat
        logical :: file_exists_before, deletion_successful
        logical :: potential_security_issues
        character(len=256) :: security_concerns
        
        ! Check file existence before deletion attempts
        inquire(file=filename, exist=file_exists_before)
        
        ! Primary deletion attempt: Fortran close with status='delete'
        close(unit, status='delete', iostat=close_iostat)
        
        ! Attempt file deletion with multiple strategies if needed
        call attempt_file_deletion(unit, filename, close_iostat, &
                                  MAX_DELETION_ATTEMPTS, deletion_successful, delete_iostat)
        
        ! Comprehensive security vulnerability assessment
        call assess_deletion_security_risks(filename, close_iostat, delete_iostat, &
                                           deletion_successful, file_exists_before, &
                                           potential_security_issues, security_concerns)
        
        ! Report any errors or security concerns
        if (.not. deletion_successful .or. close_iostat /= 0 .or. &
            potential_security_issues) then
            call report_deletion_error(error_ctx, filename, close_iostat, &
                                      deletion_successful, file_exists_before, &
                                      MAX_DELETION_ATTEMPTS, potential_security_issues, &
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
        integer :: attempts, overwrite_iostat, alt_unit
        real :: start_time, current_time
        
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
                
                ! Strategy 2: Alternative secure file deletion attempt
                ! Use Fortran intrinsics instead of shell commands
                open(newunit=alt_unit, file=filename, status='old', iostat=delete_iostat)
                if (delete_iostat == 0) then
                    close(alt_unit, status='delete', iostat=delete_iostat)
                end if
                
                ! Brief pause between attempts to handle concurrent access
                if (attempts < max_attempts) then
                    ! Use Fortran intrinsic delay instead of shell sleep
                    call cpu_time(start_time)
                    do
                        call cpu_time(current_time)
                        if (current_time - start_time >= 0.1) exit
                        ! Wait for approximately 0.1 seconds
                    end do
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
                    "Close iostat: " // int_to_string(close_iostat) // &
                    ", Delete attempts: " // int_to_string(max_attempts))
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
                "Primary close iostat: " // int_to_string(close_iostat))
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

end module file_deletion_secure