module file_deletion_secure
    use error_handling_core
    implicit none
    private

    ! Public procedures
    public :: safe_close_and_delete
    public :: secure_overwrite_file

contains

    subroutine safe_close_and_delete(unit, filename, error_ctx)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: filename
        type(error_context_t), intent(inout) :: error_ctx

        integer :: close_iostat, ios, alt_unit
        logical :: exists_after

        close_iostat = 0
        close (unit, status='delete', iostat=close_iostat)

        inquire (file=filename, exist=exists_after)
        if (.not. exists_after) return

        close (unit, iostat=ios)
        open (newunit=alt_unit, file=filename, status='old', iostat=ios)
        if (ios == 0) then
            close (alt_unit, status='delete', iostat=ios)
        end if

        inquire (file=filename, exist=exists_after)
        if (exists_after) then
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            error_ctx%recoverable = .true.
            call safe_write_message(error_ctx, &
                                    "Failed to delete temporary file: "//trim(filename))
        end if

    end subroutine safe_close_and_delete

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
        open (newunit=unit, file=filename, action='write', status='old', &
              access='stream', iostat=iostat)
        if (iostat /= 0) return

        ! Get file size
        inquire (unit=unit, size=file_size_bytes, iostat=iostat)
        if (iostat /= 0) then
            close (unit)
            return
        end if

        ! Overwrite file content with zeros
        rewind (unit)
        do i = 1, (file_size_bytes/len(overwrite_buffer)) + 1
            write (unit, iostat=iostat) overwrite_buffer
            if (iostat /= 0) exit
        end do

        ! Ensure data is written to disk
        flush (unit, iostat=iostat)
        close (unit)

    end subroutine secure_overwrite_file

end module file_deletion_secure
