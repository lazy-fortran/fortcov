module text_file_io
    use iso_fortran_env, only: int64
    use error_handling
    use iostat_error_utils
    use input_validation
    use directory_operations
    implicit none
    private
    
    ! Public procedures
    public :: write_text_file
    public :: write_text_file_safe
    public :: read_file_content
    public :: read_file_content_enhanced
    
contains

    subroutine write_text_file(filename, content, error_flag)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: content
        logical, intent(out) :: error_flag
        integer :: unit, stat
        
        error_flag = .false.
        
        open(newunit=unit, file=filename, status='replace', iostat=stat)
        if (stat /= 0) then
            error_flag = .true.
            return
        end if
        
        write(unit, '(A)', iostat=stat) content
        if (stat /= 0) then
            error_flag = .true.
            close(unit)
            return
        end if
        
        close(unit)
    end subroutine write_text_file
    
    ! Safe text file writing with comprehensive error context
    subroutine write_text_file_safe(filename, content, error_ctx)
        character(len=*), intent(in) :: filename, content
        type(error_context_t), intent(out) :: error_ctx
        
        logical :: error_flag
        character(len=:), allocatable :: dir_path
        integer :: last_slash
        
        call clear_error_context(error_ctx)
        
        ! Extract directory path and ensure it exists
        last_slash = index(filename, "/", back=.true.)
        if (last_slash > 0) then
            dir_path = filename(1:last_slash-1)
            call ensure_directory_safe(dir_path, error_ctx)
            if (error_ctx%error_code /= ERROR_SUCCESS) return
        end if
        
        ! Try to write the file
        call write_text_file(filename, content, error_flag)
        
        if (error_flag) then
            call handle_permission_denied(filename, error_ctx)
        end if
    end subroutine write_text_file_safe

    ! Read text file content with comprehensive validation
    subroutine read_file_content(filename, content, error_flag)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: content
        logical, intent(out) :: error_flag
        
        integer :: unit, iostat
        integer(int64) :: file_size
        character(len=1), allocatable :: buffer(:)
        type(validation_result_t) :: validation_result
        
        error_flag = .false.
        
        ! Comprehensive file validation before opening
        call validate_file_constraints(filename, validation_result)
        if (.not. validation_result%is_valid) then
            error_flag = .true.
            return
        end if
        
        ! Open file and get size
        open(newunit=unit, file=filename, status='old', action='read', &
             access='stream', iostat=iostat)
        if (iostat /= 0) then
            error_flag = .true.
            content = ""
            return
        end if
        
        inquire(unit=unit, size=file_size)
        if (file_size <= 0) then
            close(unit)
            content = ""
            return
        end if
        
        ! Allocate buffer
        allocate(buffer(file_size))
        
        ! Read file content
        read(unit, iostat=iostat) buffer
        if (iostat /= 0) then
            error_flag = .true.
            deallocate(buffer)
            close(unit)
            content = ""
            return
        end if
        
        close(unit)
        
        ! Convert buffer to string
        content = transfer(buffer, repeat(' ', int(file_size)))
        deallocate(buffer)
    end subroutine read_file_content

    ! Enhanced read file content with comprehensive error handling
    subroutine read_file_content_enhanced(filename, content, error_ctx)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: content
        type(error_context_t), intent(out) :: error_ctx
        
        logical :: error_flag
        
        call clear_error_context(error_ctx)
        
        call read_file_content(filename, content, error_flag)
        
        if (error_flag) then
            call handle_permission_denied(filename, error_ctx)
            content = ""
        end if
    end subroutine read_file_content_enhanced

end module text_file_io