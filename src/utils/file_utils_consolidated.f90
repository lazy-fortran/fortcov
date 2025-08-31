module file_utils_consolidated
    !! Consolidated file utilities module
    !! 
    !! Combines all file operations including:
    !! - File finding and globbing
    !! - Binary file I/O  
    !! - Text file I/O
    !! - Directory operations
    !! - Path resolution
    !! - File operations security
    
    use iso_fortran_env, only: int64
    use error_handling_core
    use iostat_utils_core
    use input_validation_core
    use directory_ops_core
    use file_ops_secure, only: safe_find_files
    use file_search_secure, only: safe_find_files_with_glob
    use path_utils_consolidated, only: resolve_path, file_exists, basename
    implicit none
    private
    
    ! File finding procedures
    public :: find_files
    public :: find_files_with_glob
    
    ! Binary I/O procedures 
    public :: read_binary_file
    public :: read_binary_file_safe
    
    ! Text I/O procedures
    public :: write_text_file
    public :: write_text_file_safe
    public :: read_file_content
    public :: read_file_content_enhanced
    
    ! Directory operations
    public :: ensure_directory
    public :: ensure_directory_safe
    
    ! Path operations
    public :: resolve_path
    public :: file_exists
    public :: basename
    
contains

    ! ========================================================================
    ! File Finding Operations
    ! ========================================================================
    
    function find_files(pattern) result(files)
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: files(:)
        type(error_context_t) :: error_ctx
        integer :: stat
        character(len=512) :: errmsg
        
        ! Use secure command executor for safe file finding
        call safe_find_files(pattern, files, error_ctx)
        
        ! If secure find fails, return empty array
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (allocated(files)) deallocate(files, stat=stat)
            allocate(character(len=256) :: files(0), stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Failed to allocate empty files array: " // trim(errmsg)
                return
            end if
        end if
    end function find_files

    function find_files_with_glob(directory, pattern) result(files)
        !! Find files in specific directory matching glob pattern
        character(len=*), intent(in) :: directory
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: files(:)
        
        type(error_context_t) :: error_ctx
        integer :: stat
        character(len=512) :: errmsg
        
        ! Use enhanced API from file_search_secure for direct directory+pattern
        call safe_find_files_with_glob(directory, pattern, files, error_ctx)
        
        ! If secure find fails, return empty array
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (allocated(files)) deallocate(files, stat=stat)
            allocate(character(len=256) :: files(0), stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Failed to allocate empty files array: " // trim(errmsg)
                return
            end if
        end if
    end function find_files_with_glob

    ! ========================================================================
    ! Binary File I/O Operations
    ! ========================================================================

    subroutine read_binary_file(filename, data, error_flag)
        character(len=*), intent(in) :: filename
        integer(kind=1), allocatable, intent(out) :: data(:)
        logical, intent(out) :: error_flag
        integer :: unit, stat, file_size, i
        integer(kind=1) :: byte
        character(len=512) :: errmsg
        
        error_flag = .false.
        
        ! Check if file exists
        inquire(file=filename, exist=error_flag, size=file_size)
        if (.not. error_flag) then
            error_flag = .true.
            allocate(data(0), stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Failed to allocate empty data array: " // trim(errmsg)
            end if
            return
        end if
        
        error_flag = .false.
        
        ! Open file as stream
        open(newunit=unit, file=filename, access='stream', &
             status='old', iostat=stat)
        if (stat /= 0) then
            error_flag = .true.
            allocate(data(0), stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Failed to allocate empty data array: " // trim(errmsg)
            end if
            return
        end if
        
        ! Allocate data array
        allocate(data(file_size), stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            write(*, '(A)') "Error: Failed to allocate data array: " // trim(errmsg)
            error_flag = .true.
            close(unit)
            return
        end if
        
        ! Read bytes
        do i = 1, file_size
            read(unit, iostat=stat) byte
            if (stat /= 0) then
                error_flag = .true.
                close(unit)
                return
            end if
            data(i) = byte
        end do
        
        close(unit)
    end subroutine read_binary_file
    
    subroutine read_binary_file_safe(filename, data, error_ctx)
        !! Safe binary file reading with comprehensive error context
        character(len=*), intent(in) :: filename
        integer(kind=1), allocatable, intent(out) :: data(:)
        type(error_context_t), intent(out) :: error_ctx
        
        logical :: file_exists, error_flag
        integer :: file_size
        integer :: stat
        character(len=512) :: errmsg
        
        call clear_error_context(error_ctx)
        
        ! Check if file exists first
        inquire(file=filename, exist=file_exists, size=file_size)
        if (.not. file_exists) then
            call handle_missing_source(filename, error_ctx)
            allocate(data(0), stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Failed to allocate empty data array: " // trim(errmsg)
            end if
            return
        end if
        
        ! Try to read the file using existing function
        call read_binary_file(filename, data, error_flag)
        
        if (error_flag) then
            if (file_size == 0) then
                error_ctx%error_code = ERROR_INVALID_CONFIG
                write(error_ctx%message, '(A,A)') &
                    "Empty file: ", trim(filename)
                write(error_ctx%suggestion, '(A)') &
                    "Check if file was properly generated."
            else
                call handle_permission_denied(filename, error_ctx)
            end if
        end if
    end subroutine read_binary_file_safe

    ! ========================================================================
    ! Text File I/O Operations
    ! ========================================================================

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
    
    subroutine write_text_file_safe(filename, content, error_ctx)
        !! Safe text file writing with comprehensive error context
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

    subroutine read_file_content(filename, content, error_flag)
        !! Read text file content with comprehensive validation
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: content
        logical, intent(out) :: error_flag
        
        integer :: unit, iostat, i
        integer(int64) :: file_size
        character(len=1), allocatable :: buffer(:)
        type(validation_result_t) :: validation_result
        integer :: stat
        character(len=512) :: errmsg
        
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
        allocate(buffer(file_size), stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            write(*, '(A)') "Error: Failed to allocate buffer: " // trim(errmsg)
            error_flag = .true.
            close(unit)
            content = ""
            return
        end if
        
        ! Read file content
        read(unit, iostat=iostat) buffer
        if (iostat /= 0) then
            error_flag = .true.
            deallocate(buffer, stat=stat)
            close(unit)
            content = ""
            return
        end if
        
        close(unit)
        
        ! Convert buffer to string safely without transfer()
        allocate(character(len=file_size) :: content, stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            write(*, '(A)') "Error: Failed to allocate content string: " // trim(errmsg)
            error_flag = .true.
            deallocate(buffer, stat=stat)
            return
        end if
        do i = 1, int(file_size)
            content(i:i) = buffer(i)
        end do
        deallocate(buffer, stat=stat)
    end subroutine read_file_content

    subroutine read_file_content_enhanced(filename, content, error_ctx)
        !! Enhanced read file content with comprehensive error handling
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

end module file_utils_consolidated