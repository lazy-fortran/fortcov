module file_utils
    use iso_fortran_env, only: error_unit, int64
    use error_handling
    use iostat_error_utils
    use secure_file_operations, only: safe_find_files, safe_mkdir
    use path_validation, only: validate_path_security
    use shell_utils, only: escape_shell_argument
    use input_validation
    implicit none
    private
    
    ! Public procedures
    public :: find_files
    public :: find_files_with_glob
    public :: resolve_path
    public :: read_binary_file
    public :: write_text_file
    public :: ensure_directory
    public :: read_binary_file_safe
    public :: write_text_file_safe
    public :: ensure_directory_safe
    public :: file_exists
    public :: read_file_content
    public :: read_multiple_files_batched
    public :: read_binary_file_enhanced
    public :: write_text_file_enhanced
    public :: read_file_content_enhanced

contains

    function find_files(pattern) result(files)
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: files(:)
        type(error_context_t) :: error_ctx
        
        ! Use secure command executor for safe file finding
        call safe_find_files(pattern, files, error_ctx)
        
        ! If secure find fails, return empty array
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (allocated(files)) deallocate(files)
            allocate(character(len=256) :: files(0))
        end if
    end function find_files

    function find_files_with_glob(directory, pattern) result(files)
        !! Find files in specific directory matching glob pattern
        character(len=*), intent(in) :: directory
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: files(:)
        
        character(len=256) :: full_pattern
        type(error_context_t) :: error_ctx
        
        ! Construct full search pattern
        if (trim(directory) == ".") then
            full_pattern = trim(pattern)
        else
            full_pattern = trim(directory) // "/" // trim(pattern)
        end if
        
        ! Use secure command executor for safe file finding
        call safe_find_files(full_pattern, files, error_ctx)
        
        ! If secure find fails, return empty array
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (allocated(files)) deallocate(files)
            allocate(character(len=256) :: files(0))
        end if
    end function find_files_with_glob

    function resolve_path(path) result(resolved)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: resolved
        character(len=256) :: cwd
        integer :: stat
        
        if (path(1:1) == "/") then
            ! Already absolute path
            resolved = trim(path)
        else if (path(1:2) == "./") then
            ! Relative to current directory
            call getcwd(cwd, stat)
            if (stat == 0) then
                resolved = trim(cwd) // "/" // path(3:)
            else
                resolved = path
            end if
        else
            ! Relative path without ./
            call getcwd(cwd, stat)
            if (stat == 0) then
                resolved = trim(cwd) // "/" // trim(path)
            else
                resolved = path
            end if
        end if
    end function resolve_path

    subroutine read_binary_file(filename, data, error_flag)
        character(len=*), intent(in) :: filename
        integer(kind=1), allocatable, intent(out) :: data(:)
        logical, intent(out) :: error_flag
        integer :: unit, stat, file_size, i
        integer(kind=1) :: byte
        
        error_flag = .false.
        
        ! Check if file exists
        inquire(file=filename, exist=error_flag, size=file_size)
        if (.not. error_flag) then
            error_flag = .true.
            allocate(data(0))
            return
        end if
        
        error_flag = .false.
        
        ! Open file as stream
        open(newunit=unit, file=filename, access='stream', &
             status='old', iostat=stat)
        if (stat /= 0) then
            error_flag = .true.
            allocate(data(0))
            return
        end if
        
        ! Allocate data array
        allocate(data(file_size))
        
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

    subroutine ensure_directory(path, error_flag)
        character(len=*), intent(in) :: path
        logical, intent(out) :: error_flag
        type(error_context_t) :: error_ctx
        
        error_flag = .false.
        
        ! Use secure command executor for safe directory creation
        call safe_mkdir(path, error_ctx)
        error_flag = (error_ctx%error_code /= ERROR_SUCCESS)
    end subroutine ensure_directory

    ! Helper function - basename functionality kept for compatibility
    function basename(filepath) result(name)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable :: name
        integer :: pos
        
        pos = index(trim(filepath), "/", back=.true.)
        if (pos > 0) then
            name = trim(filepath(pos+1:))
        else
            name = trim(filepath)
        end if
    end function basename

    ! Enhanced file operations using comprehensive error handling
    
    ! Safe binary file reading with comprehensive error context
    subroutine read_binary_file_safe(filename, data, error_ctx)
        character(len=*), intent(in) :: filename
        integer(kind=1), allocatable, intent(out) :: data(:)
        type(error_context_t), intent(out) :: error_ctx
        
        logical :: file_exists, error_flag
        integer :: file_size
        
        call clear_error_context(error_ctx)
        
        ! Check if file exists first
        inquire(file=filename, exist=file_exists, size=file_size)
        if (.not. file_exists) then
            call handle_missing_source(filename, error_ctx)
            allocate(data(0))
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
    
    ! Safe directory creation with comprehensive error context
    subroutine ensure_directory_safe(path, error_ctx)
        character(len=*), intent(in) :: path
        type(error_context_t), intent(out) :: error_ctx
        
        call clear_error_context(error_ctx)
        
        ! Use secure command executor directly
        call safe_mkdir(path, error_ctx)
    end subroutine ensure_directory_safe

    ! Check if file exists
    function file_exists(filename) result(exists)
        character(len=*), intent(in) :: filename
        logical :: exists
        
        inquire(file=filename, exist=exists)
    end function file_exists

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
             form='unformatted', access='stream', iostat=iostat)
        if (iostat /= 0) then
            error_flag = .true.
            return
        end if
        
        ! Get file size with validation
        inquire(unit=unit, size=file_size)
        if (file_size <= 0) then
            allocate(character(len=0) :: content)
            close(unit)
            return
        end if
        
        ! Validate memory allocation request
        call validate_memory_allocation_request(file_size, validation_result)
        if (.not. validation_result%is_valid) then
            error_flag = .true.
            close(unit)
            return
        end if
        
        ! Safe memory allocation with bounds checking
        if (file_size > huge(1)) then  ! Check if file_size fits in default integer
            error_flag = .true.
            close(unit)
            return
        end if
        
        ! Read entire file with validated size
        allocate(buffer(int(file_size)))
        read(unit, iostat=iostat) buffer
        close(unit)
        
        if (iostat /= 0) then
            error_flag = .true.
            if (allocated(buffer)) deallocate(buffer)
            return
        end if
        
        ! Convert buffer to string
        content = transfer(buffer, repeat(' ', int(file_size)))
        deallocate(buffer)
    end subroutine read_file_content

    ! PERFORMANCE OPTIMIZATION: Batched file reading for multiple files
    ! Reduces I/O overhead by processing multiple files in sequence
    subroutine read_multiple_files_batched(filenames, contents, success_flags)
        character(len=*), intent(in) :: filenames(:)
        character(len=:), allocatable, intent(out) :: contents(:)
        logical, intent(out) :: success_flags(:)
        
        integer :: i, num_files
        logical :: file_error
        character(len=:), allocatable :: temp_content
        
        num_files = size(filenames)
        
        ! Validate input array sizes match
        if (size(success_flags) /= num_files) then
            success_flags = .false.
            return
        end if
        
        ! Allocate contents array
        allocate(character(len=0) :: contents(num_files))
        
        ! Process files in batch with optimized I/O patterns
        do i = 1, num_files
            call read_file_content(filenames(i), temp_content, file_error)
            success_flags(i) = .not. file_error
            
            if (.not. file_error) then
                contents(i) = temp_content
            else
                contents(i) = ""
            end if
            
            ! Early exit optimization: if this is a critical batch operation
            ! and any file fails, we could exit early (implementation dependent)
        end do
    end subroutine read_multiple_files_batched
    
    ! Enhanced file operations with granular iostat error handling
    
    subroutine read_binary_file_enhanced(filename, data, error_ctx)
        !! Enhanced binary file reading with granular iostat error handling
        character(len=*), intent(in) :: filename
        integer(kind=1), allocatable, intent(out) :: data(:)
        type(error_context_t), intent(out) :: error_ctx
        
        integer :: unit, iostat, file_size, i
        integer(kind=1) :: byte
        logical :: file_exists
        
        call clear_error_context(error_ctx)
        
        ! Check if file exists
        inquire(file=filename, exist=file_exists, size=file_size)
        if (.not. file_exists) then
            call handle_missing_source(filename, error_ctx)
            allocate(data(0))
            return
        end if
        
        if (file_size == 0) then
            error_ctx%error_code = ERROR_EMPTY_FILE
            call safe_write_message(error_ctx, &
                "Empty file: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "Check if file was properly generated or contains valid data")
            call safe_write_context(error_ctx, "Binary file reading")
            allocate(data(0))
            return
        end if
        
        ! Open file as stream with enhanced error handling
        open(newunit=unit, file=filename, access='stream', &
             status='old', iostat=iostat)
        if (iostat /= 0) then
            call interpret_iostat_open_error(iostat, filename, error_ctx)
            allocate(data(0))
            return
        end if
        
        ! Allocate data array
        allocate(data(file_size))
        
        ! Read bytes with enhanced error reporting
        do i = 1, file_size
            read(unit, iostat=iostat) byte
            if (iostat /= 0) then
                call interpret_iostat_read_error(iostat, filename, error_ctx)
                close(unit)
                if (allocated(data)) deallocate(data)
                allocate(data(0))
                return
            end if
            data(i) = byte
        end do
        
        close(unit, iostat=iostat)
        if (iostat /= 0) then
            call interpret_iostat_close_error(iostat, filename, error_ctx)
            ! Continue despite close error - data was successfully read
            error_ctx%recoverable = .true.
        end if
    end subroutine read_binary_file_enhanced
    
    subroutine write_text_file_enhanced(filename, content, error_ctx)
        !! Enhanced text file writing with granular iostat error handling  
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: content
        type(error_context_t), intent(out) :: error_ctx
        
        integer :: unit, iostat
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
        
        ! Open file with enhanced error handling
        open(newunit=unit, file=filename, status='replace', iostat=iostat)
        if (iostat /= 0) then
            call interpret_iostat_open_error(iostat, filename, error_ctx)
            return
        end if
        
        ! Write content with enhanced error reporting
        write(unit, '(A)', iostat=iostat) content
        if (iostat /= 0) then
            call interpret_iostat_write_error(iostat, filename, error_ctx)
            close(unit)
            return
        end if
        
        close(unit, iostat=iostat)
        if (iostat /= 0) then
            call interpret_iostat_close_error(iostat, filename, error_ctx)
            ! Continue despite close error - content was successfully written
            error_ctx%recoverable = .true.
        end if
    end subroutine write_text_file_enhanced
    
    subroutine read_file_content_enhanced(filename, content, error_ctx)
        !! Enhanced text file reading with granular iostat error handling
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: content
        type(error_context_t), intent(out) :: error_ctx
        
        integer :: unit, iostat
        integer(int64) :: file_size
        character(len=1), allocatable :: buffer(:)
        type(validation_result_t) :: validation_result
        logical :: file_exists
        
        call clear_error_context(error_ctx)
        
        ! Check if file exists first
        inquire(file=filename, exist=file_exists)
        if (.not. file_exists) then
            call handle_missing_source(filename, error_ctx)
            content = ""
            return
        end if
        
        ! Comprehensive file validation before opening
        call validate_file_constraints(filename, validation_result)
        if (.not. validation_result%is_valid) then
            error_ctx%error_code = ERROR_INVALID_CONFIG
            call safe_write_message(error_ctx, &
                "File validation failed: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "Check file accessibility and format")
            call safe_write_context(error_ctx, "File content reading")
            content = ""
            return
        end if
        
        ! Open file with enhanced error handling
        open(newunit=unit, file=filename, status='old', action='read', &
             form='unformatted', access='stream', iostat=iostat)
        if (iostat /= 0) then
            call interpret_iostat_open_error(iostat, filename, error_ctx)
            content = ""
            return
        end if
        
        ! Get file size with validation
        inquire(unit=unit, size=file_size)
        if (file_size <= 0) then
            allocate(character(len=0) :: content)
            close(unit)
            return
        end if
        
        ! Validate memory allocation request
        call validate_memory_allocation_request(file_size, validation_result)
        if (.not. validation_result%is_valid) then
            error_ctx%error_code = ERROR_OUT_OF_MEMORY
            call safe_write_message(error_ctx, &
                "File too large for memory allocation: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "1. Process file in smaller chunks" // char(10) // &
                "2. Increase available memory" // char(10) // &
                "3. Use streaming read approach")
            call safe_write_context(error_ctx, "File content reading")
            content = ""
            close(unit)
            return
        end if
        
        ! Safe memory allocation with bounds checking
        if (file_size > huge(1)) then  ! Check if file_size fits in default integer
            error_ctx%error_code = ERROR_OUT_OF_MEMORY
            call safe_write_message(error_ctx, &
                "File size exceeds maximum supported size: " // trim(filename))
            call safe_write_suggestion(error_ctx, &
                "Process file in smaller chunks or use 64-bit build")
            call safe_write_context(error_ctx, "File content reading")
            content = ""
            close(unit)
            return
        end if
        
        ! Read entire file with enhanced error handling
        allocate(buffer(int(file_size)))
        read(unit, iostat=iostat) buffer
        
        if (iostat /= 0) then
            call interpret_iostat_read_error(iostat, filename, error_ctx)
            if (allocated(buffer)) deallocate(buffer)
            content = ""
            close(unit)
            return
        end if
        
        close(unit, iostat=iostat)
        if (iostat /= 0) then
            call interpret_iostat_close_error(iostat, filename, error_ctx)
            ! Continue despite close error - content was successfully read
            error_ctx%recoverable = .true.
        end if
        
        ! Convert buffer to string
        content = transfer(buffer, repeat(' ', int(file_size)))
        deallocate(buffer)
    end subroutine read_file_content_enhanced

end module file_utils
