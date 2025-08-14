module file_utils
    use iso_fortran_env, only: error_unit
    use error_handling
    implicit none
    private
    
    ! Public procedures
    public :: find_files
    public :: resolve_path
    public :: read_binary_file
    public :: write_text_file
    public :: ensure_directory
    public :: read_binary_file_safe
    public :: write_text_file_safe
    public :: ensure_directory_safe

contains

    function find_files(pattern) result(files)
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: files(:)
        character(len=256) :: command, line
        character(len=256) :: temp_files(100)
        integer :: unit, stat, count, i
        character(len=32) :: temp_filename
        
        ! Use find command to locate files
        count = 0
        
        ! Create unique temp filename
        temp_filename = "/tmp/fortcov_find_output"
        
        if (index(pattern, "**") > 0) then
            ! Recursive search
            command = "find " // extract_dir_from_pattern(pattern) // &
                     " -name '" // extract_filename_from_pattern(pattern) // &
                     "' 2>/dev/null > " // trim(temp_filename)
        else
            ! Non-recursive search using ls
            command = "ls " // pattern // " 2>/dev/null > " // trim(temp_filename)
        end if
        
        ! Execute command
        call execute_command_line(command, exitstat=stat)
        
        ! Read results from temp file
        open(newunit=unit, file=trim(temp_filename), action='read', &
             status='old', iostat=stat)
        if (stat == 0) then
            do i = 1, 100
                read(unit, '(A)', iostat=stat) line
                if (stat /= 0) exit
                if (len_trim(line) > 0) then
                    count = count + 1
                    temp_files(count) = trim(line)
                end if
            end do
            close(unit, status='delete')
        end if
        
        ! Allocate result array
        if (count > 0) then
            allocate(character(len=256) :: files(count))
            do i = 1, count
                files(i) = basename(temp_files(i))
            end do
        else
            allocate(character(len=256) :: files(0))
        end if
    end function find_files

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
        integer :: stat
        logical :: exists
        
        error_flag = .false.
        
        ! Check if directory already exists
        inquire(file=path, exist=exists)
        if (exists) return
        
        ! Create directory using mkdir -p
        call execute_command_line("mkdir -p " // path, exitstat=stat)
        error_flag = (stat /= 0)
    end subroutine ensure_directory

    ! Helper functions
    function extract_dir_from_pattern(pattern) result(dir_part)
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: dir_part
        integer :: pos
        
        pos = index(pattern, "/", back=.true.)
        if (pos > 0) then
            dir_part = pattern(1:pos-1)
        else
            dir_part = "."
        end if
    end function extract_dir_from_pattern

    function extract_filename_from_pattern(pattern) result(filename_part)
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: filename_part
        integer :: pos
        
        pos = index(pattern, "/", back=.true.)
        if (pos > 0) then
            filename_part = pattern(pos+1:)
        else
            filename_part = pattern
        end if
        
        ! Remove ** if present
        pos = index(filename_part, "**")
        if (pos > 0) then
            filename_part = filename_part(pos+3:)
        end if
    end function extract_filename_from_pattern

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
        
        logical :: error_flag
        
        call clear_error_context(error_ctx)
        
        call ensure_directory(path, error_flag)
        
        if (error_flag) then
            call handle_permission_denied(path, error_ctx)
        end if
    end subroutine ensure_directory_safe

end module file_utils