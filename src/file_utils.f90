module file_utils
    use iso_fortran_env, only: error_unit
    implicit none
    private
    
    ! Public procedures
    public :: find_files
    public :: resolve_path
    public :: read_binary_file
    public :: write_text_file
    public :: ensure_directory

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

end module file_utils