module file_search_secure
    use error_handling_core, only: clear_error_context, error_context_t, &
                                   ERROR_SUCCESS
    use path_security, only: validate_path_security
    use, intrinsic :: iso_c_binding, only: c_char, c_f_pointer, c_int, c_loc
    use, intrinsic :: iso_c_binding, only: c_null_ptr, c_ptr, c_size_t
    implicit none
    private

    ! Public procedures
    public :: safe_find_files
    public :: safe_find_files_with_glob  ! Enhanced API for directory+pattern
    public :: safe_find_files_recursive  ! Enhanced API for recursive search
    public :: create_secure_temp_filename
    public :: get_process_id

    ! C interop for glob()
    interface
        function c_glob(cpattern, flags, errfunc, pglob) bind(C, name="glob") &
            result(rc)
            import :: c_char, c_int, c_ptr
            character(kind=c_char), dimension(*) :: cpattern
            integer(c_int), value :: flags
            type(c_ptr), value :: errfunc
            type(c_ptr), value :: pglob
            integer(c_int) :: rc
        end function c_glob
        subroutine c_globfree(pglob) bind(C, name="globfree")
            import :: c_ptr
            type(c_ptr), value :: pglob
        end subroutine c_globfree
        function c_strlen(cstr) bind(C, name="strlen") result(n)
            import :: c_ptr, c_size_t
            type(c_ptr), value :: cstr
            integer(c_size_t) :: n
        end function c_strlen
    end interface

    type, bind(C) :: glob_t
        integer(c_size_t) :: gl_pathc
        type(c_ptr) :: gl_pathv
        integer(c_size_t) :: gl_offs
        integer(c_int) :: gl_flags
        type(c_ptr) :: gl_closedir
        type(c_ptr) :: gl_readdir
        type(c_ptr) :: gl_opendir
        type(c_ptr) :: gl_lstat
        type(c_ptr) :: gl_stat
    end type glob_t

    integer, parameter :: GLOB_MARK = 2
    integer, parameter :: GLOB_NOSORT = 32

contains

    subroutine safe_find_files(pattern, files, error_ctx)
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable, intent(out) :: files(:)
        type(error_context_t), intent(out) :: error_ctx

        character(len=:), allocatable :: safe_pattern

        call clear_error_context(error_ctx)

        call validate_path_security(pattern, safe_pattern, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            ! Leave files unallocated on validation error
            return
        end if
        call fortran_find_files(safe_pattern, files, error_ctx)

    end subroutine safe_find_files

    ! Enhanced API: Find files with directory and glob pattern
    subroutine safe_find_files_with_glob(directory, pattern, files, error_ctx)
        character(len=*), intent(in) :: directory, pattern
        character(len=:), allocatable, intent(out) :: files(:)
        type(error_context_t), intent(out) :: error_ctx

        character(len=512) :: full_pattern

        call clear_error_context(error_ctx)

        ! Construct full search pattern
        if (trim(directory) == "." .or. len_trim(directory) == 0) then
            full_pattern = trim(pattern)
        else
            full_pattern = trim(directory)//"/"//trim(pattern)
        end if

        ! Use existing safe_find_files implementation
        call safe_find_files(full_pattern, files, error_ctx)

    end subroutine safe_find_files_with_glob

    ! Enhanced API: Recursive file finding
    subroutine safe_find_files_recursive(base_dir, pattern, files, error_ctx)
        character(len=*), intent(in) :: base_dir, pattern
        character(len=:), allocatable, intent(out) :: files(:)
        type(error_context_t), intent(out) :: error_ctx

        character(len=512) :: recursive_pattern

        call clear_error_context(error_ctx)

        ! Construct recursive search pattern using ** wildcard
        if (trim(base_dir) == "." .or. len_trim(base_dir) == 0) then
            recursive_pattern = "**/"//trim(pattern)
        else
            recursive_pattern = trim(base_dir)//"/**/"//trim(pattern)
        end if

        ! Use existing safe_find_files implementation
        call safe_find_files(recursive_pattern, files, error_ctx)

    end subroutine safe_find_files_recursive

    subroutine fortran_find_files(pattern, files, error_ctx)
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable, intent(out) :: files(:)
        type(error_context_t), intent(out) :: error_ctx

        character(len=256) :: base_dir, file_pattern
        integer :: star_pos
        character(len=256), allocatable :: matches(:)

        call clear_error_context(error_ctx)

        if (index(pattern, '**/') > 0) then
            ! Recursive form: <base>/**/<file_pattern>
            star_pos = index(pattern, '**/')
            if (star_pos > 1) then
                base_dir = pattern(1:star_pos - 1)
            else
                base_dir = '.'
            end if
            file_pattern = pattern(star_pos + 3:)
            call recursive_glob_collect(trim(base_dir), trim(file_pattern), matches)
        else if (index(pattern, '/') > 0) then
            ! Single directory + pattern
            star_pos = index(pattern, '/', back=.true.)
            base_dir = pattern(1:star_pos - 1)
            file_pattern = pattern(star_pos + 1:)
            call glob_collect(trim(base_dir)//'/'//trim(file_pattern), 0, matches)
        else
            ! Current directory only
            call glob_collect(trim(pattern), 0, matches)
        end if

        if (allocated(matches)) then
            allocate (character(len=256) :: files(size(matches)))
            files = pad_or_trim(matches)
        else
            allocate (character(len=1) :: files(0))
        end if

    end subroutine fortran_find_files

    subroutine glob_collect(pattern, flags, paths)
        character(len=*), intent(in) :: pattern
        integer, intent(in) :: flags
        character(len=256), allocatable, intent(out) :: paths(:)

        type(glob_t), target :: g
        integer(c_int) :: rc
        character(kind=c_char), allocatable :: cpat(:)
        type(c_ptr), pointer :: pathv(:)
        integer :: i
        integer(c_size_t) :: n

        ! Initialize struct to safe defaults
        g%gl_pathc = 0_c_size_t
        g%gl_pathv = c_null_ptr
        g%gl_offs = 0_c_size_t
        g%gl_flags = 0
        g%gl_closedir = c_null_ptr
        g%gl_readdir = c_null_ptr
        g%gl_opendir = c_null_ptr
        g%gl_lstat = c_null_ptr
        g%gl_stat = c_null_ptr

        allocate (cpat(len_trim(pattern) + 1))
        call to_c_string(pattern, cpat)
        rc = c_glob(cpat, flags, c_null_ptr, c_loc(g))
        if (rc /= 0 .or. g%gl_pathc == 0) then
            allocate (character(len=256) :: paths(0))
            deallocate (cpat)
            return
        end if

        call c_f_pointer(g%gl_pathv, pathv, [g%gl_pathc])
        allocate (character(len=256) :: paths(int(g%gl_pathc)))
        do i = 1, int(g%gl_pathc)
            n = c_strlen(pathv(i))
            paths(i) = from_c_string(pathv(i), int(n))
        end do
        call c_globfree(c_loc(g))
        deallocate (cpat)
    end subroutine glob_collect

    recursive subroutine recursive_glob_collect(base_dir, file_pattern, paths)
        character(len=*), intent(in) :: base_dir, file_pattern
        character(len=256), allocatable, intent(out) :: paths(:)
        character(len=256), allocatable :: acc(:)
        character(len=256), allocatable :: here(:), dirs(:)
        character(len=256), allocatable :: sub(:)
        integer :: i

        call glob_collect(trim(base_dir)//'/'//trim(file_pattern), &
                          GLOB_NOSORT, here)
        call glob_collect(trim(base_dir)//'/*', GLOB_MARK, dirs)
        acc = here
        if (allocated(dirs)) then
            do i = 1, size(dirs)
                if (is_dir_marked(dirs(i))) then
                    call recursive_glob_collect(trim(strip_dir_mark(dirs(i))), &
                                                trim(file_pattern), sub)
                    acc = append_paths(acc, sub)
                end if
            end do
        end if
        paths = acc
    end subroutine recursive_glob_collect

    pure function is_dir_marked(path) result(isdir)
        character(len=*), intent(in) :: path
        logical :: isdir
        integer :: n
        n = len_trim(path)
        isdir = (n > 0 .and. path(n:n) == '/')
    end function is_dir_marked

    pure function strip_dir_mark(path) result(clean)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: clean
        integer :: n
        n = len_trim(path)
        if (n > 0 .and. path(n:n) == '/') then
            clean = path(1:n - 1)
        else
            clean = trim(path)
        end if
    end function strip_dir_mark

    subroutine to_c_string(src, dest)
        character(len=*), intent(in) :: src
        character(kind=c_char), intent(out) :: dest(:)
        integer :: i, n
        n = len_trim(src)
        do i = 1, n
            dest(i) = src(i:i)
        end do
        dest(n + 1) = char(0, kind=c_char)
    end subroutine to_c_string

    function from_c_string(cstr, n) result(fstr)
        type(c_ptr), intent(in) :: cstr
        integer, intent(in) :: n
        character(len=:), allocatable :: fstr
        character(kind=c_char), pointer :: buf(:)
        integer :: i
        call c_f_pointer(cstr, buf, [n])
        allocate (character(len=n) :: fstr)
        do i = 1, n
            fstr(i:i) = buf(i)
        end do
    end function from_c_string

    pure function append_paths(a, b) result(out)
        character(len=256), intent(in), optional :: a(:), b(:)
        character(len=256), allocatable :: out(:)
        integer :: na, nb
        na = 0; nb = 0
        if (present(a)) na = size(a)
        if (present(b)) nb = size(b)
        if (na + nb <= 0) then
            allocate (character(len=256) :: out(0))
            return
        end if
        allocate (character(len=256) :: out(na + nb))
        if (na > 0) out(1:na) = a
        if (nb > 0) out(na + 1:na + nb) = b
    end function append_paths

    pure function pad_or_trim(arr) result(out)
        character(len=256), intent(in) :: arr(:)
        character(len=256) :: out(size(arr))
        out = arr
    end function pad_or_trim

    ! Create secure temporary filename
    subroutine create_secure_temp_filename(temp_filename)
        character(len=:), allocatable, intent(out) :: temp_filename

        integer :: pid
        character(len=16) :: pid_str

        call get_process_id(pid)
        write (pid_str, '(I0)') pid
        temp_filename = "/tmp/fortcov_"//trim(pid_str)//"_temp.txt"
    end subroutine create_secure_temp_filename

    ! Get process ID helper
    subroutine get_process_id(pid)
        integer, intent(out) :: pid
        interface
            function c_getpid() bind(C, name="getpid") result(c_pid)
                import :: c_int
                integer(c_int) :: c_pid
            end function c_getpid
        end interface
        pid = int(c_getpid())
        if (pid <= 0) pid = 1
    end subroutine get_process_id

end module file_search_secure
