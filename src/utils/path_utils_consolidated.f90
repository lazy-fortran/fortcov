module path_utils_consolidated
    !! Consolidated path utilities module
    !!
    !! Combines all path operations including:
    !! - Path resolution and normalization
    !! - Path string manipulation utilities
    !! - Path security validation
    !! - Path scanning operations
    
    use error_handling_core
    use constants_core, only: MEDIUM_STRING_LEN
    implicit none
    private
    
    ! Path resolution
    public :: resolve_path
    public :: file_exists
    public :: basename
    
    ! Path string utilities
    public :: normalize_path
    public :: join_paths
    public :: is_absolute_path
    public :: get_file_extension
    
    ! Path security
    public :: validate_path_security
    public :: is_safe_path
    public :: prevent_path_traversal
    
    ! Path scanning
    public :: scan_directory
    public :: find_files_in_directory
    
contains

    ! ========================================================================
    ! Path Resolution Operations
    ! ========================================================================
    
    function resolve_path(path) result(resolved_path)
        !! Resolve relative path to absolute path
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: resolved_path
        character(len=512) :: cwd
        integer :: iostat
        
        if (is_absolute_path(path)) then
            resolved_path = trim(path)
        else
            call getcwd(cwd, iostat)
            if (iostat == 0) then
                resolved_path = join_paths(trim(cwd), path)
            else
                resolved_path = trim(path)
            end if
        end if
    end function resolve_path
    
    function file_exists(filename) result(exists)
        !! Check if file exists
        character(len=*), intent(in) :: filename
        logical :: exists
        inquire(file=filename, exist=exists)
    end function file_exists
    
    function basename(path) result(name)
        !! Extract basename from path
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: name
        integer :: last_slash
        
        last_slash = index(path, "/", back=.true.)
        if (last_slash > 0) then
            name = trim(path(last_slash+1:))
        else
            name = trim(path)
        end if
    end function basename

    ! ========================================================================
    ! Path String Utilities
    ! ========================================================================
    
    function normalize_path(path) result(normalized)
        !! Normalize path by removing redundant elements
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: normalized
        character(len=len(path)+10) :: work_path
        integer :: i, j, len_path
        
        work_path = trim(path)
        len_path = len_trim(work_path)
        
        ! Remove redundant slashes
        j = 1
        do i = 1, len_path
            if (i == 1 .or. work_path(i:i) /= '/' .or. work_path(i-1:i-1) /= '/') then
                work_path(j:j) = work_path(i:i)
                j = j + 1
            end if
        end do
        
        normalized = trim(work_path(1:j-1))
        
        ! Handle empty path
        if (len_trim(normalized) == 0) then
            normalized = "."
        end if
    end function normalize_path
    
    function join_paths(path1, path2) result(joined)
        !! Join two paths with proper separator handling
        character(len=*), intent(in) :: path1, path2
        character(len=:), allocatable :: joined
        
        if (len_trim(path1) == 0) then
            joined = trim(path2)
        else if (len_trim(path2) == 0) then
            joined = trim(path1)
        else if (path1(len_trim(path1):len_trim(path1)) == '/') then
            joined = trim(path1) // trim(path2)
        else
            joined = trim(path1) // '/' // trim(path2)
        end if
    end function join_paths
    
    function is_absolute_path(path) result(is_absolute)
        !! Check if path is absolute
        character(len=*), intent(in) :: path
        logical :: is_absolute
        
        is_absolute = .false.
        if (len_trim(path) > 0) then
            is_absolute = (path(1:1) == '/')
        end if
    end function is_absolute_path
    
    function get_file_extension(filename) result(extension)
        !! Extract file extension
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: extension
        integer :: last_dot
        
        last_dot = index(filename, ".", back=.true.)
        if (last_dot > 0 .and. last_dot < len_trim(filename)) then
            extension = filename(last_dot+1:len_trim(filename))
        else
            extension = ""
        end if
    end function get_file_extension

    ! ========================================================================
    ! Path Security Operations
    ! ========================================================================
    
    subroutine validate_path_security(path, error_ctx)
        !! Validate path for security vulnerabilities
        character(len=*), intent(in) :: path
        type(error_context_t), intent(out) :: error_ctx
        
        call clear_error_context(error_ctx)
        
        if (.not. is_safe_path(path)) then
            call prevent_path_traversal(path, error_ctx)
        end if
    end subroutine validate_path_security
    
    function is_safe_path(path) result(is_safe)
        !! Check if path is safe from traversal attacks
        character(len=*), intent(in) :: path
        logical :: is_safe
        
        is_safe = .true.
        
        ! Check for path traversal patterns
        if (index(path, "../") > 0 .or. index(path, "/..") > 0) then
            is_safe = .false.
        end if
        
        ! Check for null bytes
        if (index(path, char(0)) > 0) then
            is_safe = .false.
        end if
        
        ! Check for extremely long paths
        if (len_trim(path) > 4096) then
            is_safe = .false.
        end if
    end function is_safe_path
    
    subroutine prevent_path_traversal(path, error_ctx)
        !! Handle path traversal attack prevention
        character(len=*), intent(in) :: path
        type(error_context_t), intent(out) :: error_ctx
        
        error_ctx%error_code = ERROR_INVALID_CONFIG
        write(error_ctx%message, '(A)') "Path security violation detected"
        write(error_ctx%suggestion, '(A)') &
            "Ensure path does not contain traversal sequences like ../"
        write(error_ctx%context, '(A)') "Path validation"
    end subroutine prevent_path_traversal

    ! ========================================================================
    ! Path Scanning Operations
    ! ========================================================================
    
    function scan_directory(directory) result(files)
        !! Scan directory for files
        character(len=*), intent(in) :: directory
        character(len=:), allocatable :: files(:)
        
        ! Simplified implementation - in production would use system calls
        allocate(character(len=256) :: files(0))
    end function scan_directory
    
    function find_files_in_directory(directory, pattern) result(files)
        !! Find files matching pattern in directory
        character(len=*), intent(in) :: directory, pattern
        character(len=:), allocatable :: files(:)
        
        ! Simplified implementation - in production would use glob matching
        allocate(character(len=256) :: files(0))
    end function find_files_in_directory

end module path_utils_consolidated