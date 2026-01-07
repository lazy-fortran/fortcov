module build_detector_core
    !! Build System Detection Module
    !!
    !! Provides comprehensive detection of build systems (FPM, CMake, Make, 
    !! Meson) based on their marker files. Returns build system type,
    !! metadata including test commands, and validates build tools are 
    !! available in PATH.
    !!
    !! Priority order: FPM > CMake > Make > Meson > unknown
    !!
    !! This module uses secure_command_executor for tool validation to 
    !! prevent command injection vulnerabilities.

    use iso_fortran_env, only: error_unit
    use error_handling_core
    use path_security, only: validate_executable_path, validate_path_security
    use file_utilities, only: file_exists, find_files_with_glob
    use string_utils, only: trim_string
    implicit none
    private
    
    ! Build system type definition
    type :: build_system_info_t
        character(len=20) :: system_type = 'unknown'
        character(len=512) :: test_command = ''
        character(len=256) :: build_file = ''
        character(len=256) :: cmake_build_dir = ''
        logical :: tool_available = .false.
    end type build_system_info_t
    
    ! Public interface
    public :: build_system_info_t
    public :: detect_build_system
    public :: get_coverage_test_command
    public :: validate_build_tool_available
    
    ! Internal constants
    integer, parameter :: MAX_PATH_LEN = 4096
    
    ! Build system marker files in priority order
    character(len=*), parameter :: FPM_MARKER = 'fpm.toml'
    character(len=*), parameter :: CMAKE_MARKER = 'CMakeLists.txt'  
    character(len=*), parameter :: MAKE_MARKER = 'Makefile'
    character(len=*), parameter :: MESON_MARKER = 'meson.build'
    
    ! Build tool executables
    character(len=*), parameter :: FPM_TOOL = 'fpm'
    character(len=*), parameter :: CMAKE_TOOL = 'cmake'
    character(len=*), parameter :: MAKE_TOOL = 'make'
    character(len=*), parameter :: MESON_TOOL = 'meson'

contains

    subroutine detect_build_system(project_path, build_info, error_ctx)
        !! Main detection routine for build systems
        !!
        !! Scans project directory for build system marker files and 
        !! determines the appropriate build system type with priority
        !! ordering. Validates that build tools are available.
        !!
        !! Args:
        !!   project_path: Directory to scan for build files
        !!   build_info: Populated with detection results  
        !!   error_ctx: Error context for failure reporting
        
        character(len=*), intent(in) :: project_path
        type(build_system_info_t), intent(out) :: build_info
        type(error_context_t), intent(out) :: error_ctx
        
        character(len=:), allocatable :: safe_path
        character(len=512) :: marker_path
        logical :: path_valid
        
        call clear_error_context(error_ctx)
        
        ! Validate input path
        if (len_trim(project_path) == 0) then
            error_ctx%error_code = ERROR_MISSING_FILE
            error_ctx%message = 'Empty project path provided'
            return
        end if
        
        if (len_trim(project_path) > MAX_PATH_LEN) then
            error_ctx%error_code = ERROR_INVALID_PATH
            error_ctx%message = 'Project path too long'
            return
        end if
        
        ! Secure path validation
        call validate_path_security(project_path, safe_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! Only hard-error for clearly invalid absolute paths to avoid
        ! compiler-dependent directory INQUIRE behavior on relative paths.
        if (len_trim(safe_path) > 0 .and. safe_path(1:1) == '/') then
            inquire(file=safe_path, exist=path_valid)
            if (.not. path_valid) then
                error_ctx%error_code = ERROR_MISSING_FILE
                error_ctx%message = 'Project directory not found: ' // safe_path
                return
            end if
        end if
        
        ! Initialize build_info with defaults
        build_info%system_type = 'unknown'
        build_info%test_command = ''
        build_info%build_file = ''
        build_info%cmake_build_dir = ''
        build_info%tool_available = .false.
        
        ! Check for FPM (highest priority)
        call construct_marker_path(safe_path, FPM_MARKER, marker_path)
        if (file_exists(marker_path)) then
            call configure_fpm_system(build_info, error_ctx)
            if (error_ctx%error_code == ERROR_SUCCESS) return
        end if
        
        ! Check for CMake  
        call construct_marker_path(safe_path, CMAKE_MARKER, marker_path)
        if (file_exists(marker_path)) then
            call configure_cmake_system(build_info, error_ctx, safe_path)
            if (error_ctx%error_code == ERROR_SUCCESS) return
        end if
        
        ! Check for Make
        call construct_marker_path(safe_path, MAKE_MARKER, marker_path)
        if (file_exists(marker_path)) then
            call configure_make_system(build_info, error_ctx)
            if (error_ctx%error_code == ERROR_SUCCESS) return
        end if
        
        ! Check for Meson (lowest priority)
        call construct_marker_path(safe_path, MESON_MARKER, marker_path)
        if (file_exists(marker_path)) then
            call configure_meson_system(build_info, error_ctx)
            if (error_ctx%error_code == ERROR_SUCCESS) return
        end if
        
        ! No known build system found - return unknown but no error
        build_info%system_type = 'unknown'
        build_info%test_command = ''
        build_info%build_file = ''
        build_info%tool_available = .false.
        
    end subroutine detect_build_system

    subroutine get_coverage_test_command(build_info, command, error_ctx)
        !! Generate coverage-enabled test command for build system
        !!
        !! Returns the appropriate test command with coverage flags
        !! enabled for the specified build system type.
        !!
        !! Args:
        !!   build_info: Build system information
        !!   command: Generated test command with coverage flags
        !!   error_ctx: Error context for unsupported systems
        
        type(build_system_info_t), intent(in) :: build_info
        character(len=*), intent(out) :: command
        type(error_context_t), intent(out) :: error_ctx
        
        call clear_error_context(error_ctx)
        
        select case (trim(build_info%system_type))
        case ('fpm')
            command = 'fpm test --flag "-fprofile-arcs -ftest-coverage"'
        case ('cmake')
            command = build_cmake_test_command(build_info%cmake_build_dir)
        case ('make')
            command = 'make test'
        case ('meson')
            command = 'meson test'
        case ('unknown')
            error_ctx%error_code = ERROR_INVALID_PATH
            error_ctx%message = 'Cannot generate command for unknown ' // &
                               'build system'
            command = ''
        case default
            error_ctx%error_code = ERROR_INVALID_PATH
            error_ctx%message = 'Unsupported build system: ' // &
                               trim(build_info%system_type)
            command = ''
        end select
        
    end subroutine get_coverage_test_command

    function validate_build_tool_available(tool_name) result(available)
        !! Check if build tool is available in system PATH
        !!
        !! Uses secure command executor to safely validate tool 
        !! availability without command injection risks.
        !!
        !! Args:
        !!   tool_name: Name of build tool executable
        !! 
        !! Returns:
        !!   available: True if tool is found and executable
        
        character(len=*), intent(in) :: tool_name
        logical :: available
        
        character(len=:), allocatable :: safe_tool_path
        type(error_context_t) :: error_ctx
        
        ! Use secure validator to check tool existence
        call validate_executable_path(tool_name, safe_tool_path, error_ctx)
        
        available = (error_ctx%error_code == ERROR_SUCCESS) .and. &
                   allocated(safe_tool_path)
        
        if (allocated(safe_tool_path)) deallocate(safe_tool_path)
        
    end function validate_build_tool_available

    ! Internal configuration subroutines
    subroutine configure_fpm_system(build_info, error_ctx)
        !! Configure build_info for FPM build system
        type(build_system_info_t), intent(out) :: build_info
        type(error_context_t), intent(out) :: error_ctx
        
        call clear_error_context(error_ctx)
        
        build_info%system_type = 'fpm'
        build_info%build_file = FPM_MARKER
        build_info%test_command = &
            'fpm test --flag "-fprofile-arcs -ftest-coverage"'
        build_info%tool_available = validate_build_tool_available(FPM_TOOL)
        
    end subroutine configure_fpm_system

    subroutine configure_cmake_system(build_info, error_ctx, project_path)
        !! Configure build_info for CMake build system  
        type(build_system_info_t), intent(out) :: build_info
        type(error_context_t), intent(out) :: error_ctx
        character(len=*), intent(in) :: project_path
        character(len=:), allocatable :: detected_build_dir
        
        call clear_error_context(error_ctx)
        
        build_info%system_type = 'cmake'
        build_info%build_file = CMAKE_MARKER
        detected_build_dir = detect_cmake_build_dir(project_path)
        if (allocated(detected_build_dir)) then
            build_info%cmake_build_dir = detected_build_dir
        end if
        build_info%test_command = build_cmake_test_command( &
            build_info%cmake_build_dir)
        build_info%tool_available = validate_build_tool_available(CMAKE_TOOL)
        
    end subroutine configure_cmake_system

    subroutine configure_make_system(build_info, error_ctx)
        !! Configure build_info for Make build system
        type(build_system_info_t), intent(out) :: build_info
        type(error_context_t), intent(out) :: error_ctx
        
        call clear_error_context(error_ctx)
        
        build_info%system_type = 'make'
        build_info%build_file = MAKE_MARKER
        build_info%test_command = 'make test'
        build_info%tool_available = validate_build_tool_available(MAKE_TOOL)
        
    end subroutine configure_make_system

    subroutine configure_meson_system(build_info, error_ctx)
        !! Configure build_info for Meson build system
        type(build_system_info_t), intent(out) :: build_info
        type(error_context_t), intent(out) :: error_ctx
        
        call clear_error_context(error_ctx)
        
        build_info%system_type = 'meson' 
        build_info%build_file = MESON_MARKER
        build_info%test_command = 'meson test'
        build_info%tool_available = validate_build_tool_available(MESON_TOOL)
        
    end subroutine configure_meson_system

    subroutine construct_marker_path(base_path, marker_file, full_path)
        !! Safely construct path to marker file
        !!
        !! Combines base path and marker filename with proper 
        !! separator handling and length validation.
        
        character(len=*), intent(in) :: base_path, marker_file
        character(len=*), intent(out) :: full_path
        
        character(len=1) :: path_sep
        integer :: base_len
        
        ! Determine path separator 
        path_sep = '/'
        
        base_len = len_trim(base_path)
        
        ! Handle different base path cases
        if (base_len == 0 .or. trim(base_path) == '.') then
            full_path = trim(marker_file)
        else if (base_path(base_len:base_len) == path_sep) then
            full_path = trim(base_path) // trim(marker_file)
        else
            full_path = trim(base_path) // path_sep // trim(marker_file)
        end if
        
    end subroutine construct_marker_path

    function detect_cmake_build_dir(project_path) result(build_dir)
        character(len=*), intent(in) :: project_path
        character(len=:), allocatable :: build_dir
        character(len=:), allocatable :: cache_files(:)
        character(len=32), parameter :: candidates(6) = &
            [character(len=32) :: 'build', '_build', 'cmake-build-debug', &
             'cmake-build-release', 'cmake-build-relwithdebinfo', &
             'cmake-build-minsizerel']
        integer :: i

        do i = 1, size(candidates)
            if (find_cmake_cache_dir(project_path, candidates(i), &
                                     build_dir)) then
                return
            end if
        end do

        cache_files = find_files_with_glob(project_path, &
                                           'cmake-build-*/CMakeCache.txt')
        if (allocated(cache_files)) then
            do i = 1, size(cache_files)
                if (len_trim(cache_files(i)) > 0) then
                    build_dir = extract_parent_dir(cache_files(i))
                    if (allocated(build_dir)) return
                end if
            end do
        end if

        cache_files = find_files_with_glob(project_path, '**/CMakeCache.txt')
        if (allocated(cache_files)) then
            do i = 1, size(cache_files)
                if (len_trim(cache_files(i)) > 0) then
                    build_dir = extract_parent_dir(cache_files(i))
                    if (allocated(build_dir)) return
                end if
            end do
        end if
    end function detect_cmake_build_dir

    logical function find_cmake_cache_dir(project_path, candidate, build_dir)
        character(len=*), intent(in) :: project_path
        character(len=*), intent(in) :: candidate
        character(len=:), allocatable, intent(out) :: build_dir
        character(len=512) :: candidate_dir
        character(len=512) :: candidate_cache

        call construct_marker_path(project_path, trim(candidate), &
                                   candidate_dir)
        call construct_marker_path(candidate_dir, 'CMakeCache.txt', &
                                   candidate_cache)
        if (file_exists(candidate_cache)) then
            build_dir = trim(candidate_dir)
            find_cmake_cache_dir = .true.
        else
            find_cmake_cache_dir = .false.
        end if
    end function find_cmake_cache_dir

    function extract_parent_dir(path) result(parent_dir)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: parent_dir
        integer :: last_sep

        last_sep = index(path, '/', back=.true.)
        if (last_sep > 0) then
            parent_dir = path(1:last_sep-1)
        end if
    end function extract_parent_dir

    function build_cmake_test_command(build_dir) result(command)
        character(len=*), intent(in) :: build_dir
        character(len=:), allocatable :: command
        character(len=2), parameter :: shell_and = achar(38)//achar(38)

        if (len_trim(build_dir) > 0) then
            command = 'cmake --build '//trim(build_dir)//' '//shell_and// &
                      ' ctest --test-dir '//trim(build_dir)
        else
            command = 'cmake --build . '//shell_and//' ctest'
        end if
    end function build_cmake_test_command

end module build_detector_core
