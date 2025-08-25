module platform_detection
    !! Platform Detection Utilities
    !!
    !! Provides cross-platform detection for Unix/Windows environments.
    !! Extracted from atomic_temp_file_manager_impl.f90 for SRP compliance.
    implicit none
    private

    ! Public procedures
    public :: get_platform_is_unix
    public :: is_unix

contains

    ! Detect if current platform is Unix-based
    function get_platform_is_unix() result(is_unix_result)
        logical :: is_unix_result
        
        is_unix_result = is_unix()
    end function get_platform_is_unix

    ! Check if running on Unix platform
    function is_unix() result(unix_platform)
        logical :: unix_platform
        
        ! Simple platform detection without preprocessor directives
        ! This is a basic implementation that can be enhanced
        unix_platform = .true.  ! Assume Unix for now
    end function is_unix

end module platform_detection