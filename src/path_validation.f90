module path_validation
    use path_security_core
    implicit none
    private
    
    ! Re-export main procedures for backward compatibility
    public :: validate_path_security
    public :: validate_executable_path

end module path_validation