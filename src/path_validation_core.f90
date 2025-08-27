module path_validation_core
    use path_security_core, only: validate_path_security, validate_executable_path
    implicit none
    private
    
    ! Re-export main procedures for backward compatibility
    public :: validate_path_security
    public :: validate_executable_path

end module path_validation_core