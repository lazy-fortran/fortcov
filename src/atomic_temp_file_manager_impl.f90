module atomic_temp_file_manager_impl
    use atomic_temp_file_core
    implicit none
    private
    
    ! Re-export types and procedures for backward compatibility
    public :: secure_temp_file_t
    public :: get_secure_temp_directory
    public :: is_unix

end module atomic_temp_file_manager_impl