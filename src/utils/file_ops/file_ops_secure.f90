module file_ops_secure
    !! Secure file operations module - Compatibility wrapper
    !!
    !! This module provides a unified interface to secure file operations
    !! by re-exporting functionality from specialized security modules.
    !! 
    !! DECOMPOSITION: This module now imports from focused smaller modules:
    !! - file_deletion_secure: Secure file deletion and cleanup
    !! - file_search_secure: Pattern-based file finding
    !! - file_operations_secure: Basic file operations (move, remove, mkdir)
    !! - directory_operations_secure: Directory cleanup and removal
    !! - test_infrastructure_secure: Test command replacements
    !!
    !! This maintains API compatibility while providing modular organization
    !! with each module under 500 lines and focused responsibility.
    
    ! Import all functionality from specialized modules
    use file_deletion_secure, only: safe_close_and_delete, secure_overwrite_file
    use file_search_secure, only: safe_find_files, create_secure_temp_filename, get_process_id
    use file_operations_secure, only: safe_mkdir, safe_remove_file, safe_move_file
    use directory_operations_secure, only: safe_remove_directory
    use test_infrastructure_secure, only: safe_test_command_true, safe_test_command_false, &
                                           safe_test_file_list, safe_test_pipe_command, &
                                           safe_create_concurrent_files
    implicit none
    private
    
    ! Re-export all public procedures for API compatibility
    public :: safe_close_and_delete
    public :: safe_find_files
    public :: safe_mkdir
    public :: safe_remove_file
    public :: safe_move_file
    public :: safe_remove_directory
    public :: safe_test_command_true
    public :: safe_test_command_false
    public :: safe_test_file_list
    public :: safe_test_pipe_command
    public :: safe_create_concurrent_files
    
    ! Additional utilities that may be needed by legacy code
    public :: secure_overwrite_file
    public :: create_secure_temp_filename
    public :: get_process_id
    
contains

    ! All functionality now provided by specialized modules via use statements
    ! This module serves as a compatibility wrapper maintaining the existing API

end module file_ops_secure