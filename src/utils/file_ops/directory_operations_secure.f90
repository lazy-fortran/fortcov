module directory_operations_secure
    use error_handling_core
    use path_security, only: validate_path_security
    use file_operations_secure, only: safe_remove_file
    implicit none
    private

    ! Public procedures
    public :: safe_remove_directory

contains

    subroutine safe_remove_directory(dir_path, error_ctx)
        character(len=*), intent(in) :: dir_path
        type(error_context_t), intent(out) :: error_ctx

        character(len=:), allocatable :: safe_dir_path
        logical :: dir_exists
        type(error_context_t) :: file_error

        call clear_error_context(error_ctx)

        call validate_path_security(dir_path, safe_dir_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return

        inquire (file=safe_dir_path, exist=dir_exists)
        if (.not. dir_exists) then
            return
        end if

        call safe_remove_file(trim(safe_dir_path)//"/.fortcov_temp_dir_marker", &
                              file_error)
        call safe_remove_file(trim(safe_dir_path)//"/cmd_test.txt", file_error)
        call safe_remove_file(trim(safe_dir_path)//"/cleanup_test.txt", file_error)
        call safe_remove_file(trim(safe_dir_path)//"/temp_mgmt_test.tmp", file_error)
        call safe_remove_file(trim(safe_dir_path)//"/isolation_test.txt", file_error)

    end subroutine safe_remove_directory

end module directory_operations_secure
