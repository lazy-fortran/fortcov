module directory_operations
    use error_handling
    use secure_file_operations, only: safe_mkdir
    implicit none
    private
    
    ! Public procedures
    public :: ensure_directory
    public :: ensure_directory_safe
    
contains

    subroutine ensure_directory(path, error_flag)
        character(len=*), intent(in) :: path
        logical, intent(out) :: error_flag
        type(error_context_t) :: error_ctx
        
        error_flag = .false.
        
        ! Use secure command executor for safe directory creation
        call safe_mkdir(path, error_ctx)
        error_flag = (error_ctx%error_code /= ERROR_SUCCESS)
    end subroutine ensure_directory
    
    ! Safe directory creation with comprehensive error context
    subroutine ensure_directory_safe(path, error_ctx)
        character(len=*), intent(in) :: path
        type(error_context_t), intent(out) :: error_ctx
        
        call clear_error_context(error_ctx)
        
        ! Use secure command executor directly
        call safe_mkdir(path, error_ctx)
    end subroutine ensure_directory_safe

end module directory_operations