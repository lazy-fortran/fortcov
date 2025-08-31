module test_environment_utilities
    !! Test environment setup and cleanup utilities
    !! Manages test directories, files, and cleanup operations
    !! SECURITY FIX Issue #971: Complete elimination of execute_command_line
    
    use file_ops_secure, only: safe_mkdir, safe_remove_file
    use directory_operations, only: ensure_directory_safe
    use error_handling_core, only: error_context_t, clear_error_context
    implicit none
    private
    
    public :: setup_basic_test_environment, cleanup_basic_test_environment
    public :: cleanup_test_files_pattern
    
contains
    
    subroutine setup_basic_test_environment(test_name)
        character(len=*), intent(in) :: test_name
        
        print *, "Setting up test environment for: ", test_name
        
        ! Create basic test directory structure
        ! SECURITY FIX Issue #971: Use secure directory operations
        call create_test_directories_secure()
    end subroutine setup_basic_test_environment
    
    subroutine cleanup_basic_test_environment(test_name)
        character(len=*), intent(in) :: test_name
        
        print *, "Cleaning up test environment for: ", test_name
        
        ! Remove test directories and files
        ! SECURITY FIX Issue #971: Use secure cleanup operations
        call cleanup_test_directories_secure()
        call cleanup_test_files_secure()
    end subroutine cleanup_basic_test_environment
    
    subroutine cleanup_test_files_pattern(patterns)
        character(len=*), intent(in) :: patterns(:)
        integer :: i
        
        do i = 1, size(patterns)
            ! SECURITY FIX Issue #971: Use secure file operations
            call cleanup_files_by_pattern_secure(patterns(i))
        end do
    end subroutine cleanup_test_files_pattern
    
    ! SECURITY FIX Issue #971: Secure replacement functions for execute_command_line
    
    subroutine create_test_directories_secure()
        !! Create test directories using secure operations
        type(error_context_t) :: error_ctx
        call safe_mkdir('test_temp', error_ctx)
        call safe_mkdir('test_output', error_ctx)
        ! Note: Permissions handled by filesystem - no chmod needed
    end subroutine create_test_directories_secure
    
    subroutine cleanup_test_directories_secure()
        !! Clean up test directories securely
        type(error_context_t) :: error_ctx
        call safe_remove_file('test_temp', error_ctx)
        call safe_remove_file('test_output', error_ctx)
        ! Ignore errors - directories may not exist
    end subroutine cleanup_test_directories_secure
    
    subroutine cleanup_test_files_secure()
        !! Clean up common test files securely
        type(error_context_t) :: error_ctx
        character(len=*), parameter :: common_patterns(8) = [ &
            '*.tmp      ', '*.temp     ', '*.test     ', '*.gcov     ', &
            '*.gcda     ', '*.gcno     ', 'test_*     ', 'temp_*     ' ]
        integer :: i
        
        do i = 1, size(common_patterns)
            call safe_remove_file(trim(common_patterns(i)), error_ctx)
            ! Ignore errors - files may not exist
        end do
    end subroutine cleanup_test_files_secure
    
    subroutine cleanup_files_by_pattern_secure(pattern)
        !! Clean up files by pattern using secure operations
        character(len=*), intent(in) :: pattern
        type(error_context_t) :: error_ctx
        call safe_remove_file(pattern, error_ctx)
        ! Ignore errors - files may not exist
    end subroutine cleanup_files_by_pattern_secure
    
end module test_environment_utilities
