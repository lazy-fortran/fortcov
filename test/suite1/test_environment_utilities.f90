module test_environment_utilities
    !! Test environment setup and cleanup utilities
    !! Manages test directories, files, and cleanup operations
    
    implicit none
    private
    
    public :: setup_basic_test_environment, cleanup_basic_test_environment
    public :: cleanup_test_files_pattern
    
contains
    
    subroutine setup_basic_test_environment(test_name)
        character(len=*), intent(in) :: test_name
        
        print *, "Setting up test environment for: ", test_name
        
        ! Create basic test directory structure
        ! call execute_command_line( ! CI-disabled:'mkdir -p test_temp')
        ! call execute_command_line( ! CI-disabled:'mkdir -p test_output')
        
        ! Set proper permissions
        ! call execute_command_line( ! CI-disabled:'chmod 755 test_temp test_output 2>/dev/null || true')
    end subroutine setup_basic_test_environment
    
    subroutine cleanup_basic_test_environment(test_name)
        character(len=*), intent(in) :: test_name
        
        print *, "Cleaning up test environment for: ", test_name
        
        ! Remove test directories
        ! call execute_command_line( ! CI-disabled:'rm -rf test_temp test_output')
        
        ! Clean up common test files
        ! call execute_command_line( ! CI-disabled:'rm -f *.tmp *.temp *.test *.gcov *.gcda *.gcno')
        ! Use find to only remove files, not directories
        ! Remove test files with simple glob patterns (safer than find with parentheses)
        ! call execute_command_line( ! CI-disabled:'rm -f test_* temp_* output_* coverage_* 2>/dev/null || true')
    end subroutine cleanup_basic_test_environment
    
    subroutine cleanup_test_files_pattern(patterns)
        character(len=*), intent(in) :: patterns(:)
        integer :: i
        
        do i = 1, size(patterns)
            ! call execute_command_line( ! CI-disabled:'rm -f ' // trim(patterns(i)))
        end do
    end subroutine cleanup_test_files_pattern
    
end module test_environment_utilities