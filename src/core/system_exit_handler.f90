module system_exit_handler
    !! System Exit Handler Module
    !! 
    !! Provides unified exit handling with proper Unix exit codes.
    !! Replaces STOP statements with proper exit() calls for CI/CD integration.
    !! Addresses Issue #865 - Exit code regression.
    use constants_core, only: EXIT_SUCCESS, EXIT_FAILURE, EXIT_THRESHOLD_NOT_MET, &
                              EXIT_NO_COVERAGE_DATA, EXIT_INVALID_CONFIG, &
                              EXIT_FILE_ACCESS_ERROR, EXIT_MEMORY_ERROR, &
                              EXIT_VALIDATION_ERROR
    implicit none
    private
    
    ! Public interface for exit handling
    public :: exit_with_code
    public :: exit_success_clean
    public :: exit_failure_clean
    public :: exit_invalid_config_clean
    public :: exit_no_coverage_data_clean
    public :: exit_threshold_not_met_clean
    public :: exit_file_access_error_clean
    public :: exit_memory_error_clean
    public :: exit_validation_error_clean
    
contains
    
    subroutine exit_with_code(exit_code, message)
        !! Exits program with specified exit code and optional message
        integer, intent(in) :: exit_code
        character(len=*), intent(in), optional :: message
        
        if (present(message)) then
            if (exit_code /= EXIT_SUCCESS) then
                write(*, '(A)') trim(message)
            end if
        end if
        
        call exit(exit_code)
        
    end subroutine exit_with_code
    
    subroutine exit_success_clean(message)
        !! Exits with success code (0)
        character(len=*), intent(in), optional :: message
        
        if (present(message)) then
            call exit_with_code(EXIT_SUCCESS, message)
        else
            call exit_with_code(EXIT_SUCCESS)
        end if
        
    end subroutine exit_success_clean
    
    subroutine exit_failure_clean(message)
        !! Exits with general failure code (1)
        character(len=*), intent(in), optional :: message
        
        if (present(message)) then
            call exit_with_code(EXIT_FAILURE, message)
        else
            call exit_with_code(EXIT_FAILURE)
        end if
        
    end subroutine exit_failure_clean
    
    subroutine exit_invalid_config_clean(message)
        !! Exits with invalid configuration code (4)
        character(len=*), intent(in), optional :: message
        
        if (present(message)) then
            call exit_with_code(EXIT_INVALID_CONFIG, message)
        else
            call exit_with_code(EXIT_INVALID_CONFIG)
        end if
        
    end subroutine exit_invalid_config_clean
    
    subroutine exit_no_coverage_data_clean(message)
        !! Exits with no coverage data code (3)
        character(len=*), intent(in), optional :: message
        
        if (present(message)) then
            call exit_with_code(EXIT_NO_COVERAGE_DATA, message)
        else
            call exit_with_code(EXIT_NO_COVERAGE_DATA)
        end if
        
    end subroutine exit_no_coverage_data_clean
    
    subroutine exit_threshold_not_met_clean(message)
        !! Exits with threshold not met code (2)
        character(len=*), intent(in), optional :: message
        
        if (present(message)) then
            call exit_with_code(EXIT_THRESHOLD_NOT_MET, message)
        else
            call exit_with_code(EXIT_THRESHOLD_NOT_MET)
        end if
        
    end subroutine exit_threshold_not_met_clean
    
    subroutine exit_file_access_error_clean(message)
        !! Exits with file access error code (5)
        character(len=*), intent(in), optional :: message
        
        if (present(message)) then
            call exit_with_code(EXIT_FILE_ACCESS_ERROR, message)
        else
            call exit_with_code(EXIT_FILE_ACCESS_ERROR)
        end if
        
    end subroutine exit_file_access_error_clean
    
    subroutine exit_memory_error_clean(message)
        !! Exits with memory error code (6)
        character(len=*), intent(in), optional :: message
        
        if (present(message)) then
            call exit_with_code(EXIT_MEMORY_ERROR, message)
        else
            call exit_with_code(EXIT_MEMORY_ERROR)
        end if
        
    end subroutine exit_memory_error_clean
    
    subroutine exit_validation_error_clean(message)
        !! Exits with validation error code (7)
        character(len=*), intent(in), optional :: message
        
        if (present(message)) then
            call exit_with_code(EXIT_VALIDATION_ERROR, message)
        else
            call exit_with_code(EXIT_VALIDATION_ERROR)
        end if
        
    end subroutine exit_validation_error_clean
    
end module system_exit_handler