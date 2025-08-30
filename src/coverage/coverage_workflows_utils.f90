module coverage_workflows_utils
    !! Coverage workflow utility functions
    !!
    !! Provides common utility functions for coverage workflows including
    !! file type detection, path normalization, and testing utilities.
    
    use string_utils, only: to_lower
    implicit none
    private
    
    public :: is_test_file
    public :: normalize_path
    
contains

    function is_test_file(filepath) result(is_test)
        !! Checks if file appears to be a test file
        character(len=*), intent(in) :: filepath
        logical :: is_test
        
        character(len=:), allocatable :: lower_path
        
        lower_path = to_lower(filepath)
        
        is_test = (index(lower_path, 'test') > 0) .or. &
                  (index(lower_path, 'spec') > 0) .or. &
                  (index(lower_path, 'check') > 0)
        
    end function is_test_file
    
    function normalize_path(filepath) result(normalized)
        !! Normalizes file path for consistent processing
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable :: normalized
        integer :: i
        
        ! Basic path normalization
        normalized = trim(filepath)
        
        ! Convert backslashes to forward slashes for consistency
        ! Simple string replacement - replace '\' with '/'
        do i = 1, len(normalized)
            if (normalized(i:i) == '\') then
                normalized(i:i) = '/'
            end if
        end do
        
    end function normalize_path

end module coverage_workflows_utils