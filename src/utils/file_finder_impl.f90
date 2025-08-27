module file_finder_impl
    use error_handling_core
    use file_ops_secure, only: safe_find_files
    implicit none
    private
    
    ! Public procedures
    public :: find_files
    public :: find_files_with_glob
    
contains

    function find_files(pattern) result(files)
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: files(:)
        type(error_context_t) :: error_ctx
        
        ! Use secure command executor for safe file finding
        call safe_find_files(pattern, files, error_ctx)
        
        ! If secure find fails, return empty array
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (allocated(files)) deallocate(files)
            allocate(character(len=256) :: files(0))
        end if
    end function find_files

    function find_files_with_glob(directory, pattern) result(files)
        !! Find files in specific directory matching glob pattern
        character(len=*), intent(in) :: directory
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: files(:)
        
        character(len=256) :: full_pattern
        type(error_context_t) :: error_ctx
        
        ! Construct full search pattern
        if (trim(directory) == ".") then
            full_pattern = trim(pattern)
        else
            full_pattern = trim(directory) // "/" // trim(pattern)
        end if
        
        ! Use secure command executor for safe file finding
        call safe_find_files(full_pattern, files, error_ctx)
        
        ! If secure find fails, return empty array
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (allocated(files)) deallocate(files)
            allocate(character(len=256) :: files(0))
        end if
    end function find_files_with_glob

end module file_finder_impl