module coverage_data_utils
    !! Coverage Data Utilities Module
    !!
    !! Centralized utilities for coverage_data_t allocation and initialization
    !! patterns to eliminate code duplication across the codebase.

    use coverage_types
    implicit none
    private

    public :: allocate_files_array
    public :: ensure_files_allocated
    public :: safely_deallocate_files
    public :: validate_files_allocation
    public :: allocate_files_with_error_handling

contains

    subroutine allocate_files_array(coverage_data, size)
        !! Safely allocate files array with specified size
        !! DEPRECATED: Use allocate_files_with_error_handling for new code
        type(coverage_data_t), intent(inout) :: coverage_data
        integer, intent(in) :: size
        
        logical :: success
        character(len=256) :: error_msg
        
        ! Use enhanced allocation with error handling
        call allocate_files_with_error_handling(coverage_data, size, success, error_msg)
        if (.not. success) then
            write(*, '(A)') "Warning: " // trim(error_msg)
        end if
    end subroutine allocate_files_array

    subroutine allocate_files_with_error_handling(coverage_data, size, success, error_msg)
        !! Enhanced file array allocation with comprehensive error handling
        type(coverage_data_t), intent(inout) :: coverage_data
        integer, intent(in) :: size
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_msg
        
        integer :: stat
        character(len=512) :: errmsg
        
        success = .false.
        error_msg = ""
        
        ! Input validation
        if (size < 0) then
            error_msg = "Invalid array size: negative size not allowed"
            return
        end if
        
        ! Safely deallocate existing array if allocated
        if (allocated(coverage_data%files)) then
            deallocate(coverage_data%files, stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                error_msg = "Failed to deallocate existing files array: " // trim(errmsg)
                return
            end if
        end if
        
        ! Allocate new array with proper error handling
        allocate(coverage_data%files(size), stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            error_msg = "Failed to allocate files array: " // trim(errmsg)
            coverage_data%total_files = 0
            return
        end if
        
        coverage_data%total_files = size
        success = .true.
    end subroutine allocate_files_with_error_handling

    subroutine ensure_files_allocated(coverage_data, minimum_size)
        !! Ensure files array is allocated with at least minimum_size
        type(coverage_data_t), intent(inout) :: coverage_data
        integer, intent(in), optional :: minimum_size
        integer :: required_size
        
        required_size = 0
        if (present(minimum_size)) required_size = minimum_size
        
        if (.not. allocated(coverage_data%files)) then
            allocate(coverage_data%files(required_size))
            coverage_data%total_files = required_size
        else if (size(coverage_data%files) < required_size) then
            ! Need to grow the array
            call allocate_files_array(coverage_data, required_size)
        end if
    end subroutine ensure_files_allocated

    subroutine safely_deallocate_files(coverage_data)
        !! Safely deallocate files array with enhanced error handling
        type(coverage_data_t), intent(inout) :: coverage_data
        
        integer :: stat
        character(len=512) :: errmsg
        
        if (allocated(coverage_data%files)) then
            deallocate(coverage_data%files, stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Warning: Failed to deallocate files array: " // trim(errmsg)
            else
                coverage_data%total_files = 0
            end if
        end if
    end subroutine safely_deallocate_files

    function validate_files_allocation(coverage_data) result(is_valid)
        !! Validate files array allocation state
        type(coverage_data_t), intent(in) :: coverage_data
        logical :: is_valid
        
        is_valid = .true.
        
        ! Check basic allocation consistency
        if (.not. allocated(coverage_data%files)) then
            if (coverage_data%total_files /= 0) then
                is_valid = .false.  ! Inconsistent state
            end if
        else
            if (size(coverage_data%files) /= coverage_data%total_files) then
                is_valid = .false.  ! Size mismatch
            end if
        end if
    end function validate_files_allocation

end module coverage_data_utils
