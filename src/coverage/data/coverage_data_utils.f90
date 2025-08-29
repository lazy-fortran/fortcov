module coverage_data_utils
    !! Coverage Data Utilities Module
    !!
    !! Centralized utilities for coverage_data_t allocation and initialization
    !! patterns to eliminate code duplication across the codebase. This addresses
    !! the 15+ instances of duplicated coverage_data%files allocation patterns.
    !!
    !! Part of EPIC 2: Architectural debt consolidation to achieve:
    !! - <130 files (from 172)
    !! - <22,000 lines (from 27,311)
    !! - Eliminate duplicated allocation patterns

    use coverage_types
    implicit none
    private

    public :: allocate_files_array
    public :: ensure_files_allocated
    public :: safely_deallocate_files
    public :: validate_files_allocation

contains

    subroutine allocate_files_array(coverage_data, size)
        !! Safely allocate files array with specified size
        type(coverage_data_t), intent(inout) :: coverage_data
        integer, intent(in) :: size
        
        ! Deallocate if already allocated to prevent memory leaks
        if (allocated(coverage_data%files)) then
            deallocate(coverage_data%files)
        end if
        
        allocate(coverage_data%files(size))
        coverage_data%total_files = size
    end subroutine allocate_files_array

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
        !! Safely deallocate files array if allocated
        type(coverage_data_t), intent(inout) :: coverage_data
        
        if (allocated(coverage_data%files)) then
            deallocate(coverage_data%files)
            coverage_data%total_files = 0
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