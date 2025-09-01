module safe_allocation
    !! Standardized safe allocation helpers (Issue #996)
    !! Provides consistent error handling and input validation
    use coverage_model_core, only: coverage_line_t
    implicit none
    private

    public :: safe_allocate_int_array
    public :: safe_deallocate_int_array
    public :: safe_allocate_char_array
    public :: safe_allocate_lines_array

contains

    subroutine safe_allocate_int_array(arr, n, success)
        integer, allocatable, intent(out) :: arr(:)
        integer, intent(in) :: n
        logical, intent(out) :: success
        integer :: stat
        character(len=512) :: errmsg

        success = .false.
        if (n < 0) then
            write(*,'(A)') 'Error: Invalid array size for integer allocation'
            return
        end if
        allocate(arr(n), stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            write(*,'(A)') 'Error: Failed to allocate integer array: ' // trim(errmsg)
            return
        end if
        success = .true.
    end subroutine safe_allocate_int_array

    subroutine safe_deallocate_int_array(arr, success)
        integer, allocatable, intent(inout) :: arr(:)
        logical, intent(out) :: success
        integer :: stat
        success = .false.
        if (.not. allocated(arr)) then
            success = .true.
            return
        end if
        deallocate(arr, stat=stat)
        if (stat /= 0) return
        success = .true.
    end subroutine safe_deallocate_int_array

    subroutine safe_allocate_char_array(arr, n, len_item, success)
        character(len=:), allocatable, intent(out) :: arr(:)
        integer, intent(in) :: n
        integer, intent(in) :: len_item
        logical, intent(out) :: success
        integer :: stat
        character(len=512) :: errmsg

        success = .false.
        if (n < 0 .or. len_item < 0) then
            write(*,'(A)') 'Error: Invalid size for character allocation'
            return
        end if
        allocate(character(len=len_item) :: arr(n), stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            write(*,'(A)') 'Error: Failed to allocate character array: ' // trim(errmsg)
            return
        end if
        success = .true.
    end subroutine safe_allocate_char_array

    subroutine safe_allocate_lines_array(lines, size, success)
        !! Safely allocate coverage_line_t arrays with consistent handling
        type(coverage_line_t), allocatable, intent(out) :: lines(:)
        integer, intent(in) :: size
        logical, intent(out) :: success
        integer :: stat
        character(len=512) :: errmsg

        success = .false.
        if (size < 0) then
            write(*,'(A)') 'Error: Invalid array size for lines allocation'
            return
        end if
        allocate(lines(size), stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            write(*,'(A)') 'Error: Failed to allocate lines array: ' // trim(errmsg)
            return
        end if
        success = .true.
    end subroutine safe_allocate_lines_array

end module safe_allocation

