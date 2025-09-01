module memory_management
    !! Core Memory Management Infrastructure
    !!
    !! Provides centralized, fraud-proof memory allocation/deallocation utilities
    !! to eliminate the systematic memory leaks found across 10+ core modules.
    !! Addresses Issue #967: Systematic memory management infrastructure.
    !!
    !! Key Features:
    !! - Balanced allocation/deallocation tracking
    !! - Comprehensive error handling with stat= and errmsg=
    !! - Memory leak detection and prevention
    !! - Consistent allocation patterns across codebase
    !! - Debug-mode allocation tracking

    use iso_fortran_env, only: error_unit
    implicit none
    private

    ! Public interface for memory management
    public :: safe_allocate_character_array
    public :: safe_allocate_integer_array
    public :: safe_deallocate_character_array
    public :: safe_deallocate_integer_array
    public :: memory_status_t
    public :: get_memory_status
    public :: validate_memory_balance
    public :: memory_allocation_stats_t

    ! Memory allocation statistics tracking
    type :: memory_allocation_stats_t
        integer :: total_allocations = 0
        integer :: total_deallocations = 0
        integer :: active_allocations = 0
        integer :: failed_allocations = 0
        integer :: failed_deallocations = 0
        logical :: tracking_enabled = .false.
    end type memory_allocation_stats_t

    ! Memory status result type
    type :: memory_status_t
        logical :: is_balanced
        character(len=256) :: status_message
        integer :: unbalanced_allocations
    end type memory_status_t

    ! Module-level allocation tracking
    type(memory_allocation_stats_t), save :: global_stats

contains

    subroutine safe_allocate_character_array(array, size, success, error_msg)
        !! Safely allocate character array with comprehensive error handling
        character(len=:), allocatable, intent(out) :: array(:)
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

        ! Perform safe allocation
        if (size == 0) then
            allocate(character(len=1) :: array(0), stat=stat, errmsg=errmsg)
        else
            allocate(character(len=256) :: array(size), stat=stat, errmsg=errmsg)
        end if

        ! Handle allocation result
        if (stat /= 0) then
            error_msg = "Memory allocation failed: " // trim(errmsg)
            if (global_stats%tracking_enabled) then
                global_stats%failed_allocations = global_stats%failed_allocations + 1
            end if
            return
        end if

        ! Update allocation tracking
        if (global_stats%tracking_enabled) then
            global_stats%total_allocations = global_stats%total_allocations + 1
            global_stats%active_allocations = global_stats%active_allocations + 1
        end if

        success = .true.
    end subroutine safe_allocate_character_array

    subroutine safe_allocate_integer_array(array, size, success, error_msg)
        !! Safely allocate integer array with comprehensive error handling
        integer, allocatable, intent(out) :: array(:)
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

        ! Perform safe allocation
        allocate(array(size), stat=stat, errmsg=errmsg)

        ! Handle allocation result
        if (stat /= 0) then
            error_msg = "Memory allocation failed: " // trim(errmsg)
            if (global_stats%tracking_enabled) then
                global_stats%failed_allocations = global_stats%failed_allocations + 1
            end if
            return
        end if

        ! Update allocation tracking
        if (global_stats%tracking_enabled) then
            global_stats%total_allocations = global_stats%total_allocations + 1
            global_stats%active_allocations = global_stats%active_allocations + 1
        end if

        success = .true.
    end subroutine safe_allocate_integer_array

    subroutine safe_deallocate_character_array(array, success, error_msg)
        !! Safely deallocate character array with error handling
        character(len=:), allocatable, intent(inout) :: array(:)
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_msg
        
        integer :: stat
        character(len=512) :: errmsg

        success = .false.
        error_msg = ""

        ! Check if array is allocated
        if (.not. allocated(array)) then
            success = .true.  ! Nothing to deallocate
            return
        end if

        ! Perform safe deallocation
        deallocate(array, stat=stat, errmsg=errmsg)

        ! Handle deallocation result
        if (stat /= 0) then
            error_msg = "Memory deallocation failed: " // trim(errmsg)
            if (global_stats%tracking_enabled) then
                global_stats%failed_deallocations = global_stats%failed_deallocations + 1
            end if
            return
        end if

        ! Update deallocation tracking
        if (global_stats%tracking_enabled) then
            global_stats%total_deallocations = global_stats%total_deallocations + 1
            global_stats%active_allocations = max(0, global_stats%active_allocations - 1)
        end if

        success = .true.
    end subroutine safe_deallocate_character_array

    subroutine safe_deallocate_integer_array(array, success, error_msg)
        !! Safely deallocate integer array with error handling
        integer, allocatable, intent(inout) :: array(:)
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_msg
        
        integer :: stat
        character(len=512) :: errmsg

        success = .false.
        error_msg = ""

        ! Check if array is allocated
        if (.not. allocated(array)) then
            success = .true.  ! Nothing to deallocate
            return
        end if

        ! Perform safe deallocation
        deallocate(array, stat=stat, errmsg=errmsg)

        ! Handle deallocation result
        if (stat /= 0) then
            error_msg = "Memory deallocation failed: " // trim(errmsg)
            if (global_stats%tracking_enabled) then
                global_stats%failed_deallocations = global_stats%failed_deallocations + 1
            end if
            return
        end if

        ! Update deallocation tracking
        if (global_stats%tracking_enabled) then
            global_stats%total_deallocations = global_stats%total_deallocations + 1
            global_stats%active_allocations = max(0, global_stats%active_allocations - 1)
        end if

        success = .true.
    end subroutine safe_deallocate_integer_array

    function get_memory_status() result(status)
        !! Get current memory allocation status and balance
        type(memory_status_t) :: status

        status%unbalanced_allocations = global_stats%active_allocations
        status%is_balanced = (global_stats%active_allocations == 0)

        if (status%is_balanced) then
            write(status%status_message, '(A,I0,A,I0,A)') &
                "Memory balanced: ", global_stats%total_allocations, &
                " allocs / ", global_stats%total_deallocations, " deallocs"
        else
            write(status%status_message, '(A,I0,A)') &
                "Memory imbalanced: ", status%unbalanced_allocations, " active allocations"
        end if
    end function get_memory_status

    function validate_memory_balance() result(is_valid)
        !! Validate that all allocations have been properly deallocated
        logical :: is_valid
        
        is_valid = (global_stats%active_allocations == 0)
        
        if (.not. is_valid) then
            write(error_unit, '(A,I0,A)') &
                "WARNING: Memory imbalance detected: ", &
                global_stats%active_allocations, " active allocations remain"
            write(error_unit, '(A,I0,A,I0)') &
                "Total allocations: ", global_stats%total_allocations, &
                ", Total deallocations: ", global_stats%total_deallocations
            if (global_stats%failed_allocations > 0) then
                write(error_unit, '(A,I0)') &
                    "Failed allocations: ", global_stats%failed_allocations
            end if
            if (global_stats%failed_deallocations > 0) then
                write(error_unit, '(A,I0)') &
                    "Failed deallocations: ", global_stats%failed_deallocations
            end if
        end if
    end function validate_memory_balance

    ! Module initialization - enable tracking in debug mode
    subroutine enable_memory_tracking()
        !! Enable memory allocation tracking
        global_stats%tracking_enabled = .true.
        global_stats%total_allocations = 0
        global_stats%total_deallocations = 0  
        global_stats%active_allocations = 0
        global_stats%failed_allocations = 0
        global_stats%failed_deallocations = 0
    end subroutine enable_memory_tracking

    subroutine disable_memory_tracking()
        !! Disable memory allocation tracking
        global_stats%tracking_enabled = .false.
    end subroutine disable_memory_tracking

    ! Module finalization
    subroutine finalize_memory_management()
        !! Final memory balance check and reporting
        logical :: is_balanced
        
        if (.not. global_stats%tracking_enabled) return
        
        is_balanced = validate_memory_balance()
        if (is_balanced) then
            write(*, '(A)') "✓ Memory management: All allocations properly balanced"
        end if
    end subroutine finalize_memory_management

end module memory_management