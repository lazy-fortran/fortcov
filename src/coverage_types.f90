module coverage_types
    !! Core Coverage Data Types (Refactored)
    !!
    !! Aggregates and re-exports coverage types from specialized modules.
    !! This module maintains backward compatibility by re-exporting types
    !! from focused modules that maintain SRP and QADS size limits.
    
    ! Re-export all types from specialized modules
    use coverage_location_types, only: &
        source_location_t, &
        coverage_line_t, &
        coverage_branch_t, &
        line_coverage_t, &
        file_coverage_t
        
    use coverage_function_types, only: &
        coverage_function_t
        
    use coverage_file_types, only: &
        coverage_file_t
        
    use coverage_data_types, only: &
        coverage_data_t
        
    ! Import diff types but rename to avoid conflicts
    use coverage_diff_types, only: &
        line_diff_t_imported => line_diff_t, &
        file_diff_t_imported => file_diff_t, &
        coverage_diff_t_imported => coverage_diff_t
    
    implicit none
    private
    
    ! Re-export all types for backward compatibility
    public :: source_location_t
    public :: coverage_line_t
    public :: coverage_branch_t
    public :: coverage_function_t
    public :: coverage_file_t
    public :: coverage_data_t
    public :: coverage_diff_t
    public :: line_diff_t
    public :: file_diff_t
    public :: line_coverage_t
    public :: file_coverage_t
    
    ! Create compatibility types for diff types to match original interface
    type :: line_diff_t
        integer :: line_number = 0
        character(len=:), allocatable :: filename
        integer :: old_count = 0
        integer :: new_count = 0
        integer :: execution_count_delta = 0
        integer :: diff_type = 0
        character(len=1) :: status = ' '
        logical :: is_newly_covered = .false.
        logical :: is_newly_uncovered = .false.
    contains
        procedure :: init => line_diff_init
    end type line_diff_t
    
    type :: file_diff_t
        character(len=:), allocatable :: filename
        real :: old_coverage = 0.0
        real :: new_coverage = 0.0
        real :: coverage_change = 0.0
        real :: baseline_coverage_percentage = 0.0
        real :: current_coverage_percentage = 0.0
        real :: coverage_percentage_delta = 0.0
        real :: statistical_confidence = 0.0
        integer :: overall_significance_classification = 0
        type(line_diff_t), allocatable :: lines(:)
        type(line_diff_t), allocatable :: line_diffs(:)
    contains
        procedure :: init => file_diff_init
        procedure :: apply_threshold_analysis => file_diff_apply_threshold_analysis
    end type file_diff_t
    
    type :: coverage_diff_t
        real :: baseline_coverage = 0.0
        real :: current_coverage = 0.0
        real :: coverage_change = 0.0
        real :: threshold = 0.0
        type(file_diff_t), allocatable :: file_diffs(:)
        integer :: added_lines = 0
        integer :: removed_lines = 0
        integer :: modified_lines = 0
    contains
        procedure :: init => coverage_diff_init
        procedure :: filter_by_threshold => coverage_diff_filter_by_threshold
    end type coverage_diff_t
    
    ! Constructor interfaces
    interface line_diff_t
        module procedure :: line_diff_constructor
    end interface line_diff_t
    
contains

    function line_diff_constructor(baseline_line, current_line, diff_type) result(this)
        type(coverage_line_t), intent(in) :: baseline_line
        type(coverage_line_t), intent(in) :: current_line
        integer, intent(in) :: diff_type
        type(line_diff_t) :: this
        
        call this%init(baseline_line, current_line, diff_type)
        
    end function line_diff_constructor
    
    subroutine line_diff_init(this, baseline_line, current_line, diff_type)
        class(line_diff_t), intent(inout) :: this
        type(coverage_line_t), intent(in) :: baseline_line
        type(coverage_line_t), intent(in) :: current_line
        integer, intent(in) :: diff_type
        
        this%line_number = current_line%line_number
        this%filename = current_line%filename
        this%old_count = baseline_line%execution_count
        this%new_count = current_line%execution_count
        this%execution_count_delta = current_line%execution_count - baseline_line%execution_count
        this%diff_type = diff_type
        this%is_newly_covered = (baseline_line%execution_count == 0 .and. current_line%execution_count > 0)
        this%is_newly_uncovered = (baseline_line%execution_count > 0 .and. current_line%execution_count == 0)
        
        select case (diff_type)
        case (0); this%status = '='
        case (1); this%status = '+'
        case (2); this%status = '-'
        case (3); this%status = '~'
        case default; this%status = '?'
        end select
        
    end subroutine line_diff_init
    
    subroutine file_diff_init(this, filename, lines)
        class(file_diff_t), intent(out) :: this
        character(len=*), intent(in) :: filename
        type(line_diff_t), intent(in), optional :: lines(:)
        
        this%filename = filename
        if (present(lines)) then
            allocate(this%lines, source=lines)
            allocate(this%line_diffs, source=lines)
        end if
        
    end subroutine file_diff_init
    
    subroutine file_diff_apply_threshold_analysis(this, thresholds)
        class(file_diff_t), intent(inout) :: this
        class(*), intent(in) :: thresholds
        
        real :: sample_size_factor, magnitude_factor
        integer :: line_count
        
        if (allocated(this%lines)) then
            line_count = size(this%lines)
        else
            line_count = 0
        end if
        
        if (line_count > 0) then
            sample_size_factor = min(1.0, real(line_count) / 50.0)
        else
            sample_size_factor = 0.0
        end if
        
        magnitude_factor = min(1.0, abs(this%coverage_percentage_delta) / 20.0)
        this%statistical_confidence = sqrt(sample_size_factor * magnitude_factor)
        
    end subroutine file_diff_apply_threshold_analysis
    
    subroutine coverage_diff_init(this, file_diffs, threshold)
        class(coverage_diff_t), intent(out) :: this
        type(file_diff_t), intent(in) :: file_diffs(:)
        real, intent(in), optional :: threshold
        
        allocate(this%file_diffs, source=file_diffs)
        if (present(threshold)) this%threshold = threshold
        
    end subroutine coverage_diff_init
    
    subroutine coverage_diff_filter_by_threshold(this)
        class(coverage_diff_t), intent(inout) :: this
        
        type(file_diff_t), allocatable :: filtered(:)
        integer :: i, count
        
        if (.not. allocated(this%file_diffs)) return
        
        count = 0
        do i = 1, size(this%file_diffs)
            if (abs(this%file_diffs(i)%coverage_percentage_delta) >= this%threshold) then
                count = count + 1
            end if
        end do
        
        if (count > 0) then
            allocate(filtered(count))
            count = 0
            do i = 1, size(this%file_diffs)
                if (abs(this%file_diffs(i)%coverage_percentage_delta) >= this%threshold) then
                    count = count + 1
                    filtered(count) = this%file_diffs(i)
                end if
            end do
            call move_alloc(filtered, this%file_diffs)
        else
            if (allocated(this%file_diffs)) deallocate(this%file_diffs)
            allocate(this%file_diffs(0))
        end if
        
    end subroutine coverage_diff_filter_by_threshold

end module coverage_types