module coverage_diff_types
    !! Coverage Diff Types Module
    !!
    !! Contains types for coverage comparison and analysis:
    !! - line_diff_t: Individual line coverage differences
    !! - file_diff_t: File-level coverage differences
    !! - coverage_diff_t: Overall coverage difference container
    
    use constants_core
    use coverage_basic_types
    implicit none
    private
    
    ! ============================================================================
    ! Constants and Parameters
    ! ============================================================================
    
    ! Diff status enumeration
    integer, parameter, public :: DIFF_UNCHANGED = 0
    integer, parameter, public :: DIFF_IMPROVED = 1
    integer, parameter, public :: DIFF_DEGRADED = -1
    integer, parameter, public :: DIFF_NEW_LINE = 2
    integer, parameter, public :: DIFF_REMOVED_LINE = -2
    
    ! ============================================================================
    ! Coverage Diff Types
    ! ============================================================================
    
    ! Line diff type
    type, public :: line_diff_t
        integer :: line_number = 0
        character(len=:), allocatable :: filename
        integer :: old_count = 0
        integer :: new_count = 0
        integer :: execution_count_delta = 0
        integer :: diff_type = 0
        character(len=1) :: status = ' '
        logical :: is_newly_covered = .false.
        logical :: is_newly_uncovered = .false.
        ! Additional fields from diff_types
        integer :: baseline_count = 0
        integer :: current_count = 0
        real :: coverage_change = 0.0
        logical :: is_new = .false.
        logical :: is_removed = .false.
    contains
        procedure :: init => line_diff_init
    end type line_diff_t
    
    ! File diff type
    type, public :: file_diff_t
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
        ! Additional fields from diff_types
        real :: baseline_coverage = 0.0
        real :: current_coverage = 0.0
        integer :: improved_lines = 0
        integer :: degraded_lines = 0
        integer :: new_lines = 0
        integer :: removed_lines = 0
    contains
        procedure :: init => file_diff_init
        procedure :: apply_threshold_analysis => file_diff_apply_threshold_analysis
    end type file_diff_t
    
    ! Coverage diff container type
    type, public :: coverage_diff_t
        real :: baseline_coverage = 0.0
        real :: current_coverage = 0.0
        real :: coverage_change = 0.0
        real :: threshold = 0.0
        type(file_diff_t), allocatable :: file_diffs(:)
        integer :: added_lines = 0
        integer :: removed_lines = 0
        integer :: modified_lines = 0
        ! Additional fields from diff_types
        real :: overall_baseline_coverage = 0.0
        real :: overall_current_coverage = 0.0
        real :: overall_coverage_change = 0.0
        logical :: meets_threshold = .true.
        integer :: total_improved_lines = 0
        integer :: total_degraded_lines = 0
        integer :: total_new_lines = 0
        integer :: total_removed_lines = 0
    contains
        procedure :: init => coverage_diff_init
        procedure :: filter_by_threshold => coverage_diff_filter_by_threshold
    end type coverage_diff_t
    
    ! ============================================================================
    ! Constructor Interfaces
    ! ============================================================================
    
    ! Constructor interfaces
    interface line_diff_t
        module procedure :: line_diff_constructor
    end interface line_diff_t
    
    ! Public constructors
    public :: line_diff_constructor
    
contains
    
    ! ============================================================================
    ! Line Diff Implementation
    ! ============================================================================
    
    function line_diff_constructor(baseline_line, current_line, diff_type) result(this)
        type(coverage_line_t), intent(in) :: baseline_line
        type(coverage_line_t), intent(in) :: current_line
        integer, intent(in) :: diff_type
        type(line_diff_t) :: this
        
        call this%init(baseline_line, current_line, diff_type)
    end function line_diff_constructor
    
    subroutine line_diff_init(this, baseline_line, current_line, diff_type)
        class(line_diff_t), intent(out) :: this
        type(coverage_line_t), intent(in) :: baseline_line
        type(coverage_line_t), intent(in) :: current_line
        integer, intent(in) :: diff_type
        
        this%line_number = current_line%location%line_number
        this%filename = current_line%location%filename
        this%baseline_count = baseline_line%execution_count
        this%current_count = current_line%execution_count
        this%old_count = baseline_line%execution_count
        this%new_count = current_line%execution_count
        this%diff_type = diff_type
        this%execution_count_delta = current_line%execution_count - baseline_line%execution_count
        
        ! Calculate coverage change
        if (baseline_line%is_executable .and. current_line%is_executable) then
            if (baseline_line%execution_count == 0 .and. current_line%execution_count > 0) then
                this%coverage_change = 100.0  ! Went from uncovered to covered
                this%diff_type = DIFF_IMPROVED
                this%is_newly_covered = .true.
            else if (baseline_line%execution_count > 0 .and. current_line%execution_count == 0) then
                this%coverage_change = -100.0  ! Went from covered to uncovered
                this%diff_type = DIFF_DEGRADED
                this%is_newly_uncovered = .true.
            else
                this%coverage_change = 0.0
                this%diff_type = DIFF_UNCHANGED
            end if
        else
            this%coverage_change = 0.0
            this%diff_type = diff_type
        end if
        
        this%is_new = (diff_type == DIFF_NEW_LINE)
        this%is_removed = (diff_type == DIFF_REMOVED_LINE)
        
        ! Set status character for compatibility
        select case (this%diff_type)
        case (DIFF_IMPROVED)
            this%status = '+'
        case (DIFF_DEGRADED)
            this%status = '-'
        case (DIFF_NEW_LINE)
            this%status = 'N'
        case (DIFF_REMOVED_LINE)
            this%status = 'R'
        case default
            this%status = ' '
        end select
    end subroutine line_diff_init
    
    ! ============================================================================
    ! File Diff Implementation
    ! ============================================================================
    
    subroutine file_diff_init(this, filename, lines)
        class(file_diff_t), intent(out) :: this
        character(len=*), intent(in) :: filename
        type(line_diff_t), intent(in), optional :: lines(:)
        
        this%filename = filename
        if (present(lines)) then
            allocate(this%lines(size(lines)))
            this%lines = lines
            allocate(this%line_diffs(size(lines)))
            this%line_diffs = lines
        else
            allocate(this%lines(0))
            allocate(this%line_diffs(0))
        end if
        
        this%baseline_coverage = 0.0
        this%current_coverage = 0.0
        this%old_coverage = 0.0
        this%new_coverage = 0.0
        this%coverage_change = 0.0
        this%baseline_coverage_percentage = 0.0
        this%current_coverage_percentage = 0.0
        this%coverage_percentage_delta = 0.0
        this%statistical_confidence = 0.0
        this%overall_significance_classification = 0
        this%improved_lines = 0
        this%degraded_lines = 0
        this%new_lines = 0
        this%removed_lines = 0
    end subroutine file_diff_init
    
    subroutine file_diff_apply_threshold_analysis(this, thresholds)
        class(file_diff_t), intent(inout) :: this
        real, intent(in) :: thresholds(:)
        
        integer :: i
        
        ! Count lines by diff type
        do i = 1, size(this%lines)
            select case (this%lines(i)%diff_type)
            case (DIFF_IMPROVED)
                this%improved_lines = this%improved_lines + 1
            case (DIFF_DEGRADED)
                this%degraded_lines = this%degraded_lines + 1
            case (DIFF_NEW_LINE)
                this%new_lines = this%new_lines + 1
            case (DIFF_REMOVED_LINE)
                this%removed_lines = this%removed_lines + 1
            end select
        end do
        
        ! Calculate coverage change
        this%coverage_change = this%current_coverage - this%baseline_coverage
        this%coverage_percentage_delta = this%current_coverage_percentage - this%baseline_coverage_percentage
    end subroutine file_diff_apply_threshold_analysis
    
    ! ============================================================================
    ! Coverage Diff Implementation
    ! ============================================================================
    
    subroutine coverage_diff_init(this, file_diffs, threshold)
        class(coverage_diff_t), intent(out) :: this
        type(file_diff_t), intent(in) :: file_diffs(:)
        real, intent(in) :: threshold
        
        this%file_diffs = file_diffs
        this%threshold = threshold
        this%baseline_coverage = 0.0
        this%current_coverage = 0.0
        this%coverage_change = 0.0
        this%overall_baseline_coverage = 0.0
        this%overall_current_coverage = 0.0
        this%overall_coverage_change = 0.0
        this%meets_threshold = .true.
        this%added_lines = 0
        this%removed_lines = 0
        this%modified_lines = 0
        this%total_improved_lines = 0
        this%total_degraded_lines = 0
        this%total_new_lines = 0
        this%total_removed_lines = 0
        
        call this%filter_by_threshold()
    end subroutine coverage_diff_init
    
    subroutine coverage_diff_filter_by_threshold(this)
        class(coverage_diff_t), intent(inout) :: this
        
        integer :: i
        
        ! Calculate totals
        this%total_improved_lines = 0
        this%total_degraded_lines = 0
        this%total_new_lines = 0
        this%total_removed_lines = 0
        
        do i = 1, size(this%file_diffs)
            this%total_improved_lines = this%total_improved_lines + this%file_diffs(i)%improved_lines
            this%total_degraded_lines = this%total_degraded_lines + this%file_diffs(i)%degraded_lines
            this%total_new_lines = this%total_new_lines + this%file_diffs(i)%new_lines
            this%total_removed_lines = this%total_removed_lines + this%file_diffs(i)%removed_lines
        end do
        
        ! Aggregate for overall diff
        this%added_lines = this%total_new_lines
        this%removed_lines = this%total_removed_lines
        this%modified_lines = this%total_improved_lines + this%total_degraded_lines
        
        ! Check threshold
        this%overall_coverage_change = this%overall_current_coverage - this%overall_baseline_coverage
        this%coverage_change = this%current_coverage - this%baseline_coverage
        this%meets_threshold = (this%overall_current_coverage >= this%threshold)
    end subroutine coverage_diff_filter_by_threshold
    
end module coverage_diff_types