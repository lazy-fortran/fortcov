module coverage_threshold_core
    !! Coverage Change Threshold Analysis Module
    !! 
    !! Provides types and methods for analyzing coverage changes with
    !! configurable thresholds for classification and filtering.
    implicit none
    private
    
    ! Public types
    public :: diff_thresholds_t
    
    ! Classification constants
    integer, parameter, public :: UNCHANGED_COVERAGE = 0
    integer, parameter, public :: MINOR_IMPROVEMENT = 1
    integer, parameter, public :: MAJOR_IMPROVEMENT = 2
    integer, parameter, public :: CRITICAL_IMPROVEMENT = 3
    integer, parameter, public :: MINOR_DEGRADATION = -1
    integer, parameter, public :: MAJOR_DEGRADATION = -2
    integer, parameter, public :: CRITICAL_DEGRADATION = -3
    integer, parameter, public :: NEW_COVERAGE = 10
    integer, parameter, public :: LOST_COVERAGE = -10
    
    ! Threshold configuration type
    type :: diff_thresholds_t
        real :: critical_threshold = 5.0    ! >= 5% change
        real :: major_threshold = 2.0       ! >= 2% change
        real :: minor_threshold = 0.5       ! >= 0.5% change
        real :: significance_threshold = 0.1 ! >= 0.1% change (minimum)
    contains
        procedure :: init => thresholds_init
        procedure :: classify_change => thresholds_classify_change
    end type diff_thresholds_t
    
contains
    
    ! Initialize thresholds with custom or default values
    subroutine thresholds_init(this, critical, major, minor, significance)
        class(diff_thresholds_t), intent(out) :: this
        real, intent(in), optional :: critical, major, minor, significance
        
        if (present(critical)) this%critical_threshold = critical
        if (present(major)) this%major_threshold = major
        if (present(minor)) this%minor_threshold = minor
        if (present(significance)) this%significance_threshold = significance
        
    end subroutine thresholds_init
    
    ! Classify a coverage change based on thresholds
    function thresholds_classify_change(this, baseline_percentage, current_percentage) &
             result(classification)
        class(diff_thresholds_t), intent(in) :: this
        real, intent(in) :: baseline_percentage, current_percentage
        integer :: classification
        
        real :: delta
        
        delta = current_percentage - baseline_percentage
        
        ! Handle special cases first
        if (baseline_percentage == 0.0 .and. current_percentage > 0.0) then
            classification = NEW_COVERAGE
            return
        else if (baseline_percentage > 0.0 .and. current_percentage == 0.0) then
            classification = LOST_COVERAGE
            return
        end if
        
        ! Check if change is below significance threshold
        if (abs(delta) < this%significance_threshold) then
            classification = UNCHANGED_COVERAGE
            return
        end if
        
        ! Classify based on magnitude and direction
        if (delta > 0.0) then
            ! Improvement
            if (abs(delta) >= this%critical_threshold) then
                classification = CRITICAL_IMPROVEMENT
            else if (abs(delta) >= this%major_threshold) then
                classification = MAJOR_IMPROVEMENT
            else if (abs(delta) >= this%minor_threshold) then
                classification = MINOR_IMPROVEMENT
            else
                classification = UNCHANGED_COVERAGE
            end if
        else
            ! Degradation
            if (abs(delta) >= this%critical_threshold) then
                classification = CRITICAL_DEGRADATION
            else if (abs(delta) >= this%major_threshold) then
                classification = MAJOR_DEGRADATION
            else if (abs(delta) >= this%minor_threshold) then
                classification = MINOR_DEGRADATION
            else
                classification = UNCHANGED_COVERAGE
            end if
        end if
        
    end function thresholds_classify_change
    
end module coverage_threshold_core