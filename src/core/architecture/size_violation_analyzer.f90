module size_violation_analyzer
    !! Size Violation Analysis and Classification
    !! 
    !! Analyzes scan results and classifies violations by severity.
    !! Single responsibility: violation analysis and classification.
    use size_scanner_core, only: scan_result_t, FILE_SIZE_TARGET, FILE_SIZE_HARD_LIMIT, &
                                 DIRECTORY_SOFT_LIMIT, DIRECTORY_HARD_LIMIT
    implicit none
    private
    
    ! Public interface for violation analysis
    public :: analyze_file_violations
    public :: analyze_directory_violations
    public :: check_imminent_violations_in_results
    public :: file_size_violation_t
    public :: directory_size_violation_t
    
    ! Warning thresholds (percentage of limit)
    real, parameter :: WARNING_THRESHOLD = 0.8  ! 80% of limit
    real, parameter :: CRITICAL_THRESHOLD = 0.95  ! 95% of limit
    
    ! File size violation type
    type :: file_size_violation_t
        character(len=:), allocatable :: filename
        integer :: current_lines
        integer :: target_limit
        integer :: hard_limit
        real :: target_percentage
        real :: hard_percentage
        character(len=:), allocatable :: severity_level  ! WARNING, CRITICAL, VIOLATION
        character(len=:), allocatable :: remediation_hint
    end type file_size_violation_t
    
    ! Directory size violation type  
    type :: directory_size_violation_t
        character(len=:), allocatable :: directory_path
        integer :: current_items
        integer :: soft_limit
        integer :: hard_limit
        real :: soft_percentage
        real :: hard_percentage
        character(len=:), allocatable :: severity_level
        character(len=:), allocatable :: remediation_hint
        character(len=:), allocatable :: largest_files(:)
    end type directory_size_violation_t

contains

    subroutine analyze_file_violations(scan_results, violations)
        !! Analyzes file scan results and identifies violations
        type(scan_result_t), intent(in) :: scan_results(:)
        type(file_size_violation_t), allocatable, intent(out) :: violations(:)
        
        type(file_size_violation_t) :: temp_violations(1000)
        integer :: violation_count, i
        
        violation_count = 0
        
        do i = 1, size(scan_results)
            if (scan_results(i)%is_valid) then
                call analyze_single_file(scan_results(i)%path, &
                                       scan_results(i)%size_metric, &
                                       temp_violations(violation_count+1))
                
                if (len_trim(temp_violations(violation_count+1)%severity_level) > 0) then
                    violation_count = violation_count + 1
                end if
            end if
        end do
        
        ! Allocate and copy violations
        if (violation_count > 0) then
            allocate(violations(violation_count))
            violations(1:violation_count) = temp_violations(1:violation_count)
        else
            allocate(violations(0))
        end if
        
    end subroutine analyze_file_violations
    
    subroutine analyze_directory_violations(scan_results, violations)
        !! Analyzes directory scan results and identifies violations
        type(scan_result_t), intent(in) :: scan_results(:)
        type(directory_size_violation_t), allocatable, intent(out) :: violations(:)
        
        type(directory_size_violation_t) :: temp_violations(200)
        integer :: violation_count, i
        
        violation_count = 0
        
        do i = 1, size(scan_results)
            if (scan_results(i)%is_valid) then
                call analyze_single_directory(scan_results(i)%path, &
                                            scan_results(i)%size_metric, &
                                            temp_violations(violation_count+1))
                
                if (len_trim(temp_violations(violation_count+1)%severity_level) > 0) then
                    violation_count = violation_count + 1
                end if
            end if
        end do
        
        ! Allocate and copy violations
        if (violation_count > 0) then
            allocate(violations(violation_count))
            violations(1:violation_count) = temp_violations(1:violation_count)
        else
            allocate(violations(0))
        end if
        
    end subroutine analyze_directory_violations
    
    subroutine check_imminent_violations_in_results(scan_results, imminent_files)
        !! Identifies files approaching size limits (80%+ of target)
        type(scan_result_t), intent(in) :: scan_results(:)
        character(len=:), allocatable, intent(out) :: imminent_files(:)
        
        character(len=256) :: temp_files(500)
        integer :: imminent_count, i
        type(file_size_violation_t) :: temp_violation
        
        imminent_count = 0
        
        do i = 1, size(scan_results)
            if (scan_results(i)%is_valid) then
                call analyze_single_file(scan_results(i)%path, &
                                       scan_results(i)%size_metric, &
                                       temp_violation)
                
                if (trim(temp_violation%severity_level) == "WARNING" .or. &
                    trim(temp_violation%severity_level) == "CRITICAL") then
                    imminent_count = imminent_count + 1
                    temp_files(imminent_count) = scan_results(i)%path
                end if
            end if
        end do
        
        ! Allocate and copy imminent files
        if (imminent_count > 0) then
            allocate(character(len=256) :: imminent_files(imminent_count))
            imminent_files(1:imminent_count) = temp_files(1:imminent_count)
        else
            allocate(character(len=256) :: imminent_files(0))
        end if
        
    end subroutine check_imminent_violations_in_results

    ! ================ INTERNAL ANALYSIS FUNCTIONS ================
    
    subroutine analyze_single_file(filename, line_count, violation)
        !! Analyzes individual file for size violations
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line_count
        type(file_size_violation_t), intent(out) :: violation
        
        real :: target_pct, hard_pct
        
        ! Calculate percentages
        target_pct = real(line_count) / real(FILE_SIZE_TARGET)
        hard_pct = real(line_count) / real(FILE_SIZE_HARD_LIMIT)
        
        ! Initialize violation record
        violation%filename = trim(filename)
        violation%current_lines = line_count
        violation%target_limit = FILE_SIZE_TARGET
        violation%hard_limit = FILE_SIZE_HARD_LIMIT
        violation%target_percentage = target_pct
        violation%hard_percentage = hard_pct
        
        ! Determine severity and remediation
        if (line_count >= FILE_SIZE_HARD_LIMIT) then
            violation%severity_level = "VIOLATION"
            violation%remediation_hint = "IMMEDIATE ACTION: Decompose into smaller modules"
        else if (hard_pct >= CRITICAL_THRESHOLD) then
            violation%severity_level = "CRITICAL"  
            violation%remediation_hint = "URGENT: Approaching hard limit - refactor now"
        else if (target_pct >= WARNING_THRESHOLD) then
            violation%severity_level = "WARNING"
            violation%remediation_hint = "Consider refactoring before reaching target limit"
        else
            violation%severity_level = ""  ! No violation
        end if
        
    end subroutine analyze_single_file
    
    subroutine analyze_single_directory(dir_path, item_count, violation)
        !! Analyzes individual directory for item count violations
        character(len=*), intent(in) :: dir_path
        integer, intent(in) :: item_count
        type(directory_size_violation_t), intent(out) :: violation
        
        real :: soft_pct, hard_pct
        
        ! Calculate percentages
        soft_pct = real(item_count) / real(DIRECTORY_SOFT_LIMIT)  
        hard_pct = real(item_count) / real(DIRECTORY_HARD_LIMIT)
        
        ! Initialize violation record
        violation%directory_path = trim(dir_path)
        violation%current_items = item_count
        violation%soft_limit = DIRECTORY_SOFT_LIMIT
        violation%hard_limit = DIRECTORY_HARD_LIMIT
        violation%soft_percentage = soft_pct
        violation%hard_percentage = hard_pct
        
        ! Determine severity
        if (item_count >= DIRECTORY_HARD_LIMIT) then
            violation%severity_level = "VIOLATION"
            violation%remediation_hint = "IMMEDIATE: Restructure directory organization"
        else if (item_count >= DIRECTORY_SOFT_LIMIT) then
            violation%severity_level = "WARNING"
            violation%remediation_hint = "Consider subdirectory organization"
        else
            violation%severity_level = ""  ! No violation
        end if
        
    end subroutine analyze_single_directory

end module size_violation_analyzer