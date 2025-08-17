module coverage_model
    implicit none
    private
    
    ! Public types
    public :: source_location_t
    public :: coverage_line_t
    public :: coverage_branch_t
    public :: coverage_function_t
    public :: coverage_file_t
    public :: coverage_data_t
    public :: coverage_diff_t
    public :: line_diff_t
    public :: file_diff_t
    
    ! Source location type
    type :: source_location_t
        character(len=:), allocatable :: filename
        integer :: line_number
        integer :: column_start = 0
        integer :: column_end = 0
    contains
        procedure :: init => source_location_init
    end type source_location_t
    
    ! Line coverage type
    type :: coverage_line_t
        type(source_location_t) :: location
        integer :: execution_count = 0
        logical :: is_executable = .false.
        integer :: line_number = 0
        character(len=:), allocatable :: filename
    contains
        procedure :: is_covered => line_is_covered
        procedure :: init => line_init
    end type coverage_line_t
    
    ! Branch coverage type
    type :: coverage_branch_t
        type(source_location_t) :: location
        integer :: taken_count = 0
        integer :: not_taken_count = 0
        integer :: branch_id = 0
        integer :: line_number = 0
        character(len=:), allocatable :: filename
    contains
        procedure :: is_partially_covered => branch_is_partially_covered
        procedure :: is_fully_covered => branch_is_fully_covered
        procedure :: init => branch_init
    end type coverage_branch_t
    
    ! Function coverage type
    type :: coverage_function_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: parent_module
        logical :: is_module_procedure = .false.
        integer :: execution_count = 0
        type(source_location_t) :: location
        integer :: line_number = 0
        character(len=:), allocatable :: filename
        type(coverage_line_t), allocatable :: lines(:)
        type(coverage_branch_t), allocatable :: branches(:)
    contains
        procedure :: init => function_init
    end type coverage_function_t
    
    ! File coverage type
    type :: coverage_file_t
        character(len=:), allocatable :: filename
        type(coverage_line_t), allocatable :: lines(:)
        type(coverage_function_t), allocatable :: functions(:)
    contains
        procedure :: get_line_coverage_percentage => &
                     file_get_line_coverage_percentage
        procedure :: get_executable_line_count => &
                     file_get_executable_line_count
        procedure :: get_covered_line_count => &
                     file_get_covered_line_count
        procedure :: init => file_init
    end type coverage_file_t
    
    ! Complete coverage dataset type
    type :: coverage_data_t
        type(coverage_file_t), allocatable :: files(:)
    contains
        procedure :: serialize => coverage_data_serialize
        procedure :: deserialize => coverage_data_deserialize
        procedure :: init => coverage_data_init
    end type coverage_data_t

    ! Line change types for diff analysis
    integer, parameter :: DIFF_UNCHANGED = 0
    integer, parameter :: DIFF_ADDED = 1
    integer, parameter :: DIFF_REMOVED = 2
    integer, parameter :: DIFF_CHANGED = 3

    public :: DIFF_UNCHANGED, DIFF_ADDED, DIFF_REMOVED, DIFF_CHANGED

    ! Enhanced threshold-based classification types
    integer, parameter :: CRITICAL_DEGRADATION = -3
    integer, parameter :: MAJOR_DEGRADATION = -2
    integer, parameter :: MINOR_DEGRADATION = -1
    integer, parameter :: UNCHANGED_COVERAGE = 0
    integer, parameter :: MINOR_IMPROVEMENT = 1
    integer, parameter :: MAJOR_IMPROVEMENT = 2
    integer, parameter :: CRITICAL_IMPROVEMENT = 3
    integer, parameter :: NEW_COVERAGE = 10
    integer, parameter :: LOST_COVERAGE = -10

    public :: CRITICAL_DEGRADATION, MAJOR_DEGRADATION, MINOR_DEGRADATION
    public :: UNCHANGED_COVERAGE, MINOR_IMPROVEMENT, MAJOR_IMPROVEMENT, &
              & CRITICAL_IMPROVEMENT
    public :: NEW_COVERAGE, LOST_COVERAGE

    ! Line difference type with enhanced threshold-based classification
    type :: line_diff_t
        integer :: line_number = 0
        type(coverage_line_t) :: baseline_line
        type(coverage_line_t) :: current_line
        integer :: diff_type = DIFF_UNCHANGED
        integer :: execution_count_delta = 0
        real :: coverage_percentage_delta = 0.0
        logical :: is_newly_covered = .false.
        logical :: is_newly_uncovered = .false.
        ! Enhanced threshold-based classification
        integer :: significance_classification = UNCHANGED_COVERAGE
        real :: statistical_confidence = 1.0
    contains
        procedure :: init => line_diff_init
        procedure :: calculate_delta => line_diff_calculate_delta
        procedure :: apply_threshold_classification => line_diff_apply_classification
    end type line_diff_t

    ! File difference type with enhanced threshold-based analysis
    type :: file_diff_t
        character(len=:), allocatable :: filename
        type(line_diff_t), allocatable :: line_diffs(:)
        integer :: added_lines = 0
        integer :: removed_lines = 0
        integer :: changed_lines = 0
        integer :: unchanged_lines = 0
        real :: baseline_coverage_percentage = 0.0
        real :: current_coverage_percentage = 0.0
        real :: coverage_percentage_delta = 0.0
        integer :: newly_covered_lines = 0
        integer :: newly_uncovered_lines = 0
        ! Enhanced threshold-based classification
        integer :: overall_significance_classification = UNCHANGED_COVERAGE
        real :: statistical_confidence = 1.0
        integer :: total_executable_lines = 0
        character(len=:), allocatable :: significance_description
    contains
        procedure :: init => file_diff_init
        procedure :: calculate_summary => file_diff_calculate_summary
        procedure :: apply_threshold_analysis => file_diff_apply_threshold_analysis
        procedure :: get_significance_description => &
                     & file_diff_get_significance_description
    end type file_diff_t

    ! Coverage difference type for complete diff analysis
    type :: coverage_diff_t
        type(file_diff_t), allocatable :: file_diffs(:)
        integer :: total_added_lines = 0
        integer :: total_removed_lines = 0
        integer :: total_changed_lines = 0
        integer :: total_unchanged_lines = 0
        real :: baseline_total_coverage = 0.0
        real :: current_total_coverage = 0.0
        real :: total_coverage_delta = 0.0
        integer :: total_newly_covered_lines = 0
        integer :: total_newly_uncovered_lines = 0
        logical :: include_unchanged = .false.
        real :: significance_threshold = 0.0
    contains
        procedure :: init => coverage_diff_init
        procedure :: calculate_totals => coverage_diff_calculate_totals
        procedure :: filter_by_threshold => coverage_diff_filter_by_threshold
    end type coverage_diff_t
    
    ! Enhanced threshold configuration for sophisticated diff analysis
    type, public :: diff_thresholds_t
        real :: critical_threshold = 5.0      ! ≥5% change (critical)
        real :: major_threshold = 2.0         ! ≥2% change (major)
        real :: minor_threshold = 0.5         ! ≥0.5% change (minor)
        real :: significance_threshold = 0.1  ! Minimum for reporting
    contains
        procedure :: init => diff_thresholds_init
        procedure :: classify_change => classify_coverage_change
    end type diff_thresholds_t
    
    ! Constructor interfaces
    interface coverage_line_t
        module procedure :: new_coverage_line
    end interface coverage_line_t
    
    interface coverage_branch_t
        module procedure :: new_coverage_branch
    end interface coverage_branch_t
    
    interface coverage_function_t
        module procedure :: new_coverage_function
    end interface coverage_function_t
    
    interface coverage_file_t
        module procedure :: new_coverage_file
    end interface coverage_file_t
    
    interface coverage_data_t
        module procedure :: new_coverage_data
    end interface coverage_data_t

    interface line_diff_t
        module procedure :: new_line_diff
    end interface line_diff_t

    interface file_diff_t
        module procedure :: new_file_diff
    end interface file_diff_t

    interface coverage_diff_t
        module procedure :: new_coverage_diff
    end interface coverage_diff_t
    
contains

    ! Source location initialization
    subroutine source_location_init(this, filename, line_number, &
                                   column_start, column_end)
        class(source_location_t), intent(inout) :: this
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line_number
        integer, intent(in), optional :: column_start, column_end
        
        this%filename = filename
        this%line_number = line_number
        if (present(column_start)) this%column_start = column_start
        if (present(column_end)) this%column_end = column_end
    end subroutine source_location_init

    ! Line coverage constructor
    function new_coverage_line(execution_count, line_number, filename, &
                              is_executable) result(line_cov)
        integer, intent(in) :: execution_count
        integer, intent(in) :: line_number
        character(len=*), intent(in) :: filename
        logical, intent(in) :: is_executable
        type(coverage_line_t) :: line_cov
        
        call line_cov%init(execution_count, line_number, filename, &
                          is_executable)
    end function new_coverage_line

    ! Line coverage initialization
    subroutine line_init(this, execution_count, line_number, filename, &
                        is_executable)
        class(coverage_line_t), intent(inout) :: this
        integer, intent(in) :: execution_count
        integer, intent(in) :: line_number
        character(len=*), intent(in) :: filename
        logical, intent(in) :: is_executable
        
        this%execution_count = execution_count
        this%line_number = line_number
        this%filename = filename
        this%is_executable = is_executable
        call this%location%init(filename, line_number)
    end subroutine line_init

    ! Check if line is covered
    function line_is_covered(this) result(covered)
        class(coverage_line_t), intent(in) :: this
        logical :: covered
        
        covered = this%execution_count > 0
    end function line_is_covered

    ! Branch coverage constructor
    function new_coverage_branch(taken_count, not_taken_count, branch_id, &
                                line_number, filename) result(branch_cov)
        integer, intent(in) :: taken_count
        integer, intent(in) :: not_taken_count
        integer, intent(in) :: branch_id
        integer, intent(in) :: line_number
        character(len=*), intent(in) :: filename
        type(coverage_branch_t) :: branch_cov
        
        call branch_cov%init(taken_count, not_taken_count, branch_id, &
                            line_number, filename)
    end function new_coverage_branch

    ! Branch coverage initialization
    subroutine branch_init(this, taken_count, not_taken_count, branch_id, &
                          line_number, filename)
        class(coverage_branch_t), intent(inout) :: this
        integer, intent(in) :: taken_count
        integer, intent(in) :: not_taken_count
        integer, intent(in) :: branch_id
        integer, intent(in) :: line_number
        character(len=*), intent(in) :: filename
        
        this%taken_count = taken_count
        this%not_taken_count = not_taken_count
        this%branch_id = branch_id
        this%line_number = line_number
        this%filename = filename
        call this%location%init(filename, line_number)
    end subroutine branch_init

    ! Check if branch is partially covered
    function branch_is_partially_covered(this) result(partial)
        class(coverage_branch_t), intent(in) :: this
        logical :: partial
        
        partial = this%taken_count > 0
    end function branch_is_partially_covered

    ! Check if branch is fully covered
    function branch_is_fully_covered(this) result(full)
        class(coverage_branch_t), intent(in) :: this
        logical :: full
        
        full = (this%taken_count > 0) .and. (this%not_taken_count > 0)
    end function branch_is_fully_covered

    ! Function coverage constructor
    function new_coverage_function(name, parent_module, is_module_procedure, &
                                  execution_count, line_number, filename) &
                                  result(func_cov)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: parent_module
        logical, intent(in) :: is_module_procedure
        integer, intent(in) :: execution_count
        integer, intent(in) :: line_number
        character(len=*), intent(in) :: filename
        type(coverage_function_t) :: func_cov
        
        call func_cov%init(name, parent_module, is_module_procedure, &
                          execution_count, line_number, filename)
    end function new_coverage_function

    ! Function coverage initialization
    subroutine function_init(this, name, parent_module, is_module_procedure, &
                            execution_count, line_number, filename)
        class(coverage_function_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: parent_module
        logical, intent(in) :: is_module_procedure
        integer, intent(in) :: execution_count
        integer, intent(in) :: line_number
        character(len=*), intent(in) :: filename
        
        this%name = name
        this%parent_module = parent_module
        this%is_module_procedure = is_module_procedure
        this%execution_count = execution_count
        this%line_number = line_number
        this%filename = filename
        call this%location%init(filename, line_number)
    end subroutine function_init

    ! File coverage constructor
    function new_coverage_file(filename, lines) result(file_cov)
        character(len=*), intent(in) :: filename
        type(coverage_line_t), intent(in) :: lines(:)
        type(coverage_file_t) :: file_cov
        
        call file_cov%init(filename, lines)
    end function new_coverage_file

    ! File coverage initialization
    subroutine file_init(this, filename, lines)
        class(coverage_file_t), intent(inout) :: this
        character(len=*), intent(in) :: filename
        type(coverage_line_t), intent(in) :: lines(:)
        
        this%filename = filename
        allocate(this%lines, source=lines)
    end subroutine file_init

    ! Calculate line coverage percentage for file
    function file_get_line_coverage_percentage(this) result(percentage)
        class(coverage_file_t), intent(in) :: this
        real :: percentage
        integer :: i, covered_count, executable_count
        
        covered_count = 0
        executable_count = 0
        
        do i = 1, size(this%lines)
            if (this%lines(i)%is_executable) then
                executable_count = executable_count + 1
                if (this%lines(i)%is_covered()) then
                    covered_count = covered_count + 1
                end if
            end if
        end do
        
        if (executable_count == 0) then
            percentage = 0.0
        else
            percentage = (real(covered_count) / real(executable_count)) * 100.0
        end if
    end function file_get_line_coverage_percentage

    ! Get number of executable lines for file
    function file_get_executable_line_count(this) result(count)
        class(coverage_file_t), intent(in) :: this
        integer :: count
        integer :: i
        
        count = 0
        do i = 1, size(this%lines)
            if (this%lines(i)%is_executable) then
                count = count + 1
            end if
        end do
    end function file_get_executable_line_count

    ! Get number of covered executable lines for file
    function file_get_covered_line_count(this) result(count)
        class(coverage_file_t), intent(in) :: this
        integer :: count
        integer :: i
        
        count = 0
        do i = 1, size(this%lines)
            if (this%lines(i)%is_executable .and. this%lines(i)%is_covered()) then
                count = count + 1
            end if
        end do
    end function file_get_covered_line_count

    ! Coverage data constructor
    function new_coverage_data(files) result(data)
        type(coverage_file_t), intent(in), optional :: files(:)
        type(coverage_data_t) :: data
        
        if (present(files)) then
            call data%init(files)
        else
            call data%init()
        end if
    end function new_coverage_data

    ! Coverage data initialization
    subroutine coverage_data_init(this, files)
        class(coverage_data_t), intent(inout) :: this
        type(coverage_file_t), intent(in), optional :: files(:)
        
        ! Clean up existing allocation if present
        if (allocated(this%files)) deallocate(this%files)
        
        if (present(files)) then
            allocate(this%files, source=files)
        else
            allocate(this%files(0))
        end if
    end subroutine coverage_data_init

    ! Serialize coverage data to string (simplified implementation)
    function coverage_data_serialize(this) result(serialized)
        class(coverage_data_t), intent(in) :: this
        character(len=:), allocatable :: serialized
        character(len=:), allocatable :: result_str
        character(len=:), allocatable :: line_str
        integer :: i, j
        integer :: required_length
        integer :: max_filename_len, max_line_num, max_count
        
        ! Security: Calculate maximum required buffer size for any line
        ! This prevents buffer overflow by ensuring adequate space
        max_filename_len = 0
        max_line_num = 0
        max_count = 0
        
        ! Find maximum values to calculate safe buffer size
        do i = 1, size(this%files)
            max_filename_len = max(max_filename_len, len_trim(this%files(i)%filename))
            do j = 1, size(this%files(i)%lines)
                max_line_num = max(max_line_num, this%files(i)%lines(j)%line_number)
                max_count = max(max_count, this%files(i)%lines(j)%execution_count)
            end do
        end do
        
        ! Calculate safe buffer size with security margins
        ! Format: filename:line:count| 
        ! Add extra space for formatting and safety margin
        ! Estimate digits needed for integers (max 10 digits each + separators)
        required_length = max_filename_len + 50  ! Safe margin for numbers and &
        ! separators
        
        ! Security: Enforce maximum filename length to prevent abuse
        if (max_filename_len > 4096) then
            ! Extremely long filenames could indicate an attack
            serialized = "ERROR: Filename too long for security"
            return
        end if
        
        ! Initialize result string
        result_str = ""
        
        do i = 1, size(this%files)
            do j = 1, size(this%files(i)%lines)
                ! Dynamically allocate line_str with calculated safe size
                allocate(character(len=required_length) :: line_str)
                
                write(line_str, '(A,A,I0,A,I0,A)') &
                    trim(this%files(i)%filename), ":", &
                    this%files(i)%lines(j)%line_number, ":", &
                    this%files(i)%lines(j)%execution_count, "|"
                
                result_str = result_str // trim(line_str)
                
                ! Clean up line_str for next iteration
                deallocate(line_str)
            end do
        end do
        
        serialized = result_str
    end function coverage_data_serialize

    ! Deserialize coverage data from string (placeholder implementation)
    subroutine coverage_data_deserialize(this, serialized)
        class(coverage_data_t), intent(inout) :: this
        character(len=*), intent(in) :: serialized
        
        ! Clean up existing allocation if present
        if (allocated(this%files)) deallocate(this%files)
        
        ! Placeholder implementation: create empty structure
        ! Real implementation would parse the serialized string
        allocate(this%files(0))
        
        ! Suppress unused variable warning
        associate(dummy => serialized)
        end associate
    end subroutine coverage_data_deserialize

    ! Line diff constructor
    function new_line_diff(baseline_line, current_line, diff_type) result(line_diff)
        type(coverage_line_t), intent(in) :: baseline_line
        type(coverage_line_t), intent(in) :: current_line
        integer, intent(in) :: diff_type
        type(line_diff_t) :: line_diff
        
        call line_diff%init(baseline_line, current_line, diff_type)
    end function new_line_diff

    ! Line diff initialization
    subroutine line_diff_init(this, baseline_line, current_line, diff_type)
        class(line_diff_t), intent(inout) :: this
        type(coverage_line_t), intent(in) :: baseline_line
        type(coverage_line_t), intent(in) :: current_line
        integer, intent(in) :: diff_type
        
        this%line_number = current_line%line_number
        this%baseline_line = baseline_line
        this%current_line = current_line
        this%diff_type = diff_type
        call this%calculate_delta()
    end subroutine line_diff_init

    ! Calculate line diff deltas
    subroutine line_diff_calculate_delta(this)
        class(line_diff_t), intent(inout) :: this
        
        this%execution_count_delta = this%current_line%execution_count - &
                                   this%baseline_line%execution_count
        
        ! Check for newly covered/uncovered lines
        select case (this%diff_type)
        case (DIFF_CHANGED)
            ! Newly covered: went from 0 to >0 execution count
            this%is_newly_covered = (this%baseline_line%execution_count == 0 .and. &
                                   this%current_line%execution_count > 0)
            ! Newly uncovered: went from >0 to 0 execution count
            this%is_newly_uncovered = (this%baseline_line%execution_count > 0 .and. &
                                     this%current_line%execution_count == 0)
        case (DIFF_ADDED)
            ! Added lines don't count as newly covered/uncovered - they're just added
            this%is_newly_covered = .false.
            this%is_newly_uncovered = .false.
        case (DIFF_REMOVED)
            ! Removed lines don't count as newly covered/uncovered - they're &
            ! just removed
            this%is_newly_covered = .false.
            this%is_newly_uncovered = .false.
        case default
            this%is_newly_covered = .false.
            this%is_newly_uncovered = .false.
        end select
    end subroutine line_diff_calculate_delta

    ! File diff constructor
    function new_file_diff(filename, line_diffs) result(file_diff)
        character(len=*), intent(in) :: filename
        type(line_diff_t), intent(in) :: line_diffs(:)
        type(file_diff_t) :: file_diff
        
        call file_diff%init(filename, line_diffs)
    end function new_file_diff

    ! File diff initialization
    subroutine file_diff_init(this, filename, line_diffs)
        class(file_diff_t), intent(inout) :: this
        character(len=*), intent(in) :: filename
        type(line_diff_t), intent(in) :: line_diffs(:)
        
        this%filename = filename
        if (allocated(this%line_diffs)) deallocate(this%line_diffs)
        allocate(this%line_diffs, source=line_diffs)
        call this%calculate_summary()
    end subroutine file_diff_init

    ! Calculate file diff summary statistics
    subroutine file_diff_calculate_summary(this)
        class(file_diff_t), intent(inout) :: this
        integer :: i
        
        this%added_lines = 0
        this%removed_lines = 0
        this%changed_lines = 0
        this%unchanged_lines = 0
        this%newly_covered_lines = 0
        this%newly_uncovered_lines = 0
        
        do i = 1, size(this%line_diffs)
            select case (this%line_diffs(i)%diff_type)
            case (DIFF_ADDED)
                this%added_lines = this%added_lines + 1
            case (DIFF_REMOVED)
                this%removed_lines = this%removed_lines + 1
            case (DIFF_CHANGED)
                this%changed_lines = this%changed_lines + 1
            case (DIFF_UNCHANGED)
                this%unchanged_lines = this%unchanged_lines + 1
            end select
            
            if (this%line_diffs(i)%is_newly_covered) then
                this%newly_covered_lines = this%newly_covered_lines + 1
            end if
            if (this%line_diffs(i)%is_newly_uncovered) then
                this%newly_uncovered_lines = this%newly_uncovered_lines + 1
            end if
        end do
        
        ! Calculate coverage percentage delta
        this%coverage_percentage_delta = this%current_coverage_percentage - &
                                       this%baseline_coverage_percentage
    end subroutine file_diff_calculate_summary

    ! Coverage diff constructor
    function new_coverage_diff(file_diffs, include_unchanged, threshold) &
           result(coverage_diff)
        type(file_diff_t), intent(in) :: file_diffs(:)
        logical, intent(in), optional :: include_unchanged
        real, intent(in), optional :: threshold
        type(coverage_diff_t) :: coverage_diff
        
        call coverage_diff%init(file_diffs, include_unchanged, threshold)
    end function new_coverage_diff

    ! Coverage diff initialization
    subroutine coverage_diff_init(this, file_diffs, include_unchanged, threshold)
        class(coverage_diff_t), intent(inout) :: this
        type(file_diff_t), intent(in) :: file_diffs(:)
        logical, intent(in), optional :: include_unchanged
        real, intent(in), optional :: threshold
        
        if (allocated(this%file_diffs)) deallocate(this%file_diffs)
        allocate(this%file_diffs, source=file_diffs)
        
        if (present(include_unchanged)) then
            this%include_unchanged = include_unchanged
        else
            this%include_unchanged = .false.
        end if
        
        if (present(threshold)) then
            this%significance_threshold = threshold
        else
            this%significance_threshold = 0.0
        end if
        
        call this%calculate_totals()
    end subroutine coverage_diff_init

    ! Calculate total diff statistics
    subroutine coverage_diff_calculate_totals(this)
        class(coverage_diff_t), intent(inout) :: this
        integer :: i
        
        this%total_added_lines = 0
        this%total_removed_lines = 0
        this%total_changed_lines = 0
        this%total_unchanged_lines = 0
        this%total_newly_covered_lines = 0
        this%total_newly_uncovered_lines = 0
        
        do i = 1, size(this%file_diffs)
            this%total_added_lines = this%total_added_lines + &
                                    this%file_diffs(i)%added_lines
            this%total_removed_lines = this%total_removed_lines + &
                                     this%file_diffs(i)%removed_lines
            this%total_changed_lines = this%total_changed_lines + &
                                     this%file_diffs(i)%changed_lines
            this%total_unchanged_lines = this%total_unchanged_lines + &
                                       this%file_diffs(i)%unchanged_lines
            this%total_newly_covered_lines = this%total_newly_covered_lines + &
                                           this%file_diffs(i)%newly_covered_lines
            this%total_newly_uncovered_lines = this%total_newly_uncovered_lines + &
                                             this%file_diffs(i)%newly_uncovered_lines
        end do
        
        this%total_coverage_delta = this%current_total_coverage - &
                                  this%baseline_total_coverage
    end subroutine coverage_diff_calculate_totals

    ! Filter diff results by significance threshold
    subroutine coverage_diff_filter_by_threshold(this)
        class(coverage_diff_t), intent(inout) :: this
        type(file_diff_t), allocatable :: filtered_diffs(:)
        integer :: i, filtered_count
        
        ! Count files that meet the threshold
        filtered_count = 0
        do i = 1, size(this%file_diffs)
            if (abs(this%file_diffs(i)%coverage_percentage_delta) >= &
                this%significance_threshold) then
                filtered_count = filtered_count + 1
            end if
        end do
        
        ! Create filtered array
        allocate(filtered_diffs(filtered_count))
        filtered_count = 0
        do i = 1, size(this%file_diffs)
            if (abs(this%file_diffs(i)%coverage_percentage_delta) >= &
                this%significance_threshold) then
                filtered_count = filtered_count + 1
                filtered_diffs(filtered_count) = this%file_diffs(i)
            end if
        end do
        
        ! Replace original array
        deallocate(this%file_diffs)
        allocate(this%file_diffs, source=filtered_diffs)
        deallocate(filtered_diffs)
        
        ! Recalculate totals
        call this%calculate_totals()
    end subroutine coverage_diff_filter_by_threshold

    ! Enhanced threshold-based analysis implementations

    ! Initialize diff thresholds with custom values
    subroutine diff_thresholds_init(this, critical, major, minor, significance)
        class(diff_thresholds_t), intent(inout) :: this
        real, intent(in), optional :: critical, major, minor, significance
        
        if (present(critical)) this%critical_threshold = critical
        if (present(major)) this%major_threshold = major
        if (present(minor)) this%minor_threshold = minor
        if (present(significance)) this%significance_threshold = significance
    end subroutine diff_thresholds_init

    ! Classify coverage change using threshold-based analysis
    function classify_coverage_change(this, baseline_pct, current_pct) &
             & result(classification)
        class(diff_thresholds_t), intent(in) :: this
        real, intent(in) :: baseline_pct, current_pct
        integer :: classification
        
        real :: abs_change
        
        abs_change = current_pct - baseline_pct
        
        ! Handle edge cases for new/lost coverage
        if (baseline_pct == 0.0 .and. current_pct > 0.0) then
            classification = NEW_COVERAGE
            return
        else if (baseline_pct > 0.0 .and. current_pct == 0.0) then
            classification = LOST_COVERAGE
            return
        end if
        
        ! Apply threshold-based classification
        if (abs(abs_change) < this%significance_threshold) then
            classification = UNCHANGED_COVERAGE
        else if (abs(abs_change) >= this%critical_threshold) then
            classification = merge(CRITICAL_IMPROVEMENT, CRITICAL_DEGRADATION, &
                                   & abs_change > 0)
        else if (abs(abs_change) >= this%major_threshold) then
            classification = merge(MAJOR_IMPROVEMENT, MAJOR_DEGRADATION, abs_change > 0)
        else if (abs(abs_change) >= this%minor_threshold) then
            classification = merge(MINOR_IMPROVEMENT, MINOR_DEGRADATION, abs_change > 0)
        else
            classification = UNCHANGED_COVERAGE
        end if
    end function classify_coverage_change

    ! Apply threshold classification to line diff
    subroutine line_diff_apply_classification(this, thresholds)
        class(line_diff_t), intent(inout) :: this
        type(diff_thresholds_t), intent(in) :: thresholds
        
        real :: baseline_pct, current_pct
        
        ! Convert execution counts to coverage percentages for the line
        baseline_pct = merge(100.0, 0.0, this%baseline_line%execution_count > 0)
        current_pct = merge(100.0, 0.0, this%current_line%execution_count > 0)
        
        ! Apply threshold-based classification
        this%significance_classification = &
                thresholds%classify_change(baseline_pct, current_pct)
        
        ! Set confidence based on change magnitude and line context
        if (this%significance_classification == UNCHANGED_COVERAGE) then
            this%statistical_confidence = 1.0
        else
            ! Higher confidence for more significant changes
            this%statistical_confidence = min(1.0, abs(current_pct - &
                                               & baseline_pct) / 10.0)
        end if
    end subroutine line_diff_apply_classification

    ! Apply threshold analysis to file diff
    subroutine file_diff_apply_threshold_analysis(this, thresholds)
        class(file_diff_t), intent(inout) :: this
        type(diff_thresholds_t), intent(in) :: thresholds
        
        integer :: i
        
        ! Apply classification based on overall file coverage change
        this%overall_significance_classification = &
            thresholds%classify_change(this%baseline_coverage_percentage, &
                                     this%current_coverage_percentage)
        
        ! Calculate statistical confidence based on sample size and change magnitude
        this%total_executable_lines = max(1, this%added_lines + this%removed_lines + &
                                        this%changed_lines + this%unchanged_lines)
        
        ! Higher confidence with more lines and larger changes
        this%statistical_confidence = min(1.0, &
            (abs(this%coverage_percentage_delta) / 10.0) * &
            sqrt(real(this%total_executable_lines) / 100.0))
        
        ! Apply classification to individual line diffs
        if (allocated(this%line_diffs)) then
            do i = 1, size(this%line_diffs)
                call this%line_diffs(i)%apply_threshold_classification(thresholds)
            end do
        end if
        
        ! Generate significance description
        this%significance_description = this%get_significance_description()
    end subroutine file_diff_apply_threshold_analysis

    ! Get human-readable significance description
    function file_diff_get_significance_description(this) result(description)
        class(file_diff_t), intent(in) :: this
        character(len=:), allocatable :: description
        
        select case (this%overall_significance_classification)
        case (CRITICAL_IMPROVEMENT)
            description = "Critical improvement (≥5% coverage increase)"
        case (MAJOR_IMPROVEMENT)
            description = "Major improvement (≥2% coverage increase)"
        case (MINOR_IMPROVEMENT)
            description = "Minor improvement (≥0.5% coverage increase)"
        case (UNCHANGED_COVERAGE)
            description = "No significant change (<0.1% difference)"
        case (MINOR_DEGRADATION)
            description = "Minor degradation (≥0.5% coverage decrease)"
        case (MAJOR_DEGRADATION)
            description = "Major degradation (≥2% coverage decrease)"
        case (CRITICAL_DEGRADATION)
            description = "Critical degradation (≥5% coverage decrease)"
        case (NEW_COVERAGE)
            description = "New coverage added"
        case (LOST_COVERAGE)
            description = "Coverage completely lost"
        case default
            description = "Unknown classification"
        end select
    end function file_diff_get_significance_description

end module coverage_model
