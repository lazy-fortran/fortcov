module coverage_types
    !! Consolidated Coverage Types Module
    !!
    !! This module consolidates all coverage-related types into a single module
    !! to reduce module count from 6 to 1 (83% reduction).
    !! Combines: coverage_location_types, coverage_function_types, coverage_file_types,
    !!           coverage_data_types, coverage_diff_types, and original coverage_types
    
    use constants_core
    implicit none
    private
    
    ! ============================================================================
    ! Constants and Parameters
    ! ============================================================================
    
    ! Maximum lengths for various strings
    ! Using COV_ prefix to avoid conflicts with constants_core
    integer, parameter, public :: COV_MAX_FILENAME_LENGTH = 512
    integer, parameter, public :: COV_MAX_NAME_LENGTH = 256
    
    ! Diff status enumeration
    integer, parameter, public :: DIFF_UNCHANGED = 0
    integer, parameter, public :: DIFF_IMPROVED = 1
    integer, parameter, public :: DIFF_DEGRADED = -1
    integer, parameter, public :: DIFF_NEW_LINE = 2
    integer, parameter, public :: DIFF_REMOVED_LINE = -2
    
    ! ============================================================================
    ! Location and Basic Coverage Types
    ! ============================================================================
    
    ! Source location type
    type, public :: source_location_t
        character(len=COV_MAX_FILENAME_LENGTH) :: filename = ""
        integer :: line_number = 0
        integer :: column_start = 0
        integer :: column_end = 0
    contains
        procedure :: init => source_location_init
    end type source_location_t
    
    ! Coverage line type
    type, public :: coverage_line_t
        type(source_location_t) :: location
        integer :: execution_count = 0
        logical :: is_executable = .false.
        logical :: covered = .false.
        logical :: is_branch = .false.
        ! Additional fields for compatibility
        integer :: line_number = 0
        character(len=COV_MAX_FILENAME_LENGTH) :: filename = ""
    contains
        procedure :: init => line_init
        procedure :: is_covered => line_is_covered
    end type coverage_line_t
    
    ! Coverage branch type
    type, public :: coverage_branch_t
        type(source_location_t) :: location
        integer :: branch_id = 0
        integer :: taken_count = 0
        integer :: not_taken_count = 0
        real :: coverage_percentage = 0.0
        logical :: is_covered = .false.
        ! Additional fields for compatibility  
        integer :: line_number = 0
        character(len=COV_MAX_FILENAME_LENGTH) :: filename = ""
    contains
        procedure :: init => branch_init
        procedure :: is_partially_covered => branch_is_partially_covered
        procedure :: is_fully_covered => branch_is_fully_covered
    end type coverage_branch_t
    
    ! Simple line coverage type for compatibility
    type, public :: line_coverage_t
        integer :: line_number = 0
        integer :: execution_count = 0
    end type line_coverage_t
    
    ! Simple file coverage type for compatibility
    type, public :: file_coverage_t
        character(len=:), allocatable :: filename
        type(line_coverage_t), allocatable :: lines(:)
    end type file_coverage_t
    
    ! ============================================================================
    ! Function Coverage Type
    ! ============================================================================
    
    type, public :: coverage_function_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: parent_module
        logical :: is_module_procedure = .false.
        type(source_location_t) :: location
        integer :: execution_count = 0
        integer :: line_number = 0
        character(len=:), allocatable :: filename
        type(coverage_line_t), allocatable :: lines(:)
        type(coverage_branch_t), allocatable :: branches(:)
    contains
        procedure :: is_covered => function_is_covered
        procedure :: get_line_coverage => function_get_line_coverage
        procedure :: get_branch_coverage => function_get_branch_coverage
        procedure :: init => function_init
    end type coverage_function_t
    
    ! ============================================================================
    ! File Coverage Type
    ! ============================================================================
    
    type, public :: coverage_file_t
        character(len=:), allocatable :: filename
        type(coverage_line_t), allocatable :: lines(:)
        type(coverage_branch_t), allocatable :: branches(:)
        type(coverage_function_t), allocatable :: functions(:)
        integer :: total_lines = 0
        integer :: covered_lines = 0
        real :: line_coverage = 0.0
    contains
        procedure :: calculate_coverage => file_calculate_coverage
        procedure :: get_line_coverage => file_get_line_coverage
        procedure :: get_branch_coverage => file_get_branch_coverage
        procedure :: get_function_coverage => file_get_function_coverage
        procedure :: get_line_coverage_percentage => file_get_line_coverage_percentage
        procedure :: get_executable_line_count => file_get_executable_line_count
        procedure :: get_covered_line_count => file_get_covered_line_count
        procedure :: file_init_simple
        procedure :: file_init_with_lines
        generic :: init => file_init_simple, file_init_with_lines
    end type coverage_file_t
    
    ! ============================================================================
    ! Coverage Data Container Type
    ! ============================================================================
    
    type, public :: coverage_data_t
        character(len=:), allocatable :: version
        character(len=:), allocatable :: tool
        character(len=:), allocatable :: timestamp
        type(coverage_file_t), allocatable :: files(:)
        type(file_coverage_t), allocatable :: files_json(:)  ! JSON compatibility
        integer :: total_files = 0
        integer :: total_lines = 0
        integer :: covered_lines = 0
        real :: overall_coverage = 0.0
    contains
        procedure :: calculate_overall_coverage => data_calculate_overall_coverage
        procedure :: get_file_count => data_get_file_count
        procedure :: get_total_lines => data_get_total_lines
        procedure :: get_covered_lines => data_get_covered_lines
        procedure :: data_init_simple
        procedure :: data_init_with_files
        generic :: init => data_init_simple, data_init_with_files
        procedure :: serialize => data_serialize
        procedure :: deserialize => data_deserialize
    end type coverage_data_t
    
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
    
    interface coverage_function_t
        module procedure :: function_constructor
    end interface coverage_function_t
    
    interface coverage_file_t
        module procedure :: file_constructor
    end interface coverage_file_t
    
    interface coverage_data_t
        module procedure :: data_constructor_empty
        module procedure :: data_constructor_with_files
    end interface coverage_data_t
    
    ! Public constructors
    public :: line_constructor
    public :: branch_constructor
    public :: line_diff_constructor
    
contains
    
    ! ============================================================================
    ! Source Location Implementation
    ! ============================================================================
    
    subroutine source_location_init(this, filename, line_number, column_start, column_end)
        class(source_location_t), intent(out) :: this
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line_number
        integer, intent(in), optional :: column_start
        integer, intent(in), optional :: column_end
        
        this%filename = filename
        this%line_number = line_number
        this%column_start = 0
        this%column_end = 0
        
        if (present(column_start)) this%column_start = column_start
        if (present(column_end)) this%column_end = column_end
    end subroutine source_location_init
    
    ! ============================================================================
    ! Coverage Line Implementation
    ! ============================================================================
    
    function line_constructor(execution_count, line_number, filename, is_executable) result(this)
        integer, intent(in) :: execution_count
        integer, intent(in) :: line_number
        character(len=*), intent(in) :: filename
        logical, intent(in) :: is_executable
        type(coverage_line_t) :: this
        
        call this%init(filename, line_number, execution_count, is_executable)
    end function line_constructor
    
    subroutine line_init(this, filename, line_number, execution_count, is_executable)
        class(coverage_line_t), intent(out) :: this
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line_number
        integer, intent(in) :: execution_count
        logical, intent(in), optional :: is_executable
        
        call this%location%init(filename, line_number)
        this%execution_count = execution_count
        this%is_executable = .true.
        if (present(is_executable)) this%is_executable = is_executable
        
        this%covered = (execution_count > 0)
        this%is_branch = .false.
        this%line_number = line_number
        this%filename = filename
    end subroutine line_init
    
    function line_is_covered(this) result(is_covered)
        class(coverage_line_t), intent(in) :: this
        logical :: is_covered
        
        is_covered = this%covered .or. (this%execution_count > 0)
    end function line_is_covered
    
    ! ============================================================================
    ! Coverage Branch Implementation
    ! ============================================================================
    
    function branch_constructor(taken_count, not_taken_count, branch_id, line_number, filename) result(this)
        integer, intent(in) :: taken_count
        integer, intent(in) :: not_taken_count
        integer, intent(in) :: branch_id
        integer, intent(in) :: line_number
        character(len=*), intent(in) :: filename
        type(coverage_branch_t) :: this
        
        call this%init(filename, line_number, branch_id, taken_count, not_taken_count)
    end function branch_constructor
    
    subroutine branch_init(this, filename, line_number, branch_id, taken_count, not_taken_count)
        class(coverage_branch_t), intent(out) :: this
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line_number
        integer, intent(in) :: branch_id
        integer, intent(in) :: taken_count
        integer, intent(in) :: not_taken_count
        
        call this%location%init(filename, line_number)
        this%branch_id = branch_id
        this%taken_count = taken_count
        this%not_taken_count = not_taken_count
        this%line_number = line_number
        this%filename = filename
        this%is_covered = (taken_count > 0) .or. (not_taken_count > 0)
        
        if ((taken_count + not_taken_count) > 0) then
            this%coverage_percentage = real(max(taken_count, not_taken_count)) / &
                                       real(taken_count + not_taken_count) * 100.0
        else
            this%coverage_percentage = 0.0
        end if
    end subroutine branch_init
    
    function branch_is_partially_covered(this) result(is_covered)
        class(coverage_branch_t), intent(in) :: this
        logical :: is_covered
        
        is_covered = ((this%taken_count > 0) .and. (this%not_taken_count == 0)) .or. &
                     ((this%taken_count == 0) .and. (this%not_taken_count > 0))
    end function branch_is_partially_covered
    
    function branch_is_fully_covered(this) result(is_covered)
        class(coverage_branch_t), intent(in) :: this
        logical :: is_covered
        
        is_covered = (this%taken_count > 0) .and. (this%not_taken_count > 0)
    end function branch_is_fully_covered
    
    ! ============================================================================
    ! Coverage Function Implementation
    ! ============================================================================
    
    function function_constructor(name, parent_module, is_module_procedure, execution_count, &
                                 line_number, filename) result(this)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: parent_module
        logical, intent(in) :: is_module_procedure
        integer, intent(in) :: execution_count
        integer, intent(in) :: line_number
        character(len=*), intent(in) :: filename
        type(coverage_function_t) :: this
        
        call this%init(name, filename, line_number, execution_count, parent_module, is_module_procedure)
    end function function_constructor
    
    subroutine function_init(this, name, filename, line_number, execution_count, &
                           parent_module, is_module_procedure)
        class(coverage_function_t), intent(out) :: this
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line_number
        integer, intent(in), optional :: execution_count
        character(len=*), intent(in), optional :: parent_module
        logical, intent(in), optional :: is_module_procedure
        
        this%name = name
        this%filename = filename
        this%line_number = line_number
        call this%location%init(filename, line_number)
        if (present(execution_count)) this%execution_count = execution_count
        if (present(parent_module)) this%parent_module = parent_module
        if (present(is_module_procedure)) this%is_module_procedure = is_module_procedure
    end subroutine function_init
    
    function function_is_covered(this) result(is_covered)
        class(coverage_function_t), intent(in) :: this
        logical :: is_covered
        
        is_covered = this%execution_count > 0
    end function function_is_covered
    
    function function_get_line_coverage(this) result(coverage)
        class(coverage_function_t), intent(in) :: this
        real :: coverage
        
        integer :: i, total_lines, covered_lines
        
        if (.not. allocated(this%lines)) then
            coverage = 0.0
            return
        end if
        
        total_lines = 0
        covered_lines = 0
        
        do i = 1, size(this%lines)
            if (this%lines(i)%is_executable) then
                total_lines = total_lines + 1
                if (this%lines(i)%is_covered()) then
                    covered_lines = covered_lines + 1
                end if
            end if
        end do
        
        if (total_lines > 0) then
            coverage = real(covered_lines) / real(total_lines) * 100.0
        else
            coverage = 0.0
        end if
    end function function_get_line_coverage
    
    function function_get_branch_coverage(this) result(coverage)
        class(coverage_function_t), intent(in) :: this
        real :: coverage
        
        integer :: i, total_branches, covered_branches
        
        if (.not. allocated(this%branches)) then
            coverage = 0.0
            return
        end if
        
        total_branches = size(this%branches)
        covered_branches = 0
        
        do i = 1, total_branches
            if (this%branches(i)%is_fully_covered()) then
                covered_branches = covered_branches + 1
            end if
        end do
        
        if (total_branches > 0) then
            coverage = real(covered_branches) / real(total_branches) * 100.0
        else
            coverage = 0.0
        end if
    end function function_get_branch_coverage
    
    ! ============================================================================
    ! Coverage File Implementation
    ! ============================================================================
    
    function file_constructor(filename, lines) result(this)
        character(len=*), intent(in) :: filename
        type(coverage_line_t), intent(in) :: lines(:)
        type(coverage_file_t) :: this
        
        call this%init(filename, lines)
    end function file_constructor
    
    subroutine file_init_simple(this, filename)
        class(coverage_file_t), intent(out) :: this
        character(len=*), intent(in) :: filename
        
        this%filename = filename
        this%total_lines = 0
        this%covered_lines = 0
        this%line_coverage = 0.0
    end subroutine file_init_simple
    
    subroutine file_init_with_lines(this, filename, lines)
        class(coverage_file_t), intent(out) :: this
        character(len=*), intent(in) :: filename
        type(coverage_line_t), intent(in) :: lines(:)
        
        this%filename = filename
        this%total_lines = 0
        this%covered_lines = 0
        this%line_coverage = 0.0
        allocate(this%lines, source=lines)
    end subroutine file_init_with_lines
    
    subroutine file_calculate_coverage(this)
        class(coverage_file_t), intent(inout) :: this
        
        integer :: i, total_executable, covered_count
        
        if (.not. allocated(this%lines)) return
        
        total_executable = 0
        covered_count = 0
        
        do i = 1, size(this%lines)
            if (this%lines(i)%is_executable) then
                total_executable = total_executable + 1
                if (this%lines(i)%is_covered()) then
                    covered_count = covered_count + 1
                end if
            end if
        end do
        
        this%total_lines = total_executable
        this%covered_lines = covered_count
        
        if (total_executable > 0) then
            this%line_coverage = real(covered_count) / real(total_executable) * 100.0
        else
            this%line_coverage = 0.0
        end if
    end subroutine file_calculate_coverage
    
    function file_get_line_coverage(this) result(coverage)
        class(coverage_file_t), intent(in) :: this
        real :: coverage
        coverage = this%line_coverage
    end function file_get_line_coverage
    
    function file_get_branch_coverage(this) result(coverage)
        class(coverage_file_t), intent(in) :: this
        real :: coverage
        
        integer :: i, total_branches, covered_branches
        
        if (.not. allocated(this%branches)) then
            coverage = 0.0
            return
        end if
        
        total_branches = size(this%branches)
        covered_branches = 0
        
        do i = 1, total_branches
            if (this%branches(i)%is_fully_covered()) then
                covered_branches = covered_branches + 1
            end if
        end do
        
        if (total_branches > 0) then
            coverage = real(covered_branches) / real(total_branches) * 100.0
        else
            coverage = 0.0
        end if
    end function file_get_branch_coverage
    
    function file_get_function_coverage(this) result(coverage)
        class(coverage_file_t), intent(in) :: this
        real :: coverage
        
        integer :: i, total_functions, covered_functions
        
        if (.not. allocated(this%functions)) then
            coverage = 0.0
            return
        end if
        
        total_functions = size(this%functions)
        covered_functions = 0
        
        do i = 1, total_functions
            if (this%functions(i)%is_covered()) then
                covered_functions = covered_functions + 1
            end if
        end do
        
        if (total_functions > 0) then
            coverage = real(covered_functions) / real(total_functions) * 100.0
        else
            coverage = 0.0
        end if
    end function file_get_function_coverage
    
    function file_get_line_coverage_percentage(this) result(percentage)
        class(coverage_file_t), intent(in) :: this
        real :: percentage
        
        integer :: i, executable_count, covered_count
        
        if (.not. allocated(this%lines)) then
            percentage = 0.0
            return
        end if
        
        executable_count = 0
        covered_count = 0
        
        do i = 1, size(this%lines)
            if (this%lines(i)%is_executable) then
                executable_count = executable_count + 1
                if (this%lines(i)%execution_count > 0) then
                    covered_count = covered_count + 1
                end if
            end if
        end do
        
        if (executable_count > 0) then
            percentage = real(covered_count) / real(executable_count) * 100.0
        else
            percentage = 0.0
        end if
    end function file_get_line_coverage_percentage
    
    function file_get_executable_line_count(this) result(count)
        class(coverage_file_t), intent(in) :: this
        integer :: count
        
        integer :: i
        
        count = 0
        if (.not. allocated(this%lines)) return
        
        do i = 1, size(this%lines)
            if (this%lines(i)%is_executable) then
                count = count + 1
            end if
        end do
    end function file_get_executable_line_count
    
    function file_get_covered_line_count(this) result(count)
        class(coverage_file_t), intent(in) :: this
        integer :: count
        
        integer :: i
        
        count = 0
        if (.not. allocated(this%lines)) return
        
        do i = 1, size(this%lines)
            if (this%lines(i)%is_executable .and. this%lines(i)%execution_count > 0) then
                count = count + 1
            end if
        end do
    end function file_get_covered_line_count
    
    ! ============================================================================
    ! Coverage Data Implementation
    ! ============================================================================
    
    function data_constructor_empty() result(this)
        type(coverage_data_t) :: this
        
        call this%init()
    end function data_constructor_empty
    
    function data_constructor_with_files(files) result(this)
        type(coverage_file_t), intent(in) :: files(:)
        type(coverage_data_t) :: this
        
        call this%init(files)
    end function data_constructor_with_files
    
    subroutine data_init_simple(this)
        class(coverage_data_t), intent(out) :: this
        
        this%version = "1.0"
        this%tool = "fortcov"
        this%total_files = 0
        this%total_lines = 0
        this%covered_lines = 0
        this%overall_coverage = 0.0
        allocate(this%files(0))
    end subroutine data_init_simple
    
    subroutine data_init_with_files(this, files)
        class(coverage_data_t), intent(out) :: this
        type(coverage_file_t), intent(in) :: files(:)
        
        this%version = "1.0"
        this%tool = "fortcov"
        this%total_files = 0
        this%total_lines = 0
        this%covered_lines = 0
        this%overall_coverage = 0.0
        allocate(this%files, source=files)
        this%total_files = size(files)
        call this%calculate_overall_coverage()
    end subroutine data_init_with_files
    
    subroutine data_calculate_overall_coverage(this)
        class(coverage_data_t), intent(inout) :: this
        
        integer :: i, total_lines, covered_lines
        
        if (.not. allocated(this%files)) return
        
        total_lines = 0
        covered_lines = 0
        
        do i = 1, size(this%files)
            call this%files(i)%calculate_coverage()
            total_lines = total_lines + this%files(i)%total_lines
            covered_lines = covered_lines + this%files(i)%covered_lines
        end do
        
        this%total_files = size(this%files)
        this%total_lines = total_lines
        this%covered_lines = covered_lines
        
        if (total_lines > 0) then
            this%overall_coverage = real(covered_lines) / real(total_lines) * 100.0
        else
            this%overall_coverage = 0.0
        end if
    end subroutine data_calculate_overall_coverage
    
    function data_get_file_count(this) result(count)
        class(coverage_data_t), intent(in) :: this
        integer :: count
        count = this%total_files
    end function data_get_file_count
    
    function data_get_total_lines(this) result(lines)
        class(coverage_data_t), intent(in) :: this
        integer :: lines
        lines = this%total_lines
    end function data_get_total_lines
    
    function data_get_covered_lines(this) result(lines)
        class(coverage_data_t), intent(in) :: this
        integer :: lines
        lines = this%covered_lines
    end function data_get_covered_lines
    
    function data_serialize(this) result(serialized)
        class(coverage_data_t), intent(in) :: this
        character(len=:), allocatable :: serialized
        
        ! Simplified implementation for basic serialization
        character(len=1000) :: temp_string
        
        if (.not. allocated(this%files) .or. size(this%files) == 0) then
            serialized = ""
            return
        end if
        
        write(temp_string, '(A,I0,A,I0,A,F6.2)') &
            "files=", size(this%files), &
            ";lines=", this%total_lines, &
            ";coverage=", this%overall_coverage
        
        serialized = trim(adjustl(temp_string))
    end function data_serialize
    
    subroutine data_deserialize(this, serialized)
        class(coverage_data_t), intent(inout) :: this
        character(len=*), intent(in) :: serialized
        
        ! Basic implementation - creates empty coverage data
        call this%init()
    end subroutine data_deserialize
    
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
    
end module coverage_types