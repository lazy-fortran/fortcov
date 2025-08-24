module coverage_types
    !! Core Coverage Data Types
    !!
    !! Contains all type definitions for the coverage data model.
    !! Separated from operations for better separation of concerns
    !! and to maintain QADS size limits.
    use foundation_constants
    implicit none
    private
    
    ! Public type definitions
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
    
    ! Source location type
    type :: source_location_t
        character(len=:), allocatable :: filename
        integer :: line_number
        integer :: column_start = 0
        integer :: column_end = 0
    contains
        procedure :: init => source_location_init
    end type source_location_t
    
    ! Line coverage type with constructor interface
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
    
    ! Constructor interface for coverage_line_t
    interface coverage_line_t
        module procedure :: line_constructor
    end interface coverage_line_t
    
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
    
    ! Constructor interface for coverage_branch_t
    interface coverage_branch_t
        module procedure :: branch_constructor
    end interface coverage_branch_t
    
    ! Function coverage type
    type :: coverage_function_t
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
    
    ! Constructor interface for coverage_function_t
    interface coverage_function_t
        module procedure :: function_constructor
    end interface coverage_function_t
    
    ! File coverage type (enhanced for JSON compatibility)
    type :: coverage_file_t
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
    
    ! Constructor interface for coverage_file_t
    interface coverage_file_t
        module procedure :: file_constructor
    end interface coverage_file_t
    
    ! Compatible file coverage type for JSON I/O
    type :: file_coverage_t
        character(len=:), allocatable :: filename
        type(line_coverage_t), allocatable :: lines(:)
    end type file_coverage_t
    
    ! Compatible line coverage type for JSON I/O
    type :: line_coverage_t
        integer :: line_number = 0
        integer :: execution_count = 0
    end type line_coverage_t
    
    ! Coverage data container type
    type :: coverage_data_t
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
    
    ! Constructor interface for coverage_data_t
    interface coverage_data_t
        module procedure :: data_constructor_empty
        module procedure :: data_constructor_with_files
    end interface coverage_data_t
    
    ! Coverage diff types
    type :: line_diff_t
        integer :: line_number = 0
        character(len=:), allocatable :: filename
        integer :: old_count = 0
        integer :: new_count = 0
        integer :: execution_count_delta = 0  ! new_count - old_count
        integer :: diff_type = 0  ! DIFF_UNCHANGED, DIFF_ADDED, DIFF_REMOVED, DIFF_CHANGED
        character(len=1) :: status = ' '  ! '+', '-', '='
        logical :: is_newly_covered = .false.  ! old_count == 0 and new_count > 0
        logical :: is_newly_uncovered = .false.  ! old_count > 0 and new_count == 0
    contains
        procedure :: init => line_diff_init
    end type line_diff_t
    
    ! Constructor interface for line_diff_t
    interface line_diff_t
        module procedure :: line_diff_constructor
    end interface line_diff_t
    
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

contains

    ! Constructor function references - actual implementations in coverage_constructors
    function line_constructor(execution_count, line_number, filename, is_executable) result(this)
        integer, intent(in) :: execution_count
        integer, intent(in) :: line_number
        character(len=*), intent(in) :: filename
        logical, intent(in) :: is_executable
        type(coverage_line_t) :: this
        
        call this%init(filename, line_number, execution_count, is_executable)
        
    end function line_constructor
    
    function branch_constructor(taken_count, not_taken_count, branch_id, line_number, filename) result(this)
        integer, intent(in) :: taken_count
        integer, intent(in) :: not_taken_count
        integer, intent(in) :: branch_id
        integer, intent(in) :: line_number
        character(len=*), intent(in) :: filename
        type(coverage_branch_t) :: this
        
        call this%init(filename, line_number, branch_id, taken_count, not_taken_count)
        
    end function branch_constructor
    
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
    
    function file_constructor(filename, lines) result(this)
        character(len=*), intent(in) :: filename
        type(coverage_line_t), intent(in) :: lines(:)
        type(coverage_file_t) :: this
        
        call this%init(filename, lines)
        
    end function file_constructor
    
    function data_constructor_empty() result(this)
        type(coverage_data_t) :: this
        
        call this%init()
        
    end function data_constructor_empty
    
    function data_constructor_with_files(files) result(this)
        type(coverage_file_t), intent(in) :: files(:)
        type(coverage_data_t) :: this
        
        call this%init(files)
        
    end function data_constructor_with_files
    
    function line_diff_constructor(baseline_line, current_line, diff_type) result(this)
        type(coverage_line_t), intent(in) :: baseline_line
        type(coverage_line_t), intent(in) :: current_line
        integer, intent(in) :: diff_type
        type(line_diff_t) :: this
        
        call this%init(baseline_line, current_line, diff_type)
        
    end function line_diff_constructor

    ! Core method implementations - basic operations only
    subroutine source_location_init(this, filename, line_number, column_start, column_end)
        class(source_location_t), intent(out) :: this
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line_number
        integer, intent(in), optional :: column_start, column_end
        
        this%filename = filename
        this%line_number = line_number
        if (present(column_start)) this%column_start = column_start
        if (present(column_end)) this%column_end = column_end
        
    end subroutine source_location_init
    
    subroutine line_init(this, filename, line_number, execution_count, is_executable)
        class(coverage_line_t), intent(out) :: this
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line_number
        integer, intent(in), optional :: execution_count
        logical, intent(in), optional :: is_executable
        
        call this%location%init(filename, line_number)
        this%filename = filename
        this%line_number = line_number
        if (present(execution_count)) this%execution_count = execution_count
        if (present(is_executable)) this%is_executable = is_executable
        
    end subroutine line_init

    function line_is_covered(this) result(is_covered)
        class(coverage_line_t), intent(in) :: this
        logical :: is_covered
        
        is_covered = this%is_executable .and. this%execution_count > 0
        
    end function line_is_covered

    subroutine branch_init(this, filename, line_number, branch_id, taken_count, not_taken_count)
        class(coverage_branch_t), intent(out) :: this
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line_number
        integer, intent(in), optional :: branch_id, taken_count, not_taken_count
        
        call this%location%init(filename, line_number)
        this%filename = filename
        this%line_number = line_number
        if (present(branch_id)) this%branch_id = branch_id
        if (present(taken_count)) this%taken_count = taken_count
        if (present(not_taken_count)) this%not_taken_count = not_taken_count
        
    end subroutine branch_init

    function branch_is_partially_covered(this) result(is_covered)
        class(coverage_branch_t), intent(in) :: this
        logical :: is_covered
        
        is_covered = this%taken_count > 0
        
    end function branch_is_partially_covered

    function branch_is_fully_covered(this) result(is_covered)
        class(coverage_branch_t), intent(in) :: this
        logical :: is_covered
        
        is_covered = this%taken_count > 0 .and. this%not_taken_count > 0
        
    end function branch_is_fully_covered

    ! Minimal implementations - full implementations in coverage_constructors module
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

    ! File type minimal implementations
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

    ! Data type minimal implementations
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

    ! Diff type minimal implementations
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