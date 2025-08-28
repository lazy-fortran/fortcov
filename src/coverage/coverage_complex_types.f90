module coverage_complex_types
    !! Complex Coverage Types Module
    !!
    !! Contains complex types for coverage analysis:
    !! - coverage_function_t: Function-level coverage data
    !! - coverage_file_t: File-level coverage aggregation
    !! - coverage_data_t: Overall coverage data container
    
    use constants_core
    use coverage_basic_types
    implicit none
    private
    
    ! Export required procedures for proper functionality
    public :: file_init_simple, file_init_with_lines
    public :: file_calculate_coverage
    public :: data_init_simple, data_init_with_files
    public :: data_calculate_overall_coverage
    
    ! ============================================================================
    ! Complex Coverage Types
    ! ============================================================================
    
    ! Function Coverage Type
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
    
    ! File Coverage Type
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
    
    ! Coverage Data Container Type
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
    ! Constructor Interfaces
    ! ============================================================================
    
    ! Constructor interfaces
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
    
contains
    
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
    
end module coverage_complex_types