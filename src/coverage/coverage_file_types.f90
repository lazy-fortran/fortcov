module coverage_file_types
    !! File Coverage Types Module
    !!
    !! Contains file-level coverage types and procedures:
    !! - coverage_file_t: File-level coverage aggregation with line/branch/function analysis
    !! - Associated procedures for coverage calculation and reporting
    !!
    !! This module is part of the architectural decomposition of the original
    !! coverage_complex_types.f90 to maintain <500 line limits.

    use constants_core
    use coverage_basic_types
    use coverage_function_types
    implicit none
    private

    ! Export required procedures for proper functionality
    public :: file_init_simple, file_init_with_lines, file_calculate_coverage

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

    ! Constructor interface
    interface coverage_file_t
        module procedure :: file_constructor
    end interface coverage_file_t

contains

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

end module coverage_file_types