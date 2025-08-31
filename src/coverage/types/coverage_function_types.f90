module coverage_function_types
    !! Function Coverage Types Module
    !!
    !! Contains function-level coverage types and procedures:
    !! - coverage_function_t: Function-level coverage data with execution tracking
    !! - Associated procedures for coverage analysis and reporting
    !!
    !! This module is part of the architectural decomposition of the original
    !! coverage_complex_types.f90 to maintain <500 line limits.

    use constants_core
    use coverage_basic_types
    implicit none
    private

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

    ! Constructor interface
    interface coverage_function_t
        module procedure :: function_constructor
    end interface coverage_function_t

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

end module coverage_function_types