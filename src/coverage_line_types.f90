module coverage_line_types
    !! Line Coverage Type Definitions
    !!
    !! Contains types for representing line-level coverage data.
    !! Extracted from coverage_types.f90 for SRP compliance.
    use source_location_types
    implicit none
    private

    ! Public types
    public :: coverage_line_t, line_coverage_t

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

    ! Simple line coverage type for basic reporting
    type :: line_coverage_t
        integer :: line_number = 0
        logical :: is_covered = .false.
        integer :: execution_count = 0
    contains
        procedure :: init => simple_line_init
    end type line_coverage_t

contains

    ! Check if line is covered
    function line_is_covered(this) result(covered)
        class(coverage_line_t), intent(in) :: this
        logical :: covered
        
        covered = this%is_executable .and. this%execution_count > 0
    end function line_is_covered

    ! Initialize line coverage
    subroutine line_init(this, filename, line_number, is_executable, &
                        execution_count)
        class(coverage_line_t), intent(out) :: this
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line_number
        logical, intent(in), optional :: is_executable
        integer, intent(in), optional :: execution_count

        this%filename = trim(filename)
        this%line_number = line_number
        call this%location%init(filename, line_number)

        if (present(is_executable)) then
            this%is_executable = is_executable
        else
            this%is_executable = .false.
        end if

        if (present(execution_count)) then
            this%execution_count = execution_count
        else
            this%execution_count = 0
        end if
    end subroutine line_init

    ! Line constructor
    function line_constructor(filename, line_number, is_executable, &
                             execution_count) result(line)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line_number
        logical, intent(in), optional :: is_executable
        integer, intent(in), optional :: execution_count
        type(coverage_line_t) :: line

        call line%init(filename, line_number, is_executable, execution_count)
    end function line_constructor

    ! Initialize simple line coverage
    subroutine simple_line_init(this, line_number, is_covered, execution_count)
        class(line_coverage_t), intent(out) :: this
        integer, intent(in) :: line_number
        logical, intent(in), optional :: is_covered
        integer, intent(in), optional :: execution_count

        this%line_number = line_number

        if (present(is_covered)) then
            this%is_covered = is_covered
        else
            this%is_covered = .false.
        end if

        if (present(execution_count)) then
            this%execution_count = execution_count
        else
            this%execution_count = 0
        end if
    end subroutine simple_line_init

end module coverage_line_types