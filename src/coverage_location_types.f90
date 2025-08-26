module coverage_location_types
    !! Location and basic coverage types
    !! 
    !! This module provides the basic types for source locations
    !! and simple coverage data structures.
    
    implicit none
    private

    ! Maximum lengths for various strings
    integer, parameter, public :: MAX_FILENAME_LENGTH = 512
    integer, parameter, public :: MAX_NAME_LENGTH = 256
    
    ! Source location type
    type, public :: source_location_t
        character(len=MAX_FILENAME_LENGTH) :: filename = ""
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
        logical :: is_covered = .false.
        logical :: is_branch = .false.
    contains
        procedure :: init => line_init
        procedure :: is_covered => line_is_covered
    end type coverage_line_t

    ! Coverage branch type - for tracking branch coverage
    type, public :: coverage_branch_t
        type(source_location_t) :: location
        integer :: branch_id = 0
        integer :: taken_count = 0
        integer :: not_taken_count = 0
        real :: coverage_percentage = 0.0
        logical :: is_covered = .false.
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

    ! File coverage type (simplified)
    type, public :: file_coverage_t
        character(len=MAX_FILENAME_LENGTH) :: filename = ""
        type(line_coverage_t), allocatable :: lines(:)
    end type file_coverage_t

    ! Constructors
    public :: line_constructor
    public :: branch_constructor

contains

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

    subroutine source_location_init(this, filename, line_number, column_start, column_end)
        !! Initialize a source location
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

    subroutine line_init(this, filename, line_number, execution_count, is_executable)
        !! Initialize a coverage line
        class(coverage_line_t), intent(out) :: this
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line_number
        integer, intent(in) :: execution_count
        logical, intent(in), optional :: is_executable
        
        call this%location%init(filename, line_number)
        this%execution_count = execution_count
        this%is_executable = .true.
        if (present(is_executable)) this%is_executable = is_executable
        
        this%is_covered = (execution_count > 0)
        this%is_branch = .false.
    end subroutine line_init

    function line_is_covered(this) result(is_covered)
        !! Check if a line is covered
        class(coverage_line_t), intent(in) :: this
        logical :: is_covered
        
        is_covered = this%is_covered .or. (this%execution_count > 0)
    end function line_is_covered

    subroutine branch_init(this, filename, line_number, branch_id, taken_count, not_taken_count)
        !! Initialize a coverage branch
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
        this%is_covered = (taken_count > 0) .or. (not_taken_count > 0)
        
        if ((taken_count + not_taken_count) > 0) then
            this%coverage_percentage = real(max(taken_count, not_taken_count)) / &
                                       real(taken_count + not_taken_count) * 100.0
        else
            this%coverage_percentage = 0.0
        end if
    end subroutine branch_init

    function branch_is_partially_covered(this) result(is_covered)
        !! Check if a branch is partially covered
        class(coverage_branch_t), intent(in) :: this
        logical :: is_covered
        
        is_covered = ((this%taken_count > 0) .and. (this%not_taken_count == 0)) .or. &
                     ((this%taken_count == 0) .and. (this%not_taken_count > 0))
    end function branch_is_partially_covered

    function branch_is_fully_covered(this) result(is_covered)
        !! Check if a branch is fully covered
        class(coverage_branch_t), intent(in) :: this
        logical :: is_covered
        
        is_covered = (this%taken_count > 0) .and. (this%not_taken_count > 0)
    end function branch_is_fully_covered

end module coverage_location_types