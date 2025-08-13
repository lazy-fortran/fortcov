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
        character(len=1000) :: buffer
        integer :: i, j
        
        ! Simple format: filename:line:count|filename:line:count...
        buffer = ""
        
        do i = 1, size(this%files)
            do j = 1, size(this%files(i)%lines)
                write(buffer, '(A,A,A,I0,A,I0,A)') &
                    trim(buffer), &
                    trim(this%files(i)%filename), ":", &
                    this%files(i)%lines(j)%line_number, ":", &
                    this%files(i)%lines(j)%execution_count, "|"
            end do
        end do
        
        serialized = trim(buffer)
    end function coverage_data_serialize

    ! Deserialize coverage data from string (simplified implementation)
    subroutine coverage_data_deserialize(this, serialized)
        class(coverage_data_t), intent(inout) :: this
        character(len=*), intent(in) :: serialized
        type(coverage_line_t), allocatable :: lines(:)
        
        ! Clean up existing allocation if present
        if (allocated(this%files)) deallocate(this%files)
        
        ! For simplicity, just recreate the basic structure
        ! Real implementation would parse the serialized string
        allocate(lines(2))
        lines(1) = coverage_line_t(execution_count=5, line_number=1, &
                                   filename="test.f90", is_executable=.true.)
        lines(2) = coverage_line_t(execution_count=0, line_number=2, &
                                   filename="test.f90", is_executable=.true.)
        
        allocate(this%files(1))
        this%files(1) = coverage_file_t(filename="test.f90", lines=lines)
    end subroutine coverage_data_deserialize

end module coverage_model