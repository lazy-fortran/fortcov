module coverage_data_types
    !! Coverage Data Container Type Module
    !!
    !! Provides the main coverage data container type and operations.
    !! Extracted from coverage_types to maintain SRP and size limits.
    
    use constants_core
    use coverage_file_types
    use coverage_location_types, only: file_coverage_t
    
    implicit none
    private
    
    public :: coverage_data_t
    
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
    
contains

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

end module coverage_data_types