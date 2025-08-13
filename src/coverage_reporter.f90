module coverage_reporter
    use coverage_model
    implicit none
    private
    
    ! Public types
    public :: coverage_reporter_t
    public :: markdown_reporter_t
    public :: mock_reporter_t
    
    ! Public procedures
    public :: create_reporter
    
    ! Abstract reporter interface
    type, abstract :: coverage_reporter_t
    contains
        procedure(generate_report_interface), deferred :: generate_report
        procedure(get_format_name_interface), deferred :: get_format_name
        procedure(supports_diff_interface), deferred :: supports_diff
    end type coverage_reporter_t
    
    ! Concrete markdown reporter implementation
    type, extends(coverage_reporter_t) :: markdown_reporter_t
    contains
        procedure :: generate_report => markdown_generate_report
        procedure :: get_format_name => markdown_get_format_name
        procedure :: supports_diff => markdown_supports_diff
    end type markdown_reporter_t
    
    ! Mock reporter for testing
    type, extends(coverage_reporter_t) :: mock_reporter_t
        logical :: was_called = .false.
        type(coverage_data_t) :: captured_data
        character(len=:), allocatable :: captured_output_path
    contains
        procedure :: generate_report => mock_generate_report
        procedure :: get_format_name => mock_get_format_name
        procedure :: supports_diff => mock_supports_diff
    end type mock_reporter_t
    
    ! Abstract interfaces
    abstract interface
        subroutine generate_report_interface(this, coverage_data, output_path, &
                                            error_flag)
            import :: coverage_reporter_t, coverage_data_t
            class(coverage_reporter_t), intent(inout) :: this
            type(coverage_data_t), intent(in) :: coverage_data
            character(len=*), intent(in) :: output_path
            logical, intent(out) :: error_flag
        end subroutine generate_report_interface
        
        function get_format_name_interface(this) result(format_name)
            import :: coverage_reporter_t
            class(coverage_reporter_t), intent(in) :: this
            character(len=:), allocatable :: format_name
        end function get_format_name_interface
        
        function supports_diff_interface(this) result(supported)
            import :: coverage_reporter_t
            class(coverage_reporter_t), intent(in) :: this
            logical :: supported
        end function supports_diff_interface
    end interface

contains

    ! Factory function to create reporter based on format string
    subroutine create_reporter(format, reporter, error_flag)
        character(len=*), intent(in) :: format
        class(coverage_reporter_t), allocatable, intent(out) :: reporter
        logical, intent(out) :: error_flag
        
        error_flag = .false.
        
        ! Select reporter based on format
        select case (trim(format))
        case ("markdown", "md")
            allocate(markdown_reporter_t :: reporter)
        case ("mock")
            allocate(mock_reporter_t :: reporter)
        case default
            error_flag = .true.
        end select
    end subroutine create_reporter

    ! Markdown reporter implementations
    subroutine markdown_generate_report(this, coverage_data, output_path, &
                                       error_flag)
        class(markdown_reporter_t), intent(inout) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: error_flag
        integer :: unit, stat, i
        integer :: total_lines, covered_lines
        logical :: use_stdout
        
        error_flag = .false.
        use_stdout = (trim(output_path) == "-")
        
        ! Open output stream
        if (use_stdout) then
            unit = 6  ! Standard output
        else
            open(newunit=unit, file=output_path, status='replace', iostat=stat)
            if (stat /= 0) then
                error_flag = .true.
                return
            end if
        end if
        
        ! Write basic markdown report
        write(unit, '(A)') "# Coverage Report"
        write(unit, '(A)') ""
        write(unit, '(A)') "| Filename | Lines | Covered | Percentage |"
        write(unit, '(A)') "|----------|-------|---------|------------|"
        
        do i = 1, size(coverage_data%files)
            associate(file => coverage_data%files(i))
                ! Use model methods to avoid DRY violation
                total_lines = file%get_executable_line_count()
                covered_lines = file%get_covered_line_count()
                write(unit, '(A,A,A,I0,A,I0,A,F5.1,A)') &
                    "| ", trim(file%filename), " | ", &
                    total_lines, " | ", covered_lines, " | ", &
                    file%get_line_coverage_percentage(), "% |"
            end associate
        end do
        
        if (.not. use_stdout) close(unit)
        
        ! Suppress unused variable warning
        associate(dummy => this)
        end associate
    end subroutine markdown_generate_report

    function markdown_get_format_name(this) result(format_name)
        class(markdown_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        
        format_name = "markdown"
        
        ! Suppress unused variable warning
        associate(dummy => this)
        end associate
    end function markdown_get_format_name

    function markdown_supports_diff(this) result(supported)
        class(markdown_reporter_t), intent(in) :: this
        logical :: supported
        
        ! Markdown reporter will support diff in the future
        supported = .false.
        
        ! Suppress unused variable warning
        associate(dummy => this)
        end associate
    end function markdown_supports_diff

    ! Mock reporter implementations
    subroutine mock_generate_report(this, coverage_data, output_path, error_flag)
        class(mock_reporter_t), intent(inout) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: error_flag
        
        error_flag = .false.
        this%was_called = .true.
        this%captured_data = coverage_data
        this%captured_output_path = output_path
    end subroutine mock_generate_report

    function mock_get_format_name(this) result(format_name)
        class(mock_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        
        format_name = "mock"
        
        ! Suppress unused variable warning
        associate(dummy => this)
        end associate
    end function mock_get_format_name

    function mock_supports_diff(this) result(supported)
        class(mock_reporter_t), intent(in) :: this
        logical :: supported
        
        ! Mock reporter can simulate any capability
        supported = .true.
        
        ! Suppress unused variable warning
        associate(dummy => this)
        end associate
    end function mock_supports_diff

end module coverage_reporter