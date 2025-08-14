module coverage_reporter
    use coverage_model
    use markdown_reporter, only: generate_markdown_report, markdown_report_options_t
    implicit none
    private
    
    ! DRY helper for unused parameter warnings
    interface suppress_unused_warning
        module procedure :: suppress_unused_warning_reporter
    end interface suppress_unused_warning
    
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
        character(len=:), allocatable :: last_error
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
        integer :: unit, stat
        logical :: use_stdout
        character(len=:), allocatable :: report_content
        type(markdown_report_options_t) :: options
        
        error_flag = .false.
        use_stdout = (trim(output_path) == "-")
        
        ! Configure report options
        call options%init()
        
        ! Generate report using proper markdown reporter
        report_content = generate_markdown_report(coverage_data, options)
        
        ! Open output stream
        if (use_stdout) then
            unit = 6  ! Standard output
        else
            open(newunit=unit, file=output_path, status='replace', iostat=stat)
            if (stat /= 0) then
                error_flag = .true.
                ! Store error context in reporter instance for later retrieval
                this%last_error = "Cannot write to output file '" // &
                                 trim(output_path) // "' (iostat=" // &
                                 trim(int_to_string(stat)) // ")"
                return
            end if
        end if
        
        ! Write the complete markdown report
        write(unit, '(A)') "# Coverage Report"
        write(unit, '(A)') ""
        write(unit, '(A)') report_content
        
        if (.not. use_stdout) close(unit)
        
        call suppress_unused_warning(this)
    end subroutine markdown_generate_report

    function markdown_get_format_name(this) result(format_name)
        class(markdown_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        
        format_name = "markdown"
        
        call suppress_unused_warning(this)
    end function markdown_get_format_name

    function markdown_supports_diff(this) result(supported)
        class(markdown_reporter_t), intent(in) :: this
        logical :: supported
        
        ! Markdown reporter will support diff in the future
        supported = .false.
        
        call suppress_unused_warning(this)
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
        
        call suppress_unused_warning(this)
    end function mock_get_format_name

    function mock_supports_diff(this) result(supported)
        class(mock_reporter_t), intent(in) :: this
        logical :: supported
        
        ! Mock reporter can simulate any capability
        supported = .true.
        
        call suppress_unused_warning(this)
    end function mock_supports_diff

    ! DRY helper to suppress unused parameter warnings
    subroutine suppress_unused_warning_reporter(reporter)
        class(coverage_reporter_t), intent(in) :: reporter
        
        ! Use associate construct to avoid unused parameter warning
        associate(dummy => reporter)
        end associate
    end subroutine suppress_unused_warning_reporter

    ! Helper function to convert integer to string
    function int_to_string(value) result(str)
        integer, intent(in) :: value
        character(len=:), allocatable :: str
        character(len=20) :: temp
        
        write(temp, '(I0)') value
        str = trim(temp)
    end function int_to_string

end module coverage_reporter