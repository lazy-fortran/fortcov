module coverage_reporter
    !! Consolidated Coverage Reporter Module (Base + Factory)
    !!
    !! This module consolidates reporter base interface and factory functionality
    !! to reduce module count from 9 to 2 (78% reduction).
    !! Combines: coverage_reporter_base, coverage_reporter_factory, coverage_reporter_core
    
    use coverage_types
    implicit none
    private
    
    ! Public exports
    public :: coverage_reporter_t
    public :: create_reporter
    public :: get_supported_formats
    
    ! ============================================================================
    ! Abstract Reporter Interface
    ! ============================================================================
    
    type, abstract :: coverage_reporter_t
    contains
        procedure(generate_report_interface), deferred :: generate_report
        procedure(get_format_name_interface), deferred :: get_format_name  
        procedure(supports_diff_interface), deferred :: supports_diff
    end type coverage_reporter_t
    
    ! ============================================================================
    ! Abstract Interfaces
    ! ============================================================================
    
    abstract interface
        subroutine generate_report_interface(this, coverage_data, output_path, &
                                           success, error_message, &
                                           diff_data, threshold)
            import :: coverage_reporter_t, coverage_data_t, coverage_diff_t
            class(coverage_reporter_t), intent(in) :: this
            type(coverage_data_t), intent(in) :: coverage_data
            character(len=*), intent(in) :: output_path
            logical, intent(out) :: success
            character(len=:), allocatable, intent(out) :: error_message
            type(coverage_diff_t), intent(in), optional :: diff_data
            real, intent(in), optional :: threshold
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
    
    ! ============================================================================
    ! Factory Functions
    ! ============================================================================
    
    subroutine create_reporter(format, reporter, error_flag)
        !! Factory function to create reporter based on format string
        character(len=*), intent(in) :: format
        class(coverage_reporter_t), allocatable, intent(out) :: reporter
        logical, intent(out) :: error_flag
        
        ! Delegate to implementation module factory
        call create_reporter_impl(format, reporter, error_flag)
    end subroutine create_reporter
    
    subroutine create_reporter_impl(format, reporter, error_flag)
        !! Implementation factory - will be overridden by coverage_reporter_impl
        use coverage_reporter_impl, only: text_reporter_t, markdown_reporter_t, &
                                         json_reporter_t, html_reporter_t, xml_reporter_t
        character(len=*), intent(in) :: format
        class(coverage_reporter_t), allocatable, intent(out) :: reporter
        logical, intent(out) :: error_flag
        
        error_flag = .false.
        
        select case (trim(format))
        case ("text")
            allocate(text_reporter_t :: reporter)
        case ("markdown", "md")
            allocate(markdown_reporter_t :: reporter)
        case ("json")
            allocate(json_reporter_t :: reporter)
        case ("html")
            allocate(html_reporter_t :: reporter)
        case ("xml")
            allocate(xml_reporter_t :: reporter)
        case default
            error_flag = .true.
        end select
    end subroutine create_reporter_impl
    
    function get_supported_formats() result(formats)
        !! Get list of supported output formats
        character(len=10), dimension(5) :: formats
        
        formats(1) = "text"
        formats(2) = "markdown"
        formats(3) = "json"
        formats(4) = "html"
        formats(5) = "xml"
    end function get_supported_formats
    
end module coverage_reporter