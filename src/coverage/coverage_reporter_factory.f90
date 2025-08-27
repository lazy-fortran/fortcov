module coverage_reporter_factory
    !! Coverage Reporter Factory Module
    !!
    !! This module provides the factory function to create concrete reporter instances
    !! Separated from coverage_reporter to avoid circular dependencies
    
    use coverage_reporter, only: coverage_reporter_t
    implicit none
    private
    
    public :: create_reporter
    public :: get_supported_formats
    
contains
    
    subroutine create_reporter(format, reporter, error_flag)
        !! Create a concrete reporter instance based on format
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
    end subroutine create_reporter
    
    function get_supported_formats() result(formats)
        !! Get list of supported output formats
        character(len=10), dimension(5) :: formats
        
        formats(1) = "text"
        formats(2) = "markdown"
        formats(3) = "json"
        formats(4) = "html"
        formats(5) = "xml"
    end function get_supported_formats
    
end module coverage_reporter_factory