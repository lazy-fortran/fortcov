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
        use markdown_reporter_wrapper, only: markdown_reporter_t
        character(len=*), intent(in) :: format
        class(coverage_reporter_t), allocatable, intent(out) :: reporter
        logical, intent(out) :: error_flag
        
        error_flag = .false.
        
        select case (trim(format))
        case ("markdown", "md")
            allocate(markdown_reporter_t :: reporter)
        case default
            error_flag = .true.
        end select
    end subroutine create_reporter
    
    function get_supported_formats() result(formats)
        !! Get list of supported output formats
        character(len=10), dimension(1) :: formats
        formats(1) = "markdown"
    end function get_supported_formats
    
end module coverage_reporter_factory
