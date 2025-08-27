module coverage_reporter_factory
    !! Coverage Reporter Factory
    !! 
    !! Extracted from coverage_reporter_impl for Issue #182 module size compliance.
    !! Handles creation of different reporter types based on format string.
    use coverage_reporter_base
    use coverage_reporter_md_impl
    use coverage_reporter_json_impl
    use coverage_reporter_xml_impl
    implicit none
    private
    
    public :: create_reporter
    
contains

    ! Factory function to create reporter based on format string
    subroutine create_reporter(format, reporter, error_flag)
        character(len=*), intent(in) :: format
        class(coverage_reporter_t), allocatable, intent(out) :: reporter
        logical, intent(out) :: error_flag
        
        error_flag = .false.
        
        select case (trim(format))
        case ("markdown", "md")
            allocate(markdown_reporter_t :: reporter)
        case ("json")
            allocate(json_reporter_t :: reporter)
        case ("xml")
            allocate(xml_reporter_t :: reporter)
        case default
            error_flag = .true.
            ! Reporter remains unallocated to indicate error
        end select
        
    end subroutine create_reporter

end module coverage_reporter_factory