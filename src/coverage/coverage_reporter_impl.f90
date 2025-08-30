module coverage_reporter_impl
    !! Coverage Reporter Implementations - Backward Compatibility Layer
    !!
    !! Decomposed for SRP compliance (Issue #718 proactive size management).
    !! Re-exports all reporter types from specialized modules to maintain
    !! backward compatibility with existing code.
    !!
    !! Original size: 439 lines -> Now: ~40 lines
    !! Implementation moved to specialized modules:
    !! - text_reporter.f90
    !! - json_reporter.f90
    !! - html_reporter.f90
    !! - xml_reporter.f90
    !! - markdown_reporter_wrapper.f90
    use text_reporter
    use json_reporter
    use html_reporter_impl
    use xml_reporter
    use markdown_reporter_wrapper
    implicit none
    
    ! Re-export all reporter types for backward compatibility
    public :: text_reporter_t
    public :: markdown_reporter_t
    public :: json_reporter_t
    public :: html_reporter_t
    public :: xml_reporter_t

end module coverage_reporter_impl