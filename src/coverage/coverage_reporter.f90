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
    ! Factory functions moved to coverage_reporter_factory module
    
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
    
    ! No implementation needed - this is a pure interface module
    ! Factory functions moved to coverage_reporter_factory module
    
end module coverage_reporter