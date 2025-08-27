module coverage_reporter_base
    !! Coverage Reporter Base Interface
    !! 
    !! Extracted from coverage_reporter_impl for Issue #182 module size compliance.
    !! Defines the abstract interface that all reporters must implement.
    use coverage_model_core
    implicit none
    private
    
    public :: coverage_reporter_t
    
    ! Abstract reporter interface
    type, abstract :: coverage_reporter_t
    contains
        procedure(generate_report_interface), deferred :: generate_report
        procedure(get_format_name_interface), deferred :: get_format_name  
        procedure(supports_diff_interface), deferred :: supports_diff
    end type coverage_reporter_t

    abstract interface
        subroutine generate_report_interface(this, coverage_data, output_path, &
                                           & success, error_message, &
                                           & diff_data, threshold)
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

end module coverage_reporter_base