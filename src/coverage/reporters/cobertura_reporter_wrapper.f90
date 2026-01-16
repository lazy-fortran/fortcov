module cobertura_reporter_wrapper
    !! Cobertura XML reporter wrapper
    !!
    !! Provides coverage_reporter_t implementation that writes Cobertura XML.
    use coverage_types
    use coverage_reporter
    implicit none
    private

    public :: cobertura_reporter_t

    type, extends(coverage_reporter_t) :: cobertura_reporter_t
    contains
        procedure :: generate_report => cobertura_generate_report
        procedure :: get_format_name => cobertura_get_format_name
        procedure :: supports_diff => cobertura_supports_diff
    end type cobertura_reporter_t

contains

    subroutine cobertura_generate_report(this, coverage_data, output_path, &
                                         success, error_message, &
                                         diff_data, threshold)
        use cobertura_reporter, only: generate_cobertura_xml_report
        use error_handling_core, only: error_context_t, ERROR_SUCCESS
        use file_utilities, only: write_text_file_safe
        class(cobertura_reporter_t), intent(in) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message
        type(coverage_diff_t), intent(in), optional :: diff_data
        real, intent(in), optional :: threshold

        character(len=:), allocatable :: xml_content
        type(error_context_t) :: err_ctx

        xml_content = generate_cobertura_xml_report(coverage_data)

        call write_text_file_safe(output_path, xml_content, err_ctx)
        if (err_ctx%error_code /= ERROR_SUCCESS) then
            success = .false.
            error_message = "Failed to write Cobertura XML: " // trim(output_path)
        else
            success = .true.
            error_message = ""
        end if
    end subroutine cobertura_generate_report

    function cobertura_get_format_name(this) result(format_name)
        class(cobertura_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        format_name = "cobertura"
    end function cobertura_get_format_name

    function cobertura_supports_diff(this) result(supported)
        class(cobertura_reporter_t), intent(in) :: this
        logical :: supported
        supported = .false.
    end function cobertura_supports_diff

end module cobertura_reporter_wrapper
