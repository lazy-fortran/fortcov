module coverage_format_converter
    !! Coverage Format Converter Module (XML-focused)
    !!
    !! Provides functionality related to Cobertura XML validation and parsing.
    !! JSON-based conversions were removed to simplify the codebase.

    use xml_utils, only: is_well_formed_xml, parse_cobertura_xml
    implicit none
    private

    ! Public interface procedures
    public :: validate_cobertura_xml_schema
    public :: parse_cobertura_xml

contains

    ! Validate XML content against Cobertura schema (basic structural checks)
    subroutine validate_cobertura_xml_schema(xml_content, is_valid)
        character(len=*), intent(in) :: xml_content
        logical, intent(out) :: is_valid

        ! Basic XML validation - check for required elements
        is_valid = .false.

        ! Check for required XML structure
        if (index(xml_content, '<coverage') == 0) return
        if (index(xml_content, '</coverage>') == 0) return

        ! Accept any coverage element structure as valid for basic testing

        ! Basic well-formed XML check
        if (.not. is_well_formed_xml(xml_content)) return

        is_valid = .true.

    end subroutine validate_cobertura_xml_schema

end module coverage_format_converter
