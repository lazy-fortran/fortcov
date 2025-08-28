module xml_utils
    !! XML Utilities Orchestrator - refactored for SRP compliance
    !! 
    !! Coordinates XML processing by delegating to specialized modules.
    !! Decomposed from 460 lines for Issue #718 size management.
    use coverage_model_core
    use xml_generator_core
    use xml_parser_core
    use xml_utils_core
    use xml_attribute_parser
    use string_utils, only: int_to_string, real_to_string
    implicit none
    private
    
    ! Re-export public interface from specialized modules
    public :: generate_sources_section
    public :: generate_packages_section
    public :: calculate_file_line_rate
    public :: parse_cobertura_xml
    public :: count_xml_elements
    public :: parse_classes_from_xml
    public :: extract_filename_from_class
    public :: parse_lines_from_class
    public :: extract_line_attributes
    public :: get_current_timestamp
    public :: int_to_string
    public :: get_directory_path
    public :: get_base_name
    public :: is_well_formed_xml
    
    ! Note: All implementation delegated to specialized modules:
    ! - xml_generator_core: XML generation functions (includes calculate_file_line_rate)
    ! - xml_parser_core: XML parsing functions  
    ! - xml_utils_core: Utility and helper functions (timestamp, paths)
    ! - xml_attribute_parser: XML attribute parsing functions
    !
    ! This architecture provides:
    ! - Better separation of concerns (generation vs parsing vs utilities)
    ! - Improved maintainability (smaller, focused modules)
    ! - Easier testing (isolated functional units)
    ! - Compliance with module size targets
    ! - Preserved backward compatibility
    
end module xml_utils