module system_diff
    !! System Diff Module - Coverage Format Conversion Orchestrator
    !!
    !! Provides a unified interface for coverage data format conversions
    !! and validation. Orchestrates interactions between format converters,
    !! data comparators, and calculation utilities for comprehensive
    !! coverage data processing.
    !!
    !! Key Features:
    !! - Orchestrated format conversion between JSON and Cobertura XML
    !! - Comprehensive data validation and comparison
    !! - Coverage calculation and analysis coordination
    !! - Clean separation of concerns with focused utility modules
    
    use coverage_format_converter, only: convert_json_to_cobertura_xml, &
                                        convert_cobertura_xml_to_json, &
                                        validate_cobertura_xml_schema, &
                                        perform_round_trip_conversion, &
                                        parse_cobertura_xml
    use coverage_data_comparator, only: compare_coverage_data, &
                                       validate_structural_equivalence, &
                                       check_numerical_tolerance
    use coverage_calculation_utils, only: calculate_coverage_rates, &
                                         calculate_branch_coverage_rate
    implicit none
    private
    
    ! Re-export public interface procedures for compatibility
    public :: convert_json_to_cobertura_xml
    public :: convert_cobertura_xml_to_json
    public :: validate_cobertura_xml_schema
    public :: perform_round_trip_conversion
    public :: compare_coverage_data
    public :: validate_structural_equivalence
    public :: check_numerical_tolerance
    public :: calculate_coverage_rates
    public :: calculate_branch_coverage_rate
    public :: parse_cobertura_xml
    
contains
    !! This module now serves as a clean orchestration layer,
    !! delegating all functionality to specialized utility modules
    !! while maintaining backward compatibility with existing interfaces.
    

end module system_diff
