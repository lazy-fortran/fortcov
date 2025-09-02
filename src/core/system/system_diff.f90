module system_diff
    !! System Diff Module - Coverage Utilities Orchestrator
    !!
    !! Provides a unified interface for Cobertura XML validation, parsing,
    !! data comparison, and coverage calculation utilities for comprehensive
    !! coverage data processing.
    !!
    !! JSON-based conversion APIs have been removed. This module focuses on
    !! XML-centric workflows only.

    use coverage_format_converter, only: validate_cobertura_xml_schema, &
                                        parse_cobertura_xml
    use coverage_data_comparator, only: compare_coverage_data, &
                                       validate_structural_equivalence, &
                                       check_numerical_tolerance
    use coverage_calculation_utils, only: calculate_coverage_rates, &
                                         calculate_branch_coverage_rate
    implicit none
    private

    ! Re-export public interface procedures for compatibility
    public :: validate_cobertura_xml_schema
    public :: compare_coverage_data
    public :: validate_structural_equivalence
    public :: check_numerical_tolerance
    public :: calculate_coverage_rates
    public :: calculate_branch_coverage_rate
    public :: parse_cobertura_xml

contains
    !! This module serves as a clean orchestration layer,
    !! delegating all functionality to specialized utility modules.

end module system_diff
