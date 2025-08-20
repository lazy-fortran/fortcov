module coverage_engine
    !! Coverage Engine (Refactored for Architectural Decomposition)
    !! 
    !! This module now serves as a compatibility layer that delegates to 
    !! the new decomposed architecture while preserving the original public interface.
    !! 
    !! Original module size: 1,180 lines → Now: <100 lines
    !! Decomposed into: coverage_orchestrator + coverage_analysis + 
    !!                  coverage_workflows + coverage_integration
    use coverage_orchestrator
    implicit none
    private
    
    ! Re-export public interface from orchestrator for backward compatibility
    public :: analyze_coverage
    public :: analyze_coverage_diff
    public :: find_coverage_files
    public :: check_exclude_patterns
    public :: analyze_coverage_safe
    public :: validate_system_integration
    
    ! Re-export exit codes for interface compatibility
    integer, parameter, public :: EXIT_SUCCESS = 0
    integer, parameter, public :: EXIT_FAILURE = 1
    integer, parameter, public :: EXIT_THRESHOLD_NOT_MET = 2
    integer, parameter, public :: EXIT_NO_COVERAGE_DATA = 3
    
    ! Note: All implementation has been moved to specialized modules:
    ! - coverage_orchestrator.f90: Public interface and workflow coordination
    ! - coverage_analysis.f90: Core analysis algorithms and data processing
    ! - coverage_workflows.f90: File discovery, filtering, and workflow operations
    ! - coverage_integration.f90: System integration validation and testing
    !
    ! This architecture enables:
    ! - Better separation of concerns
    ! - Improved testability
    ! - Easier maintenance
    ! - Compliance with 400-line module size targets
    ! - Preserved backward compatibility
    
end module coverage_engine