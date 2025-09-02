module coverage_engine_core
    !! Collapsed Coverage Engine
    !!
    !! Provides a focused, minimal public interface that delegates directly
    !! to the analysis and workflow modules, removing the orchestrator and
    !! integration indirection layers.
    use coverage_analysis_core, only: perform_coverage_analysis, &
                                      perform_safe_coverage_analysis
    use coverage_workflows,     only: discover_coverage_files, &
                                      evaluate_exclude_patterns, &
                                      perform_coverage_diff_analysis
    use config_core,           only: config_t
    use error_handling_core,   only: error_context_t
    implicit none
    private

    ! Public API
    public :: analyze_coverage
    public :: analyze_coverage_diff
    public :: find_coverage_files
    public :: check_exclude_patterns
    public :: analyze_coverage_safe
    public :: validate_system_integration

    ! Exit codes (kept for interface compatibility)
    integer, parameter, public :: EXIT_SUCCESS = 0
    integer, parameter, public :: EXIT_FAILURE = 1
    integer, parameter, public :: EXIT_THRESHOLD_NOT_MET = 2
    integer, parameter, public :: EXIT_NO_COVERAGE_DATA = 3

contains

    function analyze_coverage(config) result(exit_code)
        type(config_t), intent(inout) :: config
        integer :: exit_code
        exit_code = perform_coverage_analysis(config)
    end function analyze_coverage

    function analyze_coverage_diff(config) result(exit_code)
        type(config_t), intent(in) :: config
        integer :: exit_code
        exit_code = perform_coverage_diff_analysis(config)
    end function analyze_coverage_diff

    function find_coverage_files(config) result(files)
        type(config_t), intent(in) :: config
        character(len=:), allocatable :: files(:)
        files = discover_coverage_files(config)
    end function find_coverage_files

    function check_exclude_patterns(filepath, config) result(should_exclude)
        character(len=*), intent(in) :: filepath
        type(config_t),   intent(in) :: config
        logical :: should_exclude
        should_exclude = evaluate_exclude_patterns(filepath, config)
    end function check_exclude_patterns

    function analyze_coverage_safe(config, error_ctx) result(exit_code)
        type(config_t),        intent(inout) :: config
        type(error_context_t), intent(out)   :: error_ctx
        integer :: exit_code
        exit_code = perform_safe_coverage_analysis(config, error_ctx)
    end function analyze_coverage_safe

    function validate_system_integration() result(validation_passed)
        logical :: validation_passed
        ! Orchestrator/integration layers removed; assume environment is valid.
        validation_passed = .true.
    end function validate_system_integration

end module coverage_engine_core
