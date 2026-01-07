module coverage_engine_core
    !! Collapsed Coverage Engine
    !!
    !! Provides a focused, minimal public interface that delegates directly
    !! to the analysis and workflow modules, removing the orchestrator and
    !! integration indirection layers.
    use build_detector_core,   only: build_system_info_t, detect_build_system
    use coverage_analysis_core, only: perform_coverage_analysis, &
                                      perform_safe_coverage_analysis
    use coverage_workflows,     only: discover_coverage_files, &
                                      evaluate_exclude_patterns, &
                                      perform_coverage_diff_analysis
    use config_core,            only: config_t
    use error_handling_core,    only: error_context_t, ERROR_MISSING_FILE, &
                                      ERROR_SUCCESS, clear_error_context, &
                                      handle_gcov_not_found, &
                                      handle_no_coverage_files, log_error, &
                                      safe_write_message, safe_write_suggestion
    use gcov_file_discovery,   only: discover_gcda_files, discover_gcov_files
    use gcov_generator,        only: check_gcov_availability
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

    subroutine validate_build_tools(build_ok)
        logical, intent(out) :: build_ok
        type(build_system_info_t) :: build_info
        type(error_context_t) :: error_ctx

        build_ok = .true.
        call detect_build_system(".", build_info, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call log_error(error_ctx)
            build_ok = .false.
            return
        end if

        if (trim(build_info%system_type) /= "unknown" .and. &
            .not. build_info%tool_available) then
            call clear_error_context(error_ctx)
            error_ctx%error_code = ERROR_MISSING_FILE
            call safe_write_message(error_ctx, &
                "Build tool not found for detected system: " // &
                trim(build_info%system_type))
            call safe_write_suggestion(error_ctx, &
                "Install the build tool or update PATH for this system")
            call log_error(error_ctx)
            build_ok = .false.
        end if
    end subroutine validate_build_tools

    subroutine validate_gcov_tool(gcov_ok)
        logical, intent(out) :: gcov_ok
        logical :: gcov_available
        type(error_context_t) :: error_ctx

        call check_gcov_availability(gcov_available)
        gcov_ok = gcov_available
        if (.not. gcov_available) then
            call handle_gcov_not_found("gcov", error_ctx)
            call log_error(error_ctx)
        end if
    end subroutine validate_gcov_tool

    subroutine validate_coverage_files(coverage_ok)
        logical, intent(out) :: coverage_ok
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=:), allocatable :: gcda_files(:)

        coverage_ok = .true.
        call discover_gcov_files(".", gcov_files, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call log_error(error_ctx)
            coverage_ok = .false.
            return
        end if

        if (allocated(gcov_files)) then
            if (size(gcov_files) > 0) return
        end if

        call discover_gcda_files(".", gcda_files, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call log_error(error_ctx)
            coverage_ok = .false.
            return
        end if

        if (.not. allocated(gcda_files)) then
            call handle_no_coverage_files(".", error_ctx)
            call log_error(error_ctx)
            coverage_ok = .false.
            return
        end if

        if (size(gcda_files) == 0) then
            call handle_no_coverage_files(".", error_ctx)
            call log_error(error_ctx)
            coverage_ok = .false.
        end if
    end subroutine validate_coverage_files

    function validate_system_integration() result(validation_passed)
        logical :: validation_passed
        logical :: build_ok
        logical :: gcov_ok
        logical :: coverage_ok

        call validate_build_tools(build_ok)
        call validate_gcov_tool(gcov_ok)
        call validate_coverage_files(coverage_ok)

        validation_passed = build_ok .and. gcov_ok .and. coverage_ok
    end function validate_system_integration

end module coverage_engine_core
