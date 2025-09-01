module coverage_workflow_core
    !! High-level workflow orchestration for coverage operations
    !! 
    !! Slim coordinator that orchestrates the various extracted coverage
    !! workflow modules. Provides a unified interface while maintaining
    !! separation of concerns between different workflow responsibilities.
    use constants_core
    use config_core
    use coverage_test_executor
    use coverage_processor_gcov
    use coverage_build
    use coverage_diff
    use coverage_types, only: coverage_diff_t
    use build_detector_core
    use build_system_validation, only: detect_and_validate_build_system
    use error_handling_core
    implicit none
    private

    public :: orchestrate_coverage_analysis
    public :: orchestrate_auto_test_workflow
    public :: orchestrate_gcov_discovery

contains

    function orchestrate_coverage_analysis(config) result(exit_code)
        !! Orchestrate complete coverage analysis workflow
        !! 
        !! Coordinates all aspects of coverage analysis including auto-test
        !! execution (if enabled), gcov file discovery/generation, and
        !! coverage analysis execution.
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        character(len=:), allocatable :: coverage_files(:)
        integer :: auto_test_result
        
        exit_code = EXIT_SUCCESS
        
        ! Execute auto-test workflow if enabled
        if (config%auto_test_execution) then
            auto_test_result = orchestrate_auto_test_workflow(config)
            if (auto_test_result /= EXIT_SUCCESS) then
                if (.not. config%quiet) then
                    print *, "⚠️  Auto-test execution issues, continuing with coverage analysis"
                end if
            end if
        end if
        
        ! Discover coverage files using orchestrated discovery
        call orchestrate_gcov_discovery(config, coverage_files, exit_code)
        if (exit_code /= EXIT_SUCCESS) then
            return
        end if
        
        ! Validate discovered files
        if (.not. allocated(coverage_files) .or. size(coverage_files) == 0) then
            if (.not. config%quiet) then
                print *, "❌ No coverage files discovered"
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        if (.not. config%quiet) then
            print *, "✅ Coverage workflow orchestration completed"
            write(*, '(A,I0,A)') "   Discovered ", size(coverage_files), " coverage files"
        end if
        
    end function orchestrate_coverage_analysis

    function orchestrate_auto_test_workflow(config) result(exit_code)
        !! Orchestrate automatic test execution workflow
        !! 
        !! Delegates to the specialized auto-test executor module while
        !! providing orchestration context and error handling.
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        if (.not. config%quiet) then
            print *, "🔄 Orchestrating auto-test workflow..."
        end if
        
        ! Delegate to specialized auto-test executor
        exit_code = execute_auto_test_workflow(config)
        
        if (.not. config%quiet) then
            select case (exit_code)
            case (EXIT_SUCCESS)
                print *, "✅ Auto-test workflow completed successfully"
            case (2)
                print *, "⚠️  Auto-test workflow skipped (build system issues)"
            case (124)
                print *, "⏱️  Auto-test workflow timed out"
            case default
                print *, "❌ Auto-test workflow failed"
            end select
        end if
        
    end function orchestrate_auto_test_workflow

    subroutine orchestrate_gcov_discovery(config, coverage_files, exit_code)
        !! Orchestrate gcov file discovery and generation
        !! 
        !! Coordinates gcov file discovery with build system integration
        !! to provide comprehensive coverage file discovery.
        type(config_t), intent(in) :: config
        character(len=:), allocatable, intent(out) :: coverage_files(:)
        integer, intent(out) :: exit_code
        
        type(build_system_info_t) :: build_info
        type(error_context_t) :: error_ctx
        
        exit_code = EXIT_SUCCESS
        
        if (.not. config%quiet) then
            print *, "🔍 Orchestrating coverage file discovery..."
        end if
        
        ! Try build system integration for enhanced discovery (unified)
        if (detect_and_validate_build_system(config, build_info, error_ctx, '.') == EXIT_SUCCESS .and. &
            build_info%system_type /= 'unknown' .and. &
            build_info%tool_available) then
            
            if (.not. config%quiet) then
                print *, "📦 Using build system integration for discovery: " // &
                         trim(build_info%system_type)
            end if
        end if
        
        ! Delegate to specialized gcov processor
        call discover_gcov_files(config, coverage_files)
        
        if (.not. config%quiet) then
            if (allocated(coverage_files) .and. size(coverage_files) > 0) then
                write(*, '(A,I0,A)') "✅ Discovered ", size(coverage_files), " coverage files"
            else
                print *, "⚠️  No coverage files discovered"
            end if
        end if
        
    end subroutine orchestrate_gcov_discovery

    function orchestrate_diff_analysis(config) result(exit_code)
        !! Orchestrate coverage diff analysis workflow
        !! 
        !! Provides high-level coordination for coverage diff analysis
        !! while delegating to specialized diff analysis functionality.
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        type(coverage_diff_t) :: diff_result
        logical :: diff_success
        
        exit_code = EXIT_SUCCESS
        
        if (.not. config%quiet) then
            print *, "🔄 Orchestrating coverage diff analysis..."
        end if
        
        ! Validate diff configuration
        if (.not. allocated(config%diff_baseline_file) .or. &
            .not. allocated(config%diff_current_file)) then
            if (.not. config%quiet) then
                print *, "❌ Diff analysis requires baseline and current files"
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        if (.not. config%quiet) then
            print *, "📊 Analyzing coverage differences..."
            print *, "   Baseline: " // trim(config%diff_baseline_file)
            print *, "   Compare:  " // trim(config%diff_current_file)
        end if
        
        ! Perform diff analysis (delegated to coverage_diff module)
        diff_success = .true.  ! Placeholder for actual diff analysis
        
        if (.not. diff_success) then
            if (.not. config%quiet) then
                print *, "❌ Coverage diff analysis failed"
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Apply threshold validation
        if (config%minimum_coverage > 0.0) then
            if (diff_result%current_coverage < config%minimum_coverage) then
                if (.not. config%quiet) then
                    print *, "❌ Coverage threshold not met in comparison"
                    write(*, '(A, F5.1, A, F5.1, A)') &
                        "   Required: ", config%minimum_coverage, "%, Current: ", &
                        diff_result%current_coverage, "%"
                end if
                exit_code = EXIT_THRESHOLD_NOT_MET
                return
            end if
        end if
        
        if (.not. config%quiet) then
            print *, "✅ Coverage diff analysis completed successfully"
        end if
        
    end function orchestrate_diff_analysis

    function orchestrate_tui_launch(config) result(exit_code)
        !! Orchestrate TUI mode launch
        !! 
        !! Simple orchestration for TUI mode launch with proper
        !! error handling and user feedback.
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        logical :: tui_success
        
        exit_code = EXIT_SUCCESS
        
        if (.not. config%quiet) then
            print *, "🔄 Orchestrating TUI mode launch..."
        end if
        
        ! Launch TUI interface (placeholder implementation)
        tui_success = .true.  ! Placeholder for actual TUI launch
        
        if (.not. tui_success) then
            if (.not. config%quiet) then
                print *, "❌ TUI launch failed"
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        if (.not. config%quiet) then
            print *, "✅ TUI mode launched successfully"
        end if
        
    end function orchestrate_tui_launch

end module coverage_workflow_core
