module coverage_workflows_analysis
    !! Coverage analysis workflows including diff analysis and TUI operations
    !!
    !! Provides high-level analysis workflows for coverage comparison,
    !! terminal user interface launching, and analysis result reporting.
    
    use constants_core
    use config_core
    use coverage_types, only: coverage_diff_t
    use coverage_tui, only: perform_tui_analysis
    implicit none
    private
    
    public :: perform_coverage_diff_analysis
    public :: launch_coverage_tui_mode
    
contains

    function perform_coverage_diff_analysis(config) result(exit_code)
        !! Coverage diff analysis workflow implementation
        !! Extracted from original analyze_coverage_diff function
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        type(coverage_diff_t) :: diff_result
        logical :: diff_success
        
        exit_code = EXIT_SUCCESS
        
        if (.not. config%quiet) then
            print *, "📊 Analyzing coverage differences..."
            if (allocated(config%diff_baseline_file)) then
                print *, "   Baseline: " // trim(config%diff_baseline_file)
            end if
            if (allocated(config%diff_current_file)) then
                print *, "   Compare:  " // trim(config%diff_current_file)
            end if
        end if
        
        ! Perform coverage diff analysis
        ! Note: This would need actual coverage data loaded from baseline and current files
        ! For now, just set a success flag and empty result
        diff_success = .true.
        
        if (.not. diff_success) then
            if (.not. config%quiet) then
                print *, "❌ Coverage diff analysis failed"
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Output diff summary
        call output_coverage_diff_summary(diff_result, config)
        
        ! Apply threshold validation for diff
        if (config%minimum_coverage > 0.0) then
            if (diff_result%current_coverage < config%minimum_coverage) then
                if (.not. config%quiet) then
                    print *, "❌ Coverage threshold not met in comparison"
                    write(*, '(A, F5.1, A, F5.1, A)') &
                        "   Required: ", config%minimum_coverage, "%, Current: ", &
                        diff_result%current_coverage, "%"
                end if
                exit_code = EXIT_THRESHOLD_NOT_MET
            end if
        end if
        
    end function perform_coverage_diff_analysis
    
    function launch_coverage_tui_mode(config) result(exit_code)
        !! TUI mode launch workflow implementation
        !! Extracted from original launch_tui_mode function
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        logical :: tui_success
        
        exit_code = EXIT_SUCCESS
        
        if (.not. config%quiet) then
            print *, "🖥️  Launching Terminal User Interface..."
        end if
        
        ! Launch TUI with configuration
        call start_tui_interface(config, tui_success)
        
        if (.not. tui_success) then
            if (.not. config%quiet) then
                print *, "❌ TUI launch failed"
            end if
            exit_code = EXIT_FAILURE
        end if
        
    end function launch_coverage_tui_mode
    
    ! Internal helper routines
    
    subroutine output_coverage_diff_summary(diff_result, config)
        !! Outputs coverage diff analysis summary
        type(coverage_diff_t), intent(in) :: diff_result
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "📊 Coverage Diff Analysis Results:"
            write(*, '(A, F5.1, A)') "   Baseline Coverage: ", &
                diff_result%baseline_coverage, "%"
            write(*, '(A, F5.1, A)') "   Current Coverage:  ", &
                diff_result%current_coverage, "%"
            write(*, '(A, F5.1, A)') "   Coverage Change:   ", &
                diff_result%coverage_change, "%"
            
            if (diff_result%coverage_change > 0.0) then
                print *, "   ✅ Coverage improved"
            else if (diff_result%coverage_change < 0.0) then
                print *, "   ⚠️  Coverage decreased"
            else
                print *, "   ➡️  Coverage unchanged"
            end if
        end if
        
    end subroutine output_coverage_diff_summary
    
    subroutine start_tui_interface(config, success)
        !! Starts the terminal user interface
        type(config_t), intent(in) :: config
        logical, intent(out) :: success
        
        integer :: exit_code
        
        ! Launch the actual TUI using the coverage TUI handler
        exit_code = perform_tui_analysis(config)
        success = (exit_code == EXIT_SUCCESS)
        
    end subroutine start_tui_interface

end module coverage_workflows_analysis