module tui_launcher_core
    !! TUI Mode Launcher
    !! 
    !! Handles launching of Terminal User Interface mode.
    !! Extracted from coverage_workflows.f90 for SRP compliance (Issue #718).
    use constants_core
    use config_core
    implicit none
    private
    
    public :: launch_coverage_tui_mode, start_tui_interface

contains

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
    
    subroutine start_tui_interface(config, success)
        !! Starts the terminal user interface
        use coverage_tui, only: perform_tui_analysis
        type(config_t), intent(in) :: config
        logical, intent(out) :: success
        
        integer :: exit_code
        
        ! Launch the actual TUI using the coverage TUI handler
        exit_code = perform_tui_analysis(config)
        success = (exit_code == EXIT_SUCCESS)
        
    end subroutine start_tui_interface

end module tui_launcher_core