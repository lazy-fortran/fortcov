module report_engine_terminal
    !! Report Engine Terminal UI
    !! 
    !! Handles terminal UI and browser functionality.
    !! Extracted from report_engine.f90 for SRP compliance (Issue #718).
    use coverage_model_core
    use data_transformer_core
    use data_transformer_types
    use theme_manager_core
    use syntax_highlighter
    use report_config_core
    use tui_manager_core
    implicit none
    private
    
    public :: launch_terminal_browser_session
    public :: generate_styled_terminal_output

contains

    ! Launch terminal browser session
    subroutine launch_terminal_browser_session(source_data, transformer, &
                                              theme_manager, config, session, &
                                              interactive, success, error_msg)
        type(coverage_data_t), intent(in) :: source_data
        type(data_transformer_t), intent(inout) :: transformer
        type(theme_manager_t), intent(inout) :: theme_manager
        type(report_config_t), intent(in) :: config
        type(terminal_session_t), intent(out) :: session
        logical, intent(in) :: interactive
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        type(transformed_data_t) :: transformed_data
        type(color_scheme_t) :: theme
        character(len=:), allocatable :: terminal_output
        
        success = .false.
        error_msg = ""
        
        call session%init()
        
        ! Transform data for terminal display
        call transformer%transform_data(source_data, transformed_data, &
                                       success, error_msg)
        if (.not. success) return
        
        ! Load theme for terminal
        call theme_manager%load_cyberpunk_theme(theme, success, error_msg)
        if (.not. success) return
        
        ! Generate terminal-friendly output
        call generate_terminal_display(transformed_data, theme, terminal_output)
        
        session%is_active = .true.
        session%colors_enabled = config%terminal_colors_enabled
        session%display_buffer = terminal_output
        
        ! For non-interactive mode (testing), we don't start the actual TUI
        if (.not. interactive) then
            success = .true.
            return
        end if
        
        ! In interactive mode, start the full TUI with proper loop control
        call start_interactive_tui(session, terminal_output, success, error_msg)
    end subroutine launch_terminal_browser_session
    
    ! Generate styled terminal output
    subroutine generate_styled_terminal_output(source_content, highlighter, &
                                              output, success, error_msg)
        character(len=*), intent(in) :: source_content
        type(syntax_highlighter_t), intent(inout) :: highlighter
        character(len=:), allocatable, intent(out) :: output
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        success = .false.
        error_msg = ""
        
        ! Apply terminal syntax highlighting
        call highlighter%highlight_for_terminal(source_content, &
                                               output, success)
        if (.not. success) then
            error_msg = "Failed to generate terminal highlighting"
            return
        end if
        
        success = .true.
    end subroutine generate_styled_terminal_output

end module report_engine_terminal