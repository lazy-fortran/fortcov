module report_generator_core
    !! Report Generator - Handles report generation logic
    !! 
    !! Extracted from report_engine.f90 for SRP compliance (Issue #718).
    !! Responsible for generating HTML and terminal reports.
    use coverage_model_core
    use data_transformer_core
    use data_transformer_types
    use theme_manager_core
    use report_config_core
    use html_reporter
    use tui_manager_core
    implicit none
    private
    
    public :: report_generator_t
    public :: generate_diff_report
    
    type :: report_generator_t
        logical :: initialized = .false.
    contains
        procedure :: init => generator_init
        procedure :: generate_html => generator_generate_html
        procedure :: generate_terminal_display => generator_generate_terminal
    end type report_generator_t
    
contains
    
    ! Initialize generator
    subroutine generator_init(this)
        class(report_generator_t), intent(out) :: this
        this%initialized = .true.
    end subroutine generator_init
    
    ! Generate HTML report
    subroutine generator_generate_html(this, source_data, transformer, &
                                       theme_manager, output_path, &
                                       success, error_msg)
        class(report_generator_t), intent(inout) :: this
        type(coverage_data_t), intent(in) :: source_data
        type(data_transformer_t), intent(inout) :: transformer
        type(theme_manager_t), intent(inout) :: theme_manager
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        type(transformed_data_t) :: transformed_data
        type(color_scheme_t) :: theme
        character(len=:), allocatable :: html_content, css_variables
        integer :: unit, iostat
        
        success = .false.
        error_msg = ""
        
        ! Transform data
        call transformer%transform_data(source_data, transformed_data, &
                                       success, error_msg)
        if (.not. success) return
        
        ! Load theme
        call theme_manager%load_cyberpunk_theme(theme, success, error_msg)
        if (.not. success) return
        
        ! Generate CSS variables
        call theme_manager%generate_css_variables(theme, css_variables)
        
        ! Build HTML content using html_reporter
        html_content = generate_html_structure(transformed_data, &
                                               css_variables, theme)
        
        ! Write to file
        open(newunit=unit, file=output_path, status='replace', action='write', &
             iostat=iostat)
        if (iostat /= 0) then
            error_msg = "Failed to open output file: " // output_path
            return
        end if
        
        write(unit, '(A)') html_content
        close(unit)
        
        success = .true.
    end subroutine generator_generate_html
    
    ! Generate terminal display
    subroutine generator_generate_terminal(this, source_data, transformer, &
                                          theme_manager, config, session, &
                                          interactive, success, error_msg)
        class(report_generator_t), intent(inout) :: this
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
        
        ! For non-interactive mode, return early
        if (.not. interactive) then
            success = .true.
            return
        end if
        
        ! Start interactive TUI
        call start_interactive_tui(session, terminal_output, success, error_msg)
    end subroutine generator_generate_terminal
    
    ! Generate diff report between two coverage datasets
    subroutine generate_diff_report(baseline_data, current_data, &
                                   diff_output, success, error_msg)
        use coverage_metrics_core, only: coverage_metrics_t, &
                                         calculate_metrics_for_data, &
                                         generate_diff_output
        
        type(coverage_data_t), intent(in) :: baseline_data, current_data
        character(len=:), allocatable, intent(out) :: diff_output
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        type(coverage_metrics_t) :: baseline_metrics, current_metrics
        
        success = .false.
        error_msg = ""
        
        ! Calculate metrics for both datasets
        call calculate_metrics_for_data(baseline_data, baseline_metrics)
        call calculate_metrics_for_data(current_data, current_metrics)
        
        ! Generate diff report
        call generate_diff_output(baseline_metrics, current_metrics, diff_output)
        
        success = .true.
    end subroutine generate_diff_report
    
end module report_generator_core