module report_engine
    !! Report Engine Orchestrator - Coordinates report generation
    !! 
    !! Minimal orchestrator that delegates to specialized modules.
    !! Refactored for SRP compliance (Issue #718).
    use coverage_model_core
    use data_transformer_core
    use data_transformer_types
    use theme_manager_core
    use syntax_highlighter
    use report_config_core
    use coverage_metrics_core
    use html_reporter
    use tui_manager_core
    use coverage_data_filter
    use report_generator_core
    use report_formatter_core
    implicit none
    private
    
    ! Re-export types from extracted modules
    public :: report_engine_t
    public :: report_config_t, terminal_session_t, filter_criteria_t
    public :: coverage_metrics_t
    
    type :: report_engine_t
        type(coverage_data_t) :: source_data
        type(report_config_t) :: config
        type(theme_manager_t), allocatable :: theme_manager
        type(syntax_highlighter_t), allocatable :: highlighter
        type(data_transformer_t), allocatable :: transformer
        type(report_generator_t), allocatable :: generator
        type(report_formatter_t), allocatable :: formatter
        logical :: initialized = .false.
    contains
        procedure :: init => report_engine_init
        procedure :: generate_html_report => report_engine_generate_html_report
        procedure :: launch_terminal_browser => report_engine_launch_terminal_browser
        procedure :: generate_diff_report => report_engine_generate_diff_report
        procedure :: apply_filtering => report_engine_apply_filtering
        procedure :: calculate_metrics => report_engine_calculate_metrics
        procedure :: generate_styled_report => report_engine_generate_styled_report
    end type report_engine_t

contains
    
    ! Initialize report engine
    subroutine report_engine_init(this, success, error_msg)
        class(report_engine_t), intent(out) :: this
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        logical :: component_success
        character(len=:), allocatable :: component_error
        integer :: stat
        character(len=256) :: errmsg
        
        success = .false.
        error_msg = ""
        this%initialized = .false.
        
        ! Initialize configuration
        call this%config%init()
        
        ! Initialize foundation components with proper error handling
        allocate(this%transformer, stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            error_msg = "Failed to allocate transformer: " // trim(errmsg)
            return
        end if
        call this%transformer%init()
        
        allocate(this%theme_manager, stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            error_msg = "Failed to allocate theme_manager: " // trim(errmsg)
            return
        end if
        call this%theme_manager%init()
        
        allocate(this%highlighter, stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            error_msg = "Failed to allocate highlighter: " // trim(errmsg)
            return
        end if
        call this%highlighter%init()
        
        ! Load Fortran rules for syntax highlighting
        call this%highlighter%load_fortran_rules(this%highlighter%fortran_rules, &
                                                component_success)
        if (.not. component_success) then
            error_msg = "Failed to load Fortran syntax rules"
            return
        end if
        
        ! Initialize specialized components with proper error handling
        allocate(this%generator, stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            error_msg = "Failed to allocate generator: " // trim(errmsg)
            return
        end if
        call this%generator%init()
        
        allocate(this%formatter, stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            error_msg = "Failed to allocate formatter: " // trim(errmsg)
            return
        end if
        call this%formatter%init()
        
        ! Initialize coverage data
        call this%source_data%init()
        
        this%initialized = .true.
        success = .true.
    end subroutine report_engine_init
    
    ! Generate HTML report
    subroutine report_engine_generate_html_report(this, output_path, &
                                                   success, error_msg)
        class(report_engine_t), intent(inout) :: this
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        if (.not. this%initialized) then
            error_msg = "Report engine not initialized"
            success = .false.
            return
        end if
        
        ! Delegate to generator module
        call this%generator%generate_html(this%source_data, this%transformer, &
                                         this%theme_manager, output_path, &
                                         success, error_msg)
    end subroutine report_engine_generate_html_report
    
    ! Launch terminal browser
    subroutine report_engine_launch_terminal_browser(this, session, interactive, &
                                                    success, error_msg)
        class(report_engine_t), intent(inout) :: this
        type(terminal_session_t), intent(out) :: session
        logical, intent(in) :: interactive
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        if (.not. this%initialized) then
            error_msg = "Report engine not initialized"
            success = .false.
            return
        end if
        
        ! Delegate to generator module
        call this%generator%generate_terminal_display(this%source_data, &
                                                     this%transformer, &
                                                     this%theme_manager, &
                                                     this%config, session, &
                                                     interactive, success, &
                                                     error_msg)
    end subroutine report_engine_launch_terminal_browser
    
    ! Generate diff report
    subroutine report_engine_generate_diff_report(this, baseline_data, &
                                                   current_data, &
                                                 diff_output, success, error_msg)
        class(report_engine_t), intent(inout) :: this
        type(coverage_data_t), intent(in) :: baseline_data, current_data
        character(len=:), allocatable, intent(out) :: diff_output
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        if (.not. this%initialized) then
            error_msg = "Report engine not initialized"
            success = .false.
            return
        end if
        
        ! Delegate to metrics module
        call generate_diff_report(baseline_data, current_data, diff_output, &
                                 success, error_msg)
    end subroutine report_engine_generate_diff_report
    
    ! Apply filtering
    subroutine report_engine_apply_filtering(this, input_data, criteria, &
                                           filtered_data, success, error_msg)
        class(report_engine_t), intent(inout) :: this
        type(coverage_data_t), intent(in) :: input_data
        type(filter_criteria_t), intent(in) :: criteria
        type(coverage_data_t), intent(out) :: filtered_data
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        ! Delegate to coverage_data_filter module
        call apply_filter_criteria(input_data, criteria, filtered_data, &
                                  success, error_msg)
    end subroutine report_engine_apply_filtering
    
    ! Calculate metrics
    subroutine report_engine_calculate_metrics(this, metrics, success, error_msg)
        class(report_engine_t), intent(inout) :: this
        type(coverage_metrics_t), intent(out) :: metrics
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        if (.not. this%initialized) then
            error_msg = "Report engine not initialized"
            success = .false.
            return
        end if
        
        call calculate_metrics_for_data(this%source_data, metrics)
        success = .true.
    end subroutine report_engine_calculate_metrics
    
    ! Generate styled report
    subroutine report_engine_generate_styled_report(this, format, output, &
                                                      & success, error_msg)
        class(report_engine_t), intent(inout) :: this
        character(len=*), intent(in) :: format
        character(len=:), allocatable, intent(out) :: output
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        if (.not. this%initialized) then
            error_msg = "Report engine not initialized"
            success = .false.
            return
        end if
        
        ! Delegate to formatter module
        call this%formatter%format_report(this%source_data, this%transformer, &
                                         this%theme_manager, this%highlighter, &
                                         format, output, success, error_msg)
    end subroutine report_engine_generate_styled_report

end module report_engine
