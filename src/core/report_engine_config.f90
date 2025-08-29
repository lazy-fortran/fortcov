module report_engine_config
    !! Report Engine Configuration Management
    !! 
    !! Handles initialization and configuration for report engine components.
    !! Extracted from report_engine.f90 for SRP compliance (Issue #718).
    use coverage_model_core
    use coverage_metrics_core
    use data_transformer_core
    use data_transformer_types
    use theme_manager_core
    use syntax_highlighter
    use report_config_core
    implicit none
    private
    
    public :: report_config_init, terminal_session_init, terminal_session_cleanup
    public :: filter_criteria_init, coverage_metrics_init
    public :: initialize_report_components

contains

    ! Initialize report configuration
    subroutine report_config_init(this)
        class(report_config_t), intent(out) :: this
        
        this%output_format = "html"
        this%theme_name = "cyberpunk"
        this%enable_syntax_highlighting = .true.
        this%enable_coverage_annotation = .true.
        this%terminal_colors_enabled = .true.
        this%max_memory_mb = 100
        this%startup_timeout_seconds = 2.0
    end subroutine report_config_init
    
    ! Initialize terminal session
    subroutine terminal_session_init(this)
        class(terminal_session_t), intent(out) :: this
        
        this%is_active = .false.
        this%colors_enabled = .false.
        this%terminal_width = 80
        this%terminal_height = 24
        this%display_buffer = ""
    end subroutine terminal_session_init
    
    ! Cleanup terminal session
    subroutine terminal_session_cleanup(this)
        class(terminal_session_t), intent(inout) :: this
        
        this%is_active = .false.
        this%colors_enabled = .false.
        if (allocated(this%display_buffer)) deallocate(this%display_buffer)
    end subroutine terminal_session_cleanup
    
    ! Initialize filter criteria
    subroutine filter_criteria_init(this)
        class(filter_criteria_t), intent(out) :: this
        
        this%min_coverage_threshold = 0.0
        this%show_only_uncovered = .false.
        
        ! Allocate empty arrays
        allocate(character(len=100) :: this%include_patterns(0))
        allocate(character(len=100) :: this%exclude_patterns(0))
    end subroutine filter_criteria_init
    
    ! Initialize coverage metrics
    subroutine coverage_metrics_init(this)
        class(coverage_metrics_t), intent(out) :: this
        
        this%total_lines = 0
        this%covered_lines = 0
        this%total_files = 0
        this%line_coverage_percentage = 0.0
        this%branch_coverage_percentage = 0.0
        this%function_coverage_percentage = 0.0
    end subroutine coverage_metrics_init
    
    ! Initialize report components
    subroutine initialize_report_components(transformer, theme_manager, &
                                           highlighter, success, error_msg)
        type(data_transformer_t), allocatable, intent(out) :: transformer
        type(theme_manager_t), allocatable, intent(out) :: theme_manager
        type(syntax_highlighter_t), allocatable, intent(out) :: highlighter
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        logical :: component_success
        
        success = .false.
        error_msg = ""
        
        ! Initialize foundation components
        
        ! 1. Data transformer
        allocate(transformer)
        call transformer%init()
        
        ! 2. Theme manager
        allocate(theme_manager)
        call theme_manager%init()
        
        ! 3. Syntax highlighter
        allocate(highlighter)
        call highlighter%init()
        
        ! Load Fortran rules for syntax highlighting
        call highlighter%load_fortran_rules(highlighter%fortran_rules, &
                                           component_success)
        if (.not. component_success) then
            error_msg = "Failed to load Fortran syntax rules"
            return
        end if
        
        success = .true.
    end subroutine initialize_report_components

end module report_engine_config