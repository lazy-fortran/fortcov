module report_engine_impl
    !! Report Engine Implementation - Foundation Layer Component
    !! 
    !! Orchestrates report generation using focused, extracted modules.
    !! Refactored for QADS architecture compliance (Phase 1, Issue #366).
    use coverage_model_core
    use data_transformer_impl
    use theme_manager_core
    use syntax_highlighter
    use report_config_core
    use coverage_metrics_core
    use html_reporter_impl
    use tui_manager_core
    use coverage_data_filter_impl
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
        logical :: initialized = .false.
    contains
        procedure :: init => report_engine_init
        procedure :: generate_html_report => report_engine_generate_html_report
        procedure :: launch_terminal_browser => report_engine_launch_terminal_browser
        procedure :: generate_diff_report => report_engine_generate_diff_report
        procedure :: apply_filtering => report_engine_apply_filtering
        procedure :: calculate_metrics => report_engine_calculate_metrics
        procedure :: generate_styled_report => report_engine_generate_styled_report
        ! Private helper procedures
        procedure, private :: prepare_report_data
        procedure, private :: generate_format_specific_output
    end type report_engine_t

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
    
    ! Initialize report engine
    subroutine report_engine_init(this, success, error_msg)
        class(report_engine_t), intent(out) :: this
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        logical :: component_success
        character(len=:), allocatable :: component_error
        
        success = .false.
        error_msg = ""
        this%initialized = .false.
        
        ! Initialize configuration
        call this%config%init()
        
        ! Initialize foundation components
        
        ! 1. Data transformer
        allocate(this%transformer)
        call this%transformer%init()
        
        ! 2. Theme manager
        allocate(this%theme_manager)
        call this%theme_manager%init()
        
        ! 3. Syntax highlighter
        allocate(this%highlighter)
        call this%highlighter%init()
        
        ! Load Fortran rules for syntax highlighting
        call this%highlighter%load_fortran_rules(this%highlighter%fortran_rules, &
                                                component_success)
        if (.not. component_success) then
            error_msg = "Failed to load Fortran syntax rules"
            return
        end if
        
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
        
        type(transformed_data_t) :: transformed_data
        type(color_scheme_t) :: theme
        character(len=:), allocatable :: html_content, css_variables
        integer :: unit, iostat
        
        success = .false.
        error_msg = ""
        
        if (.not. this%initialized) then
            error_msg = "Report engine not initialized"
            return
        end if
        
        ! Transform data
        call this%transformer%transform_data(this%source_data, transformed_data, &
                                           success, error_msg)
        if (.not. success) return
        
        ! Load theme
        call this%theme_manager%load_cyberpunk_theme(theme, success, error_msg)
        if (.not. success) return
        
        ! Generate CSS variables
        call this%theme_manager%generate_css_variables(theme, css_variables)
        
        ! Build HTML content
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
    end subroutine report_engine_generate_html_report
    
    ! Launch terminal browser
    subroutine report_engine_launch_terminal_browser(this, session, interactive, &
                                                    success, error_msg)
        class(report_engine_t), intent(inout) :: this
        type(terminal_session_t), intent(out) :: session
        logical, intent(in) :: interactive
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        type(transformed_data_t) :: transformed_data
        type(color_scheme_t) :: theme
        character(len=:), allocatable :: terminal_output
        
        success = .false.
        error_msg = ""
        
        if (.not. this%initialized) then
            error_msg = "Report engine not initialized"
            return
        end if
        
        call session%init()
        
        ! Transform data for terminal display
        call this%transformer%transform_data(this%source_data, transformed_data, &
                                           success, error_msg)
        if (.not. success) return
        
        ! Load theme for terminal
        call this%theme_manager%load_cyberpunk_theme(theme, success, error_msg)
        if (.not. success) return
        
        ! Generate terminal-friendly output
        call generate_terminal_display(transformed_data, theme, terminal_output)
        
        session%is_active = .true.
        session%colors_enabled = this%config%terminal_colors_enabled
        session%display_buffer = terminal_output
        
        ! For non-interactive mode (testing), we don't start the actual TUI
        if (.not. interactive) then
            success = .true.
            return
        end if
        
        ! In interactive mode, start the full TUI with proper loop control
        call start_interactive_tui(session, terminal_output, success, error_msg)
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
        
        type(coverage_metrics_t) :: baseline_metrics, current_metrics
        
        success = .false.
        error_msg = ""
        
        if (.not. this%initialized) then
            error_msg = "Report engine not initialized"
            return
        end if
        
        ! Calculate metrics for both datasets
        call calculate_metrics_for_data(baseline_data, baseline_metrics)
        call calculate_metrics_for_data(current_data, current_metrics)
        
        ! Generate diff report
        call generate_diff_output(baseline_metrics, current_metrics, diff_output)
        
        success = .true.
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
        
        type(coverage_file_t), allocatable :: temp_files(:)
        integer :: i, filtered_count
        logical :: include_file
        
        success = .false.
        error_msg = ""
        
        if (.not. allocated(input_data%files)) then
            error_msg = "No input data to filter"
            return
        end if
        
        ! First pass: count files that pass filter
        filtered_count = 0
        do i = 1, size(input_data%files)
            call should_include_file(input_data%files(i), criteria, include_file)
            if (include_file) filtered_count = filtered_count + 1
        end do
        
        if (filtered_count == 0) then
            call filtered_data%init()
            success = .true.
            return
        end if
        
        ! Second pass: collect filtered files
        allocate(temp_files(filtered_count))
        filtered_count = 0
        
        do i = 1, size(input_data%files)
            call should_include_file(input_data%files(i), criteria, include_file)
            if (include_file) then
                filtered_count = filtered_count + 1
                temp_files(filtered_count) = input_data%files(i)
            end if
        end do
        
        call filtered_data%init()
        filtered_data%files = temp_files
        success = .true.
    end subroutine report_engine_apply_filtering
    
    ! Calculate metrics
    subroutine report_engine_calculate_metrics(this, metrics, success, error_msg)
        class(report_engine_t), intent(inout) :: this
        type(coverage_metrics_t), intent(out) :: metrics
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        success = .false.
        error_msg = ""
        
        if (.not. this%initialized) then
            error_msg = "Report engine not initialized"
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
        
        type(transformed_data_t) :: transformed_data
        type(color_scheme_t) :: theme
        character(len=:), allocatable :: source_content
        
        success = .false.
        error_msg = ""
        
        if (.not. this%initialized) then
            error_msg = "Report engine not initialized"
            return
        end if
        
        ! Prepare report data (theme, transformed data, source content)
        call this%prepare_report_data(transformed_data, theme, source_content, &
                                      success, error_msg)
        if (.not. success) return
        
        ! Generate format-specific output
        call this%generate_format_specific_output(format, source_content, theme, &
                                                 output, success, error_msg)
        
    end subroutine report_engine_generate_styled_report
    
    ! Prepare report data (theme, transformed data, source content)
    subroutine prepare_report_data(this, transformed_data, theme, &
                                   source_content, success, error_msg)
        class(report_engine_t), intent(inout) :: this
        type(transformed_data_t), intent(out) :: transformed_data
        type(color_scheme_t), intent(out) :: theme
        character(len=:), allocatable, intent(out) :: source_content
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        success = .false.
        error_msg = ""
        
        ! Transform data
        call this%transformer%transform_data(this%source_data, transformed_data, &
                                           success, error_msg)
        if (.not. success) return
        
        ! Load theme
        call this%theme_manager%load_cyberpunk_theme(theme, success, error_msg)
        if (.not. success) return
        
        ! Create sample source content from transformed data
        if (allocated(transformed_data%files) .and. &
            & size(transformed_data%files) > 0) then
            source_content = "program example" // new_line('a') // &
                           "    ! Test comment" // new_line('a') // &
                           "    integer :: x = 1" // new_line('a') // &
                           "end program"
        else
            source_content = "! No source files available"
        end if
        
        success = .true.
    end subroutine prepare_report_data
    
    ! Generate format-specific output
    subroutine generate_format_specific_output(this, format, source_content, &
                                               theme, output, success, error_msg)
        class(report_engine_t), intent(inout) :: this
        character(len=*), intent(in) :: format
        character(len=*), intent(in) :: source_content
        type(color_scheme_t), intent(in) :: theme
        character(len=:), allocatable, intent(out) :: output
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        character(len=:), allocatable :: highlighted_content, css_variables
        
        success = .false.
        error_msg = ""
        
        ! Apply syntax highlighting based on format
        select case (trim(format))
        case ("html")
            call this%highlighter%highlight_for_html(source_content, &
                                                    highlighted_content, success)
            if (.not. success) then
                error_msg = "Failed to generate HTML highlighting"
                return
            end if
            
            ! Add theme CSS variables
            call this%theme_manager%generate_css_variables(theme, css_variables)
            output = '<style>' // new_line('a') // css_variables // &
                     new_line('a') // &
                     '</style>' // new_line('a') // highlighted_content
            
        case ("terminal")
            call this%highlighter%highlight_for_terminal(source_content, &
                                                        highlighted_content, success)
            if (.not. success) then
                error_msg = "Failed to generate terminal highlighting"
                return
            end if
            output = highlighted_content
            
        case default
            error_msg = "Unsupported output format: " // format
            return
        end select
        
        success = .true.
    end subroutine generate_format_specific_output
    
    ! HTML generation delegated to html_report_generator module
    
    ! CSS generation delegated to html_report_generator module
    
    ! All helper functions delegated to specialized modules:
    ! - File coverage details -> html_report_generator
    ! - Timestamp -> html_report_generator  
    ! - Terminal display -> terminal_ui_manager
    ! - Metrics calculation -> coverage_metrics_calculator
    ! - Diff generation -> coverage_metrics_calculator
    ! - File filtering -> coverage_data_filter
    ! - String conversion -> individual modules
    ! - Interactive TUI -> terminal_ui_manager

end module report_engine_impl
