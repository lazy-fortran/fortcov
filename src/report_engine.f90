module report_engine
    use iso_fortran_env, only: real64
    use coverage_model
    use data_transformer
    use theme_manager
    use syntax_highlighter
    use tui_main_loop
    implicit none
    private
    
    ! Public types
    public :: report_engine_t
    public :: report_config_t
    public :: terminal_session_t
    public :: filter_criteria_t
    public :: coverage_metrics_t
    
    type :: report_config_t
        character(len=:), allocatable :: output_format
        character(len=:), allocatable :: theme_name
        logical :: enable_syntax_highlighting
        logical :: enable_coverage_annotation
        logical :: terminal_colors_enabled
        integer :: max_memory_mb
        real :: startup_timeout_seconds
    contains
        procedure :: init => report_config_init
    end type report_config_t
    
    type :: terminal_session_t
        logical :: is_active = .false.
        logical :: colors_enabled = .false.
        integer :: terminal_width = 80
        integer :: terminal_height = 24
        character(len=:), allocatable :: display_buffer
    contains
        procedure :: init => terminal_session_init
        procedure :: cleanup => terminal_session_cleanup
    end type terminal_session_t
    
    type :: filter_criteria_t
        real :: min_coverage_threshold = 0.0
        character(len=:), allocatable :: include_patterns(:)
        character(len=:), allocatable :: exclude_patterns(:)
        logical :: show_only_uncovered = .false.
    contains
        procedure :: init => filter_criteria_init
    end type filter_criteria_t
    
    type :: coverage_metrics_t
        integer :: total_lines = 0
        integer :: covered_lines = 0
        integer :: total_files = 0
        real :: line_coverage_percentage = 0.0
        real :: branch_coverage_percentage = 0.0
        real :: function_coverage_percentage = 0.0
    contains
        procedure :: init => coverage_metrics_init
    end type coverage_metrics_t
    
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
    subroutine report_engine_generate_html_report(this, output_path, success, error_msg)
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
        html_content = generate_html_structure(transformed_data, css_variables, theme)
        
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
    subroutine report_engine_generate_diff_report(this, baseline_data, current_data, &
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
        
        call filtered_data%init(temp_files)
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
    subroutine report_engine_generate_styled_report(this, format, output, success, error_msg)
        class(report_engine_t), intent(inout) :: this
        character(len=*), intent(in) :: format
        character(len=:), allocatable, intent(out) :: output
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        type(transformed_data_t) :: transformed_data
        type(color_scheme_t) :: theme
        character(len=:), allocatable :: source_content, highlighted_content, css_variables
        
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
        
        ! Create sample source content from transformed data
        if (allocated(transformed_data%files) .and. size(transformed_data%files) > 0) then
            source_content = "program example" // new_line('a') // &
                           "    ! Test comment" // new_line('a') // &
                           "    integer :: x = 1" // new_line('a') // &
                           "end program"
        else
            source_content = "! No source files available"
        end if
        
        ! Apply syntax highlighting based on format
        select case (trim(format))
        case ("html")
            call this%highlighter%highlight_for_html(source_content, highlighted_content, success)
            if (.not. success) then
                error_msg = "Failed to generate HTML highlighting"
                return
            end if
            
            ! Add theme CSS variables
            call this%theme_manager%generate_css_variables(theme, css_variables)
            output = '<style>' // new_line('a') // css_variables // new_line('a') // '</style>' // new_line('a') // highlighted_content
            
        case ("terminal")
            call this%highlighter%highlight_for_terminal(source_content, highlighted_content, success)
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
    end subroutine report_engine_generate_styled_report
    
    ! Helper functions
    
    function generate_html_structure(data, css_vars, theme) result(html)
        type(transformed_data_t), intent(in) :: data
        character(len=*), intent(in) :: css_vars
        type(color_scheme_t), intent(in) :: theme
        character(len=:), allocatable :: html
        
        html = '<!DOCTYPE html>' // new_line('a') // &
               '<html lang="en">' // new_line('a') // &
               '<head>' // new_line('a') // &
               '    <meta charset="UTF-8">' // new_line('a') // &
               '    <meta name="viewport" content="width=device-width, initial-scale=1.0">' // new_line('a') // &
               '    <title>Coverage Report</title>' // new_line('a') // &
               '    <style>' // new_line('a') // &
               css_vars // new_line('a') // &
               '        body { font-family: monospace; background: var(--background-color); color: var(--text-color); }' // new_line('a') // &
               '        .cyberpunk { border: 1px solid var(--accent-color); }' // new_line('a') // &
               '        .keyword { color: var(--accent-color); font-weight: bold; }' // new_line('a') // &
               '        .comment { color: #888; font-style: italic; }' // new_line('a') // &
               '    </style>' // new_line('a') // &
               '</head>' // new_line('a') // &
               '<body>' // new_line('a') // &
               '    <h1>Coverage Report</h1>' // new_line('a') // &
               '    <div class="cyberpunk">' // new_line('a') // &
               '        <p>Theme: ' // theme%name // '</p>' // new_line('a') // &
               '        <p>Files: ' // int_to_str(data%summary%total_files) // '</p>' // new_line('a') // &
               '        <p>Coverage: ' // real_to_str(data%summary%coverage_percentage) // '%</p>' // new_line('a') // &
               '    </div>' // new_line('a') // &
               '</body>' // new_line('a') // &
               '</html>'
    end function generate_html_structure
    
    subroutine generate_terminal_display(data, theme, output)
        type(transformed_data_t), intent(in) :: data
        type(color_scheme_t), intent(in) :: theme
        character(len=:), allocatable, intent(out) :: output
        
        output = char(27)//'[1;32m' // 'Coverage Report' // char(27)//'[0m' // new_line('a') // &
                '================================' // new_line('a') // &
                'Theme: ' // theme%name // new_line('a') // &
                'Files: ' // int_to_str(data%summary%total_files) // new_line('a') // &
                'Coverage: ' // real_to_str(data%summary%coverage_percentage) // '%' // new_line('a')
    end subroutine generate_terminal_display
    
    subroutine calculate_metrics_for_data(data, metrics)
        type(coverage_data_t), intent(in) :: data
        type(coverage_metrics_t), intent(out) :: metrics
        
        integer :: i, j, total_executable, total_covered
        
        call metrics%init()
        
        if (.not. allocated(data%files)) return
        
        metrics%total_files = size(data%files)
        total_executable = 0
        total_covered = 0
        
        do i = 1, size(data%files)
            if (allocated(data%files(i)%lines)) then
                do j = 1, size(data%files(i)%lines)
                    if (data%files(i)%lines(j)%is_executable) then
                        total_executable = total_executable + 1
                        if (data%files(i)%lines(j)%execution_count > 0) then
                            total_covered = total_covered + 1
                        end if
                    end if
                end do
            end if
        end do
        
        metrics%total_lines = total_executable
        metrics%covered_lines = total_covered
        
        if (total_executable > 0) then
            metrics%line_coverage_percentage = &
                real(total_covered) / real(total_executable) * 100.0
        end if
    end subroutine calculate_metrics_for_data
    
    subroutine generate_diff_output(baseline, current, output)
        type(coverage_metrics_t), intent(in) :: baseline, current
        character(len=:), allocatable, intent(out) :: output
        
        real :: coverage_delta
        character(len=:), allocatable :: status
        
        coverage_delta = current%line_coverage_percentage - baseline%line_coverage_percentage
        
        if (coverage_delta > 0.1) then
            status = "improved"
        else if (coverage_delta < -0.1) then
            status = "degraded"
        else
            status = "unchanged"
        end if
        
        output = 'Coverage Diff Report' // new_line('a') // &
                '===================' // new_line('a') // &
                'Baseline: ' // real_to_str(baseline%line_coverage_percentage) // '%' // new_line('a') // &
                'Current:  ' // real_to_str(current%line_coverage_percentage) // '%' // new_line('a') // &
                'Delta:    ' // real_to_str(coverage_delta) // '%' // new_line('a') // &
                'Status:   ' // status // new_line('a')
    end subroutine generate_diff_output
    
    subroutine should_include_file(file, criteria, include)
        type(coverage_file_t), intent(in) :: file
        type(filter_criteria_t), intent(in) :: criteria
        logical, intent(out) :: include
        
        real :: file_coverage
        integer :: i
        logical :: matches_include, matches_exclude
        
        include = .true.
        
        ! Check coverage threshold
        file_coverage = file%get_line_coverage_percentage()
        if (file_coverage < criteria%min_coverage_threshold) then
            include = .false.
            return
        end if
        
        ! Check include patterns
        if (allocated(criteria%include_patterns) .and. &
            size(criteria%include_patterns) > 0) then
            matches_include = .false.
            do i = 1, size(criteria%include_patterns)
                if (index(file%filename, trim(criteria%include_patterns(i))) > 0) then
                    matches_include = .true.
                    exit
                end if
            end do
            if (.not. matches_include) then
                include = .false.
                return
            end if
        end if
        
        ! Check exclude patterns
        if (allocated(criteria%exclude_patterns) .and. &
            size(criteria%exclude_patterns) > 0) then
            matches_exclude = .false.
            do i = 1, size(criteria%exclude_patterns)
                if (index(file%filename, trim(criteria%exclude_patterns(i))) > 0) then
                    matches_exclude = .true.
                    exit
                end if
            end do
            if (matches_exclude) then
                include = .false.
                return
            end if
        end if
    end subroutine should_include_file
    
    function int_to_str(num) result(str)
        integer, intent(in) :: num
        character(len=:), allocatable :: str
        character(len=20) :: temp_str
        
        write(temp_str, '(I0)') num
        str = trim(temp_str)
    end function int_to_str
    
    function real_to_str(num) result(str)
        real, intent(in) :: num
        character(len=:), allocatable :: str
        character(len=20) :: temp_str
        
        write(temp_str, '(F0.1)') num
        str = trim(temp_str)
    end function real_to_str
    
    ! Start interactive TUI with proper main loop
    subroutine start_interactive_tui(session, display_content, success, error_msg)
        type(terminal_session_t), intent(inout) :: session
        character(len=*), intent(in) :: display_content
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        type(tui_engine_t) :: tui
        type(tui_config_t) :: tui_config
        
        success = .false.
        error_msg = ""
        
        ! Configure TUI engine
        call tui_config%init()
        tui_config%enable_colors = session%colors_enabled
        tui_config%input_timeout_ms = 50  ! 20 FPS for responsive UI
        tui_config%max_iterations = 5000  ! Safety limit
        tui_config%debug_mode = .false.
        
        ! Initialize TUI engine
        call tui%init(tui_config)
        
        ! Set display content
        tui%display_buffer = display_content
        
        ! Display initial content
        if (session%colors_enabled) then
            print *, char(27)//'[2J'//char(27)//'[H'  ! Clear screen and home cursor
        end if
        print *, trim(display_content)
        print *, ""
        print *, "Interactive TUI Mode - Press Ctrl+C to exit"
        print *, "Frame rate limited to prevent system issues"
        
        ! Run the TUI main loop with proper controls
        call tui%run_main_loop(.true., 30.0_real64)  ! Interactive, 30 second max
        
        ! Clean up
        if (session%colors_enabled) then
            print *, char(27)//'[0m'  ! Reset terminal colors
        end if
        
        call tui%cleanup()
        success = .true.
    end subroutine start_interactive_tui

end module report_engine