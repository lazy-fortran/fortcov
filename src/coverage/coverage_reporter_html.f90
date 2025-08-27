module coverage_reporter_html
    !! HTML Coverage Reporter Implementation
    !! 
    !! HTML-based coverage reporter that generates styled HTML reports.
    !! Reuses existing HTML generation infrastructure from html_reporter_impl.
    use coverage_model_core
    use coverage_stats_core, only: stats_t => coverage_stats_t
    use coverage_reporter_base
    use coverage_reporter_utils
    use data_transformer_core, only: transform_coverage_data
    use html_reporter, only: generate_html_structure
    use theme_manager_core, only: theme_manager_t, color_scheme_t
    implicit none
    private
    
    public :: html_reporter_t
    
    ! HTML reporter implementation
    type, extends(coverage_reporter_t) :: html_reporter_t
    contains
        procedure :: generate_report => html_generate_report
        procedure :: get_format_name => html_get_format_name
        procedure :: supports_diff => html_supports_diff
    end type html_reporter_t

contains

    subroutine html_generate_report(this, coverage_data, output_path, &
                                  & success, error_message, &
                                  & diff_data, threshold)
        use zero_config_manager, only: ensure_output_directory_structure
        use error_handling_core, only: error_context_t
        use data_transformer_types, only: transformed_data_t
        use theme_manager_core, only: color_scheme_t
        class(html_reporter_t), intent(in) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message
        type(coverage_diff_t), intent(in), optional :: diff_data
        real, intent(in), optional :: threshold
        
        type(stats_t) :: line_stats
        type(error_context_t) :: error_ctx
        type(transformed_data_t) :: transformed_data
        type(theme_manager_t) :: theme_manager
        type(color_scheme_t) :: theme
        character(len=:), allocatable :: css_vars, html_content
        integer :: unit, stat
        logical :: use_stdout
        logical :: theme_success, transform_success
        character(len=256) :: theme_error
        character(len=:), allocatable :: transform_error
        
        success = .false.
        use_stdout = (trim(output_path) == "-")
        
        ! Ensure output directory exists for file output
        if (.not. use_stdout) then
            call ensure_output_directory_structure(output_path, error_ctx)
            if (error_ctx%error_code /= 0) then
                error_message = trim(error_ctx%message)
                return
            end if
        end if
        
        ! Calculate coverage statistics
        call calculate_manual_line_stats(coverage_data, line_stats)
        
        ! Apply threshold if provided
        if (present(threshold)) then
            if (line_stats%percentage < threshold) then
                error_message = "Coverage below threshold"
                return
            end if
        end if
        
        ! Transform coverage data to required format
        call transform_coverage_data(coverage_data, transformed_data, &
                                    transform_success, transform_error)
        if (.not. transform_success) then
            error_message = "Failed to transform coverage data: " // transform_error
            return
        end if
        
        ! Initialize theme manager and get default theme
        call theme_manager%init()
        call theme_manager%load_default_theme(theme, theme_success, theme_error)
        if (.not. theme_success) then
            error_message = "Failed to load default theme: " // trim(theme_error)
            return
        end if
        
        ! Generate CSS variables
        call theme_manager%generate_css_variables(theme, css_vars)
        
        ! Generate HTML content
        html_content = generate_html_structure(transformed_data, css_vars, theme)
        
        ! Write to output
        if (use_stdout) then
            unit = 6  ! Standard output
        else
            open(newunit=unit, file=output_path, status='replace', &
                 form='formatted', access='sequential', iostat=stat)
            if (stat /= 0) then
                success = .false.
                error_message = "Cannot write to output file '" // &
                              trim(output_path) // "'"
                return
            end if
        end if
        
        write(unit, '(A)') html_content
        
        if (.not. use_stdout) close(unit)
        
        success = .true.
        
        ! Suppress unused parameter warnings
        call suppress_unused_warning_html(this, diff_data)
        
    end subroutine html_generate_report

    function html_get_format_name(this) result(format_name)
        class(html_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        
        call suppress_unused_warning_simple_html(this)
        format_name = "html"
        
    end function html_get_format_name

    function html_supports_diff(this) result(supported)
        class(html_reporter_t), intent(in) :: this
        logical :: supported
        
        call suppress_unused_warning_simple_html(this)
        supported = .false.
        
    end function html_supports_diff

    ! Helper subroutines for unused parameter warnings
    subroutine suppress_unused_warning_html(reporter, diff_data)
        class(html_reporter_t), intent(in) :: reporter
        type(coverage_diff_t), intent(in), optional :: diff_data
        associate(r => reporter); end associate
        if (present(diff_data)) then
            associate(d => diff_data); end associate
        end if
    end subroutine suppress_unused_warning_html
    
    subroutine suppress_unused_warning_simple_html(reporter)
        class(html_reporter_t), intent(in) :: reporter
        associate(r => reporter); end associate
    end subroutine suppress_unused_warning_simple_html

end module coverage_reporter_html