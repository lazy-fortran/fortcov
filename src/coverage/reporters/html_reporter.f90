module html_reporter
    !! Consolidated HTML Reporter Module
    !! 
    !! Provides both object-oriented and procedural interfaces for HTML report generation.
    !! Consolidates functionality from both html_reporter implementations.
    !! Object-oriented interface for coverage reporter factory.
    !! Procedural interface for report engine compatibility.
    use coverage_types
    use coverage_reporter
    use coverage_stats_core, only: stats_t => coverage_stats_t
    use string_utils, only: int_to_string, format_percentage, int_to_str, real_to_str
    use data_transformer_types, only: transformed_data_t
    use theme_manager_core, only: color_scheme_t
    use timestamp_utils, only: get_current_timestamp_with_space
    implicit none
    private
    
    ! Object-oriented interface
    public :: html_reporter_t
    
    ! Procedural interface for compatibility
    public :: generate_html_structure
    public :: generate_comprehensive_css
    public :: generate_file_coverage_details
    
    type, extends(coverage_reporter_t) :: html_reporter_t
    contains
        procedure :: generate_report => html_generate_report
        procedure :: get_format_name => html_get_format_name
        procedure :: supports_diff => html_supports_diff
    end type html_reporter_t
    
contains
    
    subroutine html_generate_report(this, coverage_data, output_path, &
                                  success, error_message, &
                                  diff_data, threshold)
        use file_utilities, only: write_text_file
        use coverage_stats_core, only: calculate_line_coverage, coverage_stats_t
        class(html_reporter_t), intent(in) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message
        type(coverage_diff_t), intent(in), optional :: diff_data
        real, intent(in), optional :: threshold
        
        character(len=:), allocatable :: html_content
        type(coverage_stats_t) :: stats
        logical :: write_error
        integer :: i
        
        ! Calculate statistics
        stats = calculate_line_coverage(coverage_data)
        
        ! Generate HTML content
        html_content = "<!DOCTYPE html>" // new_line('a') // &
            "<html><head><title>Coverage Report</title>" // new_line('a') // &
            "<style>" // new_line('a') // &
            "body { font-family: Arial, sans-serif; margin: 20px; }" // new_line('a') // &
            ".summary { background: #f0f0f0; padding: 10px; border-radius: 5px; }" // new_line('a') // &
            ".file { margin: 10px 0; padding: 5px; border-left: 3px solid #007acc; }" // new_line('a') // &
            "</style></head><body>" // new_line('a') // &
            "<h1>Coverage Report</h1>" // new_line('a') // &
            "<div class='summary'>" // new_line('a')
        
        html_content = html_content // "<p><strong>Overall Coverage:</strong> " // &
                      format_percentage(stats%percentage, 2) // "</p>" // new_line('a')
        html_content = html_content // "<p><strong>Lines Covered:</strong> " // &
                      int_to_string(stats%covered_count) // " of " // &
                      int_to_string(stats%total_count) // "</p>" // new_line('a')
        if (allocated(coverage_data%files)) then
            html_content = html_content // "<p><strong>Files Analyzed:</strong> " // &
                          int_to_string(size(coverage_data%files)) // "</p>" // new_line('a')
        else
            html_content = html_content // "<p><strong>Files Analyzed:</strong> 0</p>" // new_line('a')
        end if
        
        html_content = html_content // "</div>" // new_line('a') // &
            "<h2>File Details</h2>" // new_line('a')
        
        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                html_content = html_content // "<div class='file'><strong>" // &
                              trim(coverage_data%files(i)%filename) // ":</strong> " // &
                              format_percentage(coverage_data%files(i)%line_coverage, 2) // &
                              "</div>" // new_line('a')
            end do
        end if
        
        html_content = html_content // "</body></html>"
        
        ! Write to file
        call write_text_file(output_path, html_content, write_error)
        
        if (write_error) then
            success = .false.
            error_message = "Failed to write HTML file: " // trim(output_path)
        else
            success = .true.
            error_message = ""
        end if
        
        ! Suppress unused warnings
        if (present(diff_data)) continue
        if (present(threshold)) continue
    end subroutine html_generate_report
    
    function html_get_format_name(this) result(format_name)
        class(html_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        format_name = "html"
    end function html_get_format_name
    
    function html_supports_diff(this) result(supported)
        class(html_reporter_t), intent(in) :: this
        logical :: supported
        supported = .false.
    end function html_supports_diff
    
    ! ========================================
    ! PROCEDURAL INTERFACE (for report engine compatibility)
    ! ========================================
    
    ! Generate complete HTML structure
    function generate_html_structure(data, css_vars, theme) result(html)
        type(transformed_data_t), intent(in) :: data
        character(len=*), intent(in) :: css_vars
        type(color_scheme_t), intent(in) :: theme
        character(len=:), allocatable :: html
        character(len=:), allocatable :: html_header, html_body, html_footer
        
        ! Generate HTML sections
        html_header = generate_html_header_proc(css_vars)
        html_body = generate_html_body_proc(data, theme)
        html_footer = generate_html_footer_proc()
        
        ! Combine all HTML sections
        html = '<!DOCTYPE html>' // new_line('a') // &
               '<html lang="en">' // new_line('a') // &
               html_header // new_line('a') // &
               '<body>' // new_line('a') // &
               html_body // new_line('a') // &
               html_footer // new_line('a') // &
               '</body>' // new_line('a') // &
               '</html>'
    end function generate_html_structure

    ! Generate comprehensive CSS styles
    function generate_comprehensive_css(css_vars) result(css)
        character(len=*), intent(in) :: css_vars
        character(len=:), allocatable :: css
        character(len=:), allocatable :: base_styles, component_styles, responsive_styles
        
        ! Generate CSS sections
        base_styles = generate_base_css_styles()
        component_styles = generate_component_css_styles()
        responsive_styles = generate_responsive_css_styles()
        
        ! Combine all CSS sections
        css = '    <style>' // new_line('a') // &
              css_vars // new_line('a') // &
              base_styles // new_line('a') // &
              component_styles // new_line('a') // &
              responsive_styles // new_line('a') // &
              '    </style>'
    end function generate_comprehensive_css

    ! Generate file coverage details section
    function generate_file_coverage_details(data) result(details)
        type(transformed_data_t), intent(in) :: data
        character(len=:), allocatable :: details
        character(len=:), allocatable :: file_list
        integer :: i

        ! Generate file list
        file_list = ''
        if (allocated(data%files) .and. size(data%files) > 0) then
            do i = 1, size(data%files)
                file_list = file_list // &
                    '            <div class="file-item">' // new_line('a') // &
                    '                <span class="file-name">' // &
                    trim(data%files(i)%filename) // '</span>' // new_line('a') // &
                    '                <span class="file-coverage">' // &
                    real_to_str(data%files(i)%coverage_percentage) // &
                    '%</span>' // new_line('a') // &
                    '                <div style="clear: both;"></div>' // new_line('a') // &
                    '            </div>' // new_line('a')
            end do
        else
            file_list = '            <div class="file-item">' // new_line('a') // &
                       '                <span class="file-name">No files found</span>' // new_line('a') // &
                       '                <span class="file-coverage">0.0%</span>' // new_line('a') // &
                       '            </div>' // new_line('a')
        end if

        details = '        <section class="file-section">' // new_line('a') // &
                  '            <h2>File Coverage Details</h2>' // new_line('a') // &
                  '            <div class="file-list cyberpunk">' // new_line('a') // &
                  file_list // &
                  '            </div>' // new_line('a') // &
                  '        </section>' // new_line('a')
    end function generate_file_coverage_details
    
    ! Internal procedural helper functions
    function generate_html_header_proc(css_vars) result(header)
        character(len=*), intent(in) :: css_vars
        character(len=:), allocatable :: header
        character(len=:), allocatable :: css_styles
        
        css_styles = generate_comprehensive_css(css_vars)
        
        header = '<head>' // new_line('a') // &
                 '    <meta charset="UTF-8">' // new_line('a') // &
                 '    <meta name="viewport" content="width=device-width, initial-scale=1.0">' // new_line('a') // &
                 '    <title>Fortcov Coverage Report</title>' // new_line('a') // &
                 '    <meta name="description" content="Code coverage report generated by Fortcov">' // new_line('a') // &
                 css_styles // new_line('a') // &
                 '</head>'
    end function generate_html_header_proc
    
    function generate_html_body_proc(data, theme) result(body)
        type(transformed_data_t), intent(in) :: data
        type(color_scheme_t), intent(in) :: theme
        character(len=:), allocatable :: body
        character(len=:), allocatable :: file_details
        
        file_details = generate_file_coverage_details(data)
        
        body = '    <header class="report-header">' // new_line('a') // &
               '        <h1>Coverage Report</h1>' // new_line('a') // &
               '        <div class="theme-info">Theme: ' // theme%name // '</div>' // new_line('a') // &
               '    </header>' // new_line('a') // &
               '    <main class="report-main">' // new_line('a') // &
               '        <section class="summary-section cyberpunk">' // new_line('a') // &
               '            <h2>Coverage Summary</h2>' // new_line('a') // &
               '            <div class="summary-grid">' // new_line('a') // &
               '                <div class="summary-item">' // new_line('a') // &
               '                    <span class="summary-label">Total Files:</span>' // new_line('a') // &
               '                    <span class="summary-value">' // &
               int_to_str(data%summary%total_files) // '</span>' // new_line('a') // &
               '                </div>' // new_line('a') // &
               '                <div class="summary-item">' // new_line('a') // &
               '                    <span class="summary-label">Total Lines:</span>' // new_line('a') // &
               '                    <span class="summary-value">' // &
               int_to_str(data%summary%total_lines) // '</span>' // new_line('a') // &
               '                </div>' // new_line('a') // &
               '                <div class="summary-item">' // new_line('a') // &
               '                    <span class="summary-label">Covered Lines:</span>' // new_line('a') // &
               '                    <span class="summary-value">' // &
               int_to_str(data%summary%covered_lines) // '</span>' // new_line('a') // &
               '                </div>' // new_line('a') // &
               '                <div class="summary-item highlight">' // new_line('a') // &
               '                    <span class="summary-label">Coverage:</span>' // new_line('a') // &
               '                    <span class="summary-value coverage-percent">' // &
               real_to_str(data%summary%coverage_percentage) // '%</span>' // new_line('a') // &
               '                </div>' // new_line('a') // &
               '            </div>' // new_line('a') // &
               '        </section>' // new_line('a') // &
               file_details // new_line('a') // &
               '    </main>'
    end function generate_html_body_proc
    
    function generate_html_footer_proc() result(footer)
        character(len=:), allocatable :: footer
        
        footer = '    <footer class="report-footer">' // new_line('a') // &
                 '        <p>Generated by <strong>Fortcov</strong> - Fortran Coverage Analysis Tool</p>' // new_line('a') // &
                 '        <p class="timestamp">Report generated at: ' // &
                 get_current_timestamp_with_space() // '</p>' // new_line('a') // &
                 '    </footer>'
    end function generate_html_footer_proc

    ! CSS generation functions
    function generate_base_css_styles() result(css)
        character(len=:), allocatable :: css
        
        css = '        /* Modern HTML5 reset and base styles */' // new_line('a') // &
              '        * { margin: 0; padding: 0; box-sizing: border-box; }' // new_line('a') // &
              '        body { font-family: "SF Mono", Monaco, Inconsolata, "Roboto Mono", ' // &
              'monospace; background: var(--background-color, #1a1a1a); color: ' // &
              'var(--text-color, #ffffff); line-height: 1.6; margin: 0; padding: 20px; }' // new_line('a') // &
              '        .report-header { background: linear-gradient(135deg, var(--accent-color, #ff00ff), ' // &
              'var(--secondary-color, #00ffff)); padding: 2rem; margin-bottom: 2rem; ' // &
              'border-radius: 8px; box-shadow: 0 4px 12px rgba(0,0,0,0.3); }' // new_line('a') // &
              '        .report-header h1 { font-size: 2.5rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.5); }' // new_line('a') // &
              '        .theme-info { font-size: 1.1rem; opacity: 0.9; margin-top: 0.5rem; }' // new_line('a') // &
              '        .report-footer { margin-top: 3rem; padding: 1.5rem; text-align: center; ' // &
              'border-top: 1px solid var(--accent-color, #ff00ff); opacity: 0.8; }' // new_line('a') // &
              '        .timestamp { font-size: 0.9rem; margin-top: 0.5rem; opacity: 0.7; }'
    end function generate_base_css_styles
    
    function generate_component_css_styles() result(css)
        character(len=:), allocatable :: css
        
        css = '        .cyberpunk { border: 2px solid var(--accent-color, #ff00ff); border-radius: 8px; padding: 1.5rem; margin: 1rem 0; background: rgba(255, 0, 255, 0.1); backdrop-filter: blur(5px); }' // new_line('a') // &
              '        .summary-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 1rem; margin-top: 1rem; }' // new_line('a') // &
              '        .summary-item { background: rgba(0, 255, 255, 0.1); padding: 1rem; border-radius: 6px; border-left: 4px solid var(--accent-color, #ff00ff); }' // new_line('a') // &
              '        .summary-item.highlight { background: rgba(255, 0, 255, 0.2); border-left: 4px solid var(--secondary-color, #00ffff); }' // new_line('a') // &
              '        .summary-label { display: block; font-weight: bold; margin-bottom: 0.5rem; }' // new_line('a') // &
              '        .summary-value { font-size: 1.5rem; color: var(--accent-color, #ff00ff); }' // new_line('a') // &
              '        .coverage-percent { font-weight: bold; text-shadow: 0 0 10px currentColor; }' // new_line('a') // &
              '        .file-list { margin-top: 2rem; }' // new_line('a') // &
              '        .file-item { background: rgba(255, 255, 255, 0.05); margin: 0.5rem 0; padding: 1rem; border-radius: 6px; border-left: 3px solid var(--secondary-color, #00ffff); }' // new_line('a') // &
              '        .file-name { font-weight: bold; color: var(--accent-color, #ff00ff); }' // new_line('a') // &
              '        .file-coverage { float: right; color: var(--secondary-color, #00ffff); }'
    end function generate_component_css_styles
    
    function generate_responsive_css_styles() result(css)
        character(len=:), allocatable :: css
        
        css = '        @media (max-width: 768px) { body { padding: 10px; } .report-header h1 { font-size: 2rem; } .summary-grid { grid-template-columns: 1fr; } }'
    end function generate_responsive_css_styles

end module html_reporter