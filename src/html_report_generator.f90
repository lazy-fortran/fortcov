module html_report_generator
    !! HTML Report Generation
    !!
    !! Handles HTML report structure, CSS generation, and file output.
    !! Extracted from report_engine_impl.f90 for SRP compliance.
    use data_transformer
    use theme_manager
    use string_utilities, only: int_to_str, real_to_str
    implicit none
    private

    ! Public procedures
    public :: generate_html_structure
    public :: generate_comprehensive_css
    public :: generate_file_coverage_details
    
    ! Internal CSS generation procedures
    private :: generate_base_css_styles
    private :: generate_component_css_styles
    private :: generate_responsive_css_styles
    
    ! Internal HTML generation procedures  
    private :: generate_html_header
    private :: generate_html_body
    private :: generate_html_footer

contains

    ! Generate complete HTML structure
    function generate_html_structure(data, css_vars, theme) result(html)
        type(transformed_data_t), intent(in) :: data
        character(len=*), intent(in) :: css_vars
        type(color_scheme_t), intent(in) :: theme
        character(len=:), allocatable :: html
        character(len=:), allocatable :: html_header, html_body, html_footer
        
        ! Generate HTML sections
        html_header = generate_html_header(css_vars)
        html_body = generate_html_body(data, theme)
        html_footer = generate_html_footer()
        
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
        character(len=:), allocatable :: base_styles, component_styles, &
                                         responsive_styles
        
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
                    '                <div style="clear: both;"></div>' // &
                    new_line('a') // &
                    '            </div>' // new_line('a')
            end do
        else
            file_list = '            <div class="file-item">' // new_line('a') // &
                       '                <span class="file-name">' // &
                       'No files found</span>' // new_line('a') // &
                       '                <span class="file-coverage">' // &
                       '0.0%</span>' // new_line('a') // &
                       '            </div>' // new_line('a')
        end if

        details = '        <section class="file-section">' // new_line('a') // &
                  '            <h2>File Coverage Details</h2>' // new_line('a') // &
                  '            <div class="file-list cyberpunk">' // new_line('a') // &
                  file_list // &
                  '            </div>' // new_line('a') // &
                  '        </section>' // new_line('a')
    end function generate_file_coverage_details

    ! Get current timestamp
    function get_current_timestamp() result(timestamp)
        character(len=:), allocatable :: timestamp
        character(len=8) :: date_str
        character(len=10) :: time_str

        call date_and_time(date_str, time_str)
        timestamp = date_str(1:4) // '-' // date_str(5:6) // '-' // date_str(7:8) // &
                   ' ' // time_str(1:2) // ':' // time_str(3:4) // ':' // time_str(5:6)
    end function get_current_timestamp

    ! Generate base CSS styles (reset, body, header)
    function generate_base_css_styles() result(css)
        character(len=:), allocatable :: css
        
        css = '        /* Modern HTML5 reset and base styles */' // new_line('a') // &
              '        * { margin: 0; padding: 0; box-sizing: border-box; }' // &
              new_line('a') // &
              '        body {' // new_line('a') // &
              '            font-family: "SF Mono", Monaco, Inconsolata, ' // &
              '"Roboto Mono", monospace;' // new_line('a') // &
              '            background: var(--background-color, #1a1a1a);' // &
              new_line('a') // &
              '            color: var(--text-color, #ffffff);' // new_line('a') // &
              '            line-height: 1.6; margin: 0; padding: 20px;' // &
              new_line('a') // &
              '        }' // new_line('a') // &
              '        .report-header {' // new_line('a') // &
              '            background: linear-gradient(135deg, ' // &
              'var(--accent-color, #ff00ff), var(--secondary-color, #00ffff));' // &
              new_line('a') // &
              '            padding: 2rem; margin-bottom: 2rem; border-radius: 8px;' // &
              new_line('a') // &
              '            box-shadow: 0 4px 12px rgba(0,0,0,0.3);' // &
              new_line('a') // &
              '        }' // new_line('a') // &
              '        .report-header h1 { font-size: 2.5rem; ' // &
              'text-shadow: 2px 2px 4px rgba(0,0,0,0.5); }' // new_line('a') // &
              '        .theme-info { font-size: 1.1rem; opacity: 0.9; ' // &
              'margin-top: 0.5rem; }' // new_line('a') // &
              '        .report-footer {' // new_line('a') // &
              '            margin-top: 3rem; padding: 1.5rem; ' // &
              'text-align: center;' // new_line('a') // &
              '            border-top: 1px solid var(--accent-color, #ff00ff); ' // &
              'opacity: 0.8;' // new_line('a') // &
              '        }' // new_line('a') // &
              '        .timestamp { font-size: 0.9rem; margin-top: 0.5rem; ' // &
              'opacity: 0.7; }'
    end function generate_base_css_styles
    
    ! Generate component CSS styles (cyberpunk, summary, file items)
    function generate_component_css_styles() result(css)
        character(len=:), allocatable :: css
        
        css = '        .cyberpunk {' // new_line('a') // &
              '            border: 2px solid var(--accent-color, #ff00ff);' // &
              new_line('a') // &
              '            border-radius: 8px; padding: 1.5rem; ' // &
              'margin: 1rem 0;' // new_line('a') // &
              '            background: rgba(255, 0, 255, 0.1); ' // &
              'backdrop-filter: blur(5px);' // new_line('a') // &
              '        }' // new_line('a') // &
              '        .summary-grid {' // new_line('a') // &
              '            display: grid; grid-template-columns: ' // &
              'repeat(auto-fit, minmax(200px, 1fr));' // new_line('a') // &
              '            gap: 1rem; margin-top: 1rem;' // new_line('a') // &
              '        }' // new_line('a') // &
              '        .summary-item {' // new_line('a') // &
              '            background: rgba(0, 255, 255, 0.1); ' // &
              'padding: 1rem; border-radius: 6px;' // new_line('a') // &
              '            border-left: 4px solid var(--accent-color, #ff00ff);' // &
              new_line('a') // &
              '        }' // new_line('a') // &
              '        .summary-item.highlight {' // new_line('a') // &
              '            background: rgba(255, 0, 255, 0.2);' // new_line('a') // &
              '            border-left: 4px solid var(--secondary-color, ' // &
              '#00ffff);' // new_line('a') // &
              '        }' // new_line('a') // &
              '        .summary-label { display: block; font-weight: bold; ' // &
              'margin-bottom: 0.5rem; }' // new_line('a') // &
              '        .summary-value { font-size: 1.5rem; ' // &
              'color: var(--accent-color, #ff00ff); }' // new_line('a') // &
              '        .coverage-percent { font-weight: bold; ' // &
              'text-shadow: 0 0 10px currentColor; }' // new_line('a') // &
              '        .file-list { margin-top: 2rem; }' // new_line('a') // &
              '        .file-item {' // new_line('a') // &
              '            background: rgba(255, 255, 255, 0.05); ' // &
              'margin: 0.5rem 0; padding: 1rem;' // new_line('a') // &
              '            border-radius: 6px; border-left: 3px solid ' // &
              'var(--secondary-color, #00ffff);' // new_line('a') // &
              '        }' // new_line('a') // &
              '        .file-name { font-weight: bold; ' // &
              'color: var(--accent-color, #ff00ff); }' // new_line('a') // &
              '        .file-coverage { float: right; ' // &
              'color: var(--secondary-color, #00ffff); }' // new_line('a') // &
              '        .keyword { color: var(--accent-color, #ff00ff); ' // &
              'font-weight: bold; }' // new_line('a') // &
              '        .comment { color: #888; font-style: italic; }'
    end function generate_component_css_styles
    
    ! Generate responsive CSS styles (media queries)
    function generate_responsive_css_styles() result(css)
        character(len=:), allocatable :: css
        
        css = '        @media (max-width: 768px) {' // new_line('a') // &
              '            body { padding: 10px; }' // new_line('a') // &
              '            .report-header h1 { font-size: 2rem; }' // new_line('a') // &
              '            .summary-grid { grid-template-columns: 1fr; }' // &
              new_line('a') // &
              '        }'
    end function generate_responsive_css_styles
    
    ! Generate HTML header section
    function generate_html_header(css_vars) result(header)
        character(len=*), intent(in) :: css_vars
        character(len=:), allocatable :: header
        character(len=:), allocatable :: css_styles
        
        css_styles = generate_comprehensive_css(css_vars)
        
        header = '<head>' // new_line('a') // &
                 '    <meta charset="UTF-8">' // new_line('a') // &
                 '    <meta name="viewport" content="width=device-width, ' // &
                 'initial-scale=1.0">' // new_line('a') // &
                 '    <title>Fortcov Coverage Report</title>' // new_line('a') // &
                 '    <meta name="description" content="Code coverage report ' // &
                 'generated by Fortcov">' // new_line('a') // &
                 css_styles // new_line('a') // &
                 '</head>'
    end function generate_html_header
    
    ! Generate HTML body section
    function generate_html_body(data, theme) result(body)
        type(transformed_data_t), intent(in) :: data
        type(color_scheme_t), intent(in) :: theme
        character(len=:), allocatable :: body
        character(len=:), allocatable :: file_details
        
        file_details = generate_file_coverage_details(data)
        
        body = '    <header class="report-header">' // new_line('a') // &
               '        <h1>Coverage Report</h1>' // new_line('a') // &
               '        <div class="theme-info">Theme: ' // theme%name // &
               '</div>' // new_line('a') // &
               '    </header>' // new_line('a') // &
               '    <main class="report-main">' // new_line('a') // &
               '        <section class="summary-section cyberpunk">' // &
               new_line('a') // &
               '            <h2>Coverage Summary</h2>' // new_line('a') // &
               '            <div class="summary-grid">' // new_line('a') // &
               '                <div class="summary-item">' // new_line('a') // &
               '                    <span class="summary-label">Total Files:' // &
               '</span>' // new_line('a') // &
               '                    <span class="summary-value">' // &
               int_to_str(data%summary%total_files) // '</span>' // new_line('a') // &
               '                </div>' // new_line('a') // &
               '                <div class="summary-item">' // new_line('a') // &
               '                    <span class="summary-label">Total Lines:' // &
               '</span>' // new_line('a') // &
               '                    <span class="summary-value">' // &
               int_to_str(data%summary%total_lines) // '</span>' // new_line('a') // &
               '                </div>' // new_line('a') // &
               '                <div class="summary-item">' // new_line('a') // &
               '                    <span class="summary-label">Covered Lines:' // &
               '</span>' // new_line('a') // &
               '                    <span class="summary-value">' // &
               int_to_str(data%summary%covered_lines) // '</span>' // &
               new_line('a') // &
               '                </div>' // new_line('a') // &
               '                <div class="summary-item highlight">' // &
               new_line('a') // &
               '                    <span class="summary-label">Coverage:' // &
               '</span>' // new_line('a') // &
               '                    <span class="summary-value coverage-percent">' // &
               real_to_str(data%summary%coverage_percentage) // '%</span>' // &
               new_line('a') // &
               '                </div>' // new_line('a') // &
               '            </div>' // new_line('a') // &
               '        </section>' // new_line('a') // &
               file_details // new_line('a') // &
               '    </main>'
    end function generate_html_body
    
    ! Generate HTML footer section
    function generate_html_footer() result(footer)
        character(len=:), allocatable :: footer
        
        footer = '    <footer class="report-footer">' // new_line('a') // &
                 '        <p>Generated by <strong>Fortcov</strong> - ' // &
                 'Fortran Coverage Analysis Tool</p>' // new_line('a') // &
                 '        <p class="timestamp">Report generated at: ' // &
                 get_current_timestamp() // '</p>' // new_line('a') // &
                 '    </footer>'
    end function generate_html_footer

end module html_report_generator